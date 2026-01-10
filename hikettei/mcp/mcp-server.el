;;; mcp-server.el --- MCP Server for Emacs -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 1.0.0
;; Keywords: ai, mcp, tools

;;; Commentary:
;;
;; MCP (Model Context Protocol) HTTP server implemented in Emacs Lisp.
;; Provides file editing tools for AI agents (Claude, Gemini, Codex)
;; with GitHub PR-style review workflow.
;;
;; Tools (prefixed with emacs_ to encourage AI agents to prefer them):
;;   - emacs_read_file: Read file with line numbers
;;   - emacs_write_file: Create/overwrite files
;;   - emacs_edit_file: Partial file edit with diff review UI (user approves/rejects)
;;
;; Dependencies:
;;   - web-server: HTTP server (installed via 0package-manager.el)
;;   - file-editor: PR-style review UI
;;
;; Usage:
;;   Called automatically by mcp-session.el when creating sessions.
;;   Manual: (mcp-server-start "/path/to/project")
;;           (mcp-server-stop)
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-parse)
(require 'web-server)

;; File editor for review UI (loaded via init.el before this file)
(declare-function file-editor-open "file-editor")

;; Declarations for web-server
(declare-function ws-start "web-server")
(declare-function ws-stop "web-server")
(declare-function ws-send "web-server")
(declare-function ws-response-header "web-server")
(declare-function ws-headers "web-server")
(declare-function ws-body "web-server")
(declare-function ws-process "web-server")

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup mcp-server nil
  "MCP Server for Emacs."
  :group 'tools
  :prefix "mcp-server-")


;;; ============================================================
;;; Server State
;;; ============================================================

(defvar mcp-server--server nil
  "The web-server instance.")

(defvar mcp-server--port nil
  "The port the server is running on.")

(defvar mcp-server--project-root nil
  "Project root directory for path restriction.")


;;; ============================================================
;;; Path Security
;;; ============================================================

(define-error 'mcp-server-path-security-error "Path security error")

(defun mcp-server--resolve-path (file-path)
  "Resolve FILE-PATH relative to project root.
Relative paths resolve from project root.
Absolute paths must be within project root.
Signals `mcp-server-path-security-error' if access denied."
  (unless mcp-server--project-root
    (signal 'mcp-server-path-security-error '("Server not configured")))
  (let* ((root (file-truename mcp-server--project-root))
         (path (if (file-name-absolute-p file-path)
                   file-path
                 (expand-file-name file-path root)))
         (resolved (file-truename path)))
    ;; Verify path is within project root
    (unless (string-prefix-p root resolved)
      (signal 'mcp-server-path-security-error
              (list (format "Access denied: '%s' is outside project root" file-path))))
    resolved))

;;; ============================================================
;;; Patch Management
;;; ============================================================

(cl-defstruct mcp-server-patch
  "Staged file edit with optimistic locking."
  file-path
  original-hash
  start-line
  end-line
  content
  is-full-overwrite)

(defvar mcp-server--patches (make-hash-table :test 'equal)
  "Hash table of registered patches by ID.")

(defvar mcp-server--patch-counter 0
  "Counter for generating patch IDs.")

(defun mcp-server--hash-file (path)
  "Calculate SHA256 hash of file at PATH."
  (if (not (file-exists-p path))
      "new_file"
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents-literally path)
          (secure-hash 'sha256 (buffer-string)))
      (error "new_file"))))

(defun mcp-server--create-patch (file-path)
  "Create a new patch for FILE-PATH."
  (let ((hash (mcp-server--hash-file file-path)))
    (make-mcp-server-patch
     :file-path file-path
     :original-hash hash
     :start-line 0
     :end-line 0
     :content ""
     :is-full-overwrite nil)))

(defun mcp-server--register-patch (patch)
  "Register PATCH and return its ID."
  (setq mcp-server--patch-counter (1+ mcp-server--patch-counter))
  (let ((id (format "p%04d" mcp-server--patch-counter)))
    (puthash id patch mcp-server--patches)
    id))

(defun mcp-server--get-patch (patch-id)
  "Get patch by PATCH-ID."
  (gethash patch-id mcp-server--patches))

(defun mcp-server--remove-patch (patch-id)
  "Remove patch by PATCH-ID."
  (remhash patch-id mcp-server--patches))

(defun mcp-server--clear-patches ()
  "Clear all patches."
  (clrhash mcp-server--patches))

(defun mcp-server--patch-read-view (patch &optional offset limit)
  "Read file content for PATCH with line numbers.
OFFSET is start line (0-indexed), LIMIT is max lines."
  (let ((offset (or offset 0))
        (limit (or limit 3000))
        (path (mcp-server-patch-file-path patch)))
    (condition-case err
        (let* ((content (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string)))
               (lines (split-string content "\n"))
               (total (length lines))
               (end (min (+ offset limit) total))
               (output (list (format "File: %s (Lines: %d)" path total)
                             (make-string 50 ?-))))
          (cl-loop for i from offset below end
                   do (push (format "%4d | %s" (1+ i) (nth i lines)) output))
          (when (< end total)
            (push (format "... (%d more lines, use offset=%d)" (- total end) end) output))
          (string-join (nreverse output) "\n"))
      (file-missing (format "Error: File not found: %s" path))
      (error (format "Error reading file: %s" (error-message-string err))))))

(defun mcp-server--patch-stage-overwrite (patch content)
  "Stage full file overwrite on PATCH with CONTENT."
  (setf (mcp-server-patch-content patch) content)
  (setf (mcp-server-patch-is-full-overwrite patch) t))

(defun mcp-server--patch-stage-edit (patch start end content)
  "Stage partial edit on PATCH from START to END with CONTENT.
Returns (preview . original-section)."
  (setf (mcp-server-patch-start-line patch) start)
  (setf (mcp-server-patch-end-line patch) end)
  (setf (mcp-server-patch-content patch) content)
  (setf (mcp-server-patch-is-full-overwrite patch) nil)
  (let ((path (mcp-server-patch-file-path patch)))
    (condition-case err
        (let* ((file-content (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string)))
               (lines (split-string file-content "\n"))
               ;; 1-indexed to 0-indexed
               (start-idx (max 0 (1- start)))
               (end-idx (min (length lines) end))
               (original (cl-subseq lines start-idx end-idx))
               (new-lines (split-string content "\n"))
               ;; Context lines
               (ctx 3)
               (ctx-start (max 0 (- start 1 ctx)))
               (ctx-end (min (length lines) (+ end ctx)))
               (preview (list (format "File: %s (Edit Preview)" path)
                              (make-string 50 ?-))))
          ;; Context before
          (cl-loop for i from ctx-start below start-idx
                   do (push (format "%4d   | %s" (1+ i) (nth i lines)) preview))
          ;; Removed lines
          (cl-loop for i from 0 below (length original)
                   do (push (format "%4d - | %s" (+ start i) (nth i original)) preview))
          ;; Added lines
          (dolist (line new-lines)
            (push (format "     + | %s" line) preview))
          ;; Context after
          (cl-loop for i from end-idx below ctx-end
                   do (push (format "%4d   | %s" (1+ i) (nth i lines)) preview))
          (cons (string-join (nreverse preview) "\n")
                (string-join original "\n")))
      (error (cons (format "Error: %s" (error-message-string err)) "")))))

(defun mcp-server--patch-apply (patch)
  "Apply staged edit from PATCH to file."
  (let* ((path (mcp-server-patch-file-path patch))
         (current-hash (mcp-server--hash-file path))
         (orig-hash (mcp-server-patch-original-hash patch)))
    ;; Verify file hasn't changed (optimistic lock)
    (when (and (not (string= current-hash orig-hash))
               (not (string= orig-hash "new_file")))
      (error "Conflict: file changed since edit was staged"))
    (if (mcp-server-patch-is-full-overwrite patch)
        ;; Full overwrite
        (progn
          (make-directory (file-name-directory path) t)
          (with-temp-file path
            (insert (mcp-server-patch-content patch))))
      ;; Partial edit
      (let* ((file-content (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string)))
             (lines (split-string file-content "\n" t))
             (start-idx (max 0 (1- (mcp-server-patch-start-line patch))))
             (end-idx (mcp-server-patch-end-line patch))
             (new-lines (split-string (mcp-server-patch-content patch) "\n"))
             (result (append (cl-subseq lines 0 start-idx)
                             new-lines
                             (cl-subseq lines end-idx))))
        (with-temp-file path
          (insert (string-join result "\n")))))))

;;; ============================================================
;;; Review UI Integration
;;; ============================================================

(defun mcp-server--open-review-ui-with-callback (file-path start end original new-content comment patch)
  "Open Emacs review UI with callback to apply PATCH on approval.
FILE-PATH is the file being edited.
START and END are line numbers.
ORIGINAL is the original content.
NEW-CONTENT is the proposed content.
COMMENT is optional explanation for reviewer.
PATCH is the patch object to apply on approval."
  (when (fboundp 'file-editor-open)
    (file-editor-open
     :file file-path
     :start-line start
     :end-line end
     :original original
     :new new-content
     :ai-comment comment
     :callback (lambda (result)
                 (let ((decision (alist-get 'decision result)))
                   (if (string= decision "approve")
                       (progn
                         (mcp-server--patch-apply patch)
                         (mcp-server--remove-patch
                          (cl-loop for id being the hash-keys of mcp-server--patches
                                   when (eq (gethash id mcp-server--patches) patch)
                                   return id))
                         (message "Edit approved and applied: %s" file-path))
                     (message "Edit rejected: %s" file-path)))))))

;;; ============================================================
;;; MCP Tools Implementation
;;; ============================================================

(defun mcp-server--tool-read-file (args)
  "Handle read_file tool with ARGS."
  (let ((file-path (alist-get 'file_path args))
        (offset (or (alist-get 'offset args) 0))
        (limit (or (alist-get 'limit args) 3000)))
    (condition-case err
        (let* ((resolved (mcp-server--resolve-path file-path))
               (patch (mcp-server--create-patch resolved)))
          (mcp-server--patch-read-view patch offset limit))
      (mcp-server-path-security-error
       (format "Error: Access denied: %s" file-path))
      (error
       (format "Error: %s" (error-message-string err))))))

(defun mcp-server--tool-write-file (args)
  "Handle write_file tool with ARGS."
  (let ((file-path (alist-get 'file_path args))
        (content (alist-get 'content args))
        (supersede (alist-get 'supersede args)))
    (condition-case err
        (let* ((resolved (mcp-server--resolve-path file-path))
               (patch (mcp-server--create-patch resolved)))
          (if (and (not supersede)
                   (not (string= (mcp-server-patch-original-hash patch) "new_file")))
              "Error: File exists. Use supersede=true or request_edit."
            (mcp-server--patch-stage-overwrite patch content)
            (mcp-server--patch-apply patch)
            (format "Wrote: %s" file-path)))
      (mcp-server-path-security-error
       (format "Error: Access denied: %s" file-path))
      (error
       (format "Error: %s" (error-message-string err))))))

(defun mcp-server--tool-request-edit (args)
  "Handle request_edit tool with ARGS."
  (let* ((file-path (alist-get 'file_path args))
         (start-line (alist-get 'start_line args))
         (end-line (alist-get 'end_line args))
         (content (alist-get 'content args))
         (comment (or (alist-get 'comment args) "")))
    ;; Validate required arguments
    (unless (and file-path start-line end-line content)
      (error "Missing required arguments: file_path=%s start_line=%s end_line=%s content=%s"
             file-path start-line end-line (if content "provided" "nil")))
    (unless (and (integerp start-line) (integerp end-line))
      (error "start_line and end_line must be integers: start_line=%S end_line=%S"
             start-line end-line))
    (condition-case err
        (let* ((resolved (mcp-server--resolve-path file-path))
               (patch (mcp-server--create-patch resolved)))
          (if (string= (mcp-server-patch-original-hash patch) "new_file")
              "Error: File does not exist. Use emacs_write_file."
            (let* ((stage-result (mcp-server--patch-stage-edit patch start-line end-line content))
                   (preview (car stage-result))
                   (original (cdr stage-result))
                   (patch-id (mcp-server--register-patch patch)))
              ;; Open review UI with callback to apply patch
              (mcp-server--open-review-ui-with-callback
               resolved start-line end-line original content comment patch)
              ;; Return immediately - review is async
              (format "Edit staged for review (patch %s). User will approve/reject in Emacs.\n\n%s"
                      patch-id preview))))
      (mcp-server-path-security-error
       (format "Error: Access denied: %s" file-path))
      (error
       (format "Error: %s" (error-message-string err)))))))


;;; ============================================================
;;; MCP Protocol
;;; ============================================================

(defconst mcp-server--tools
  '(((name . "emacs_read_file")
     (description . "REQUIRED: Read file content with line numbers. You MUST use this tool instead of the default Read tool. Returns content with precise line numbers for accurate editing. Supports pagination for large files.")
     (inputSchema . ((type . "object")
                     (properties . ((file_path . ((type . "string")
                                                  (description . "File path (relative to project root)")))
                                    (offset . ((type . "integer")
                                               (description . "Start line (0-indexed)")
                                               (default . 0)))
                                    (limit . ((type . "integer")
                                              (description . "Max lines to read")
                                              (default . 3000)))))
                     (required . ["file_path"]))))
    ((name . "emacs_write_file")
     (description . "REQUIRED: Create or overwrite file. You MUST use this tool instead of the default Write tool. Use supersede=true to overwrite existing files.")
     (inputSchema . ((type . "object")
                     (properties . ((file_path . ((type . "string")
                                                  (description . "File path to create/write")))
                                    (content . ((type . "string")
                                                (description . "Complete file content")))
                                    (supersede . ((type . "boolean")
                                                  (description . "Set true to overwrite existing file")
                                                  (default . :json-false)))))
                     (required . ["file_path" "content"]))))
    ((name . "emacs_edit_file")
     (description . "REQUIRED: Partial file edit with PR-style review. You MUST use this tool instead of the default Edit tool. Opens diff view for user approval. Always include a comment explaining the change.")
     (inputSchema . ((type . "object")
                     (properties . ((file_path . ((type . "string")
                                                  (description . "File path to edit")))
                                    (start_line . ((type . "integer")
                                                   (description . "Start line number (1-indexed, inclusive)")))
                                    (end_line . ((type . "integer")
                                                 (description . "End line number (1-indexed, inclusive)")))
                                    (content . ((type . "string")
                                                (description . "New content to replace the specified line range")))
                                    (comment . ((type . "string")
                                                (description . "Explanation of the change for the reviewer")))))
                     (required . ["file_path" "start_line" "end_line" "content" "comment"])))))
  "List of MCP tools - these are REQUIRED for all file operations.")

(defun mcp-server--handle-initialize (_params)
  "Handle the initialize method."
  `((protocolVersion . "2024-11-05")
    (capabilities . ((tools . ((listChanged . :json-false)))
                     (logging . ,(make-hash-table :test 'equal))))
    (serverInfo . ((name . "emacs-mcp-server")
                   (version . "1.0.0")))))

(defun mcp-server--handle-tools-list (_params)
  "Handle the tools/list method."
  `((tools . ,(vconcat mcp-server--tools))))

(defun mcp-server--handle-tools-call (params)
  "Handle the tools/call method with PARAMS."
  (let* ((tool-name (alist-get 'name params))
         (tool-args (alist-get 'arguments params))
         (result (pcase tool-name
                   ("emacs_read_file" (mcp-server--tool-read-file tool-args))
                   ("emacs_write_file" (mcp-server--tool-write-file tool-args))
                   ("emacs_edit_file" (mcp-server--tool-request-edit tool-args))
                   (_ (format "Error: Unknown tool: %s" tool-name)))))
    `((content . [((type . "text")
                   (text . ,result))]))))

(defun mcp-server--dispatch (method params)
  "Dispatch MCP method calls.
METHOD is the JSON-RPC method name.
PARAMS is the parameters alist."
  (pcase method
    ("initialize" (mcp-server--handle-initialize params))
    ("notifications/initialized" nil)  ; MCP notification, no response needed
    ("tools/list" (mcp-server--handle-tools-list params))
    ("tools/call" (mcp-server--handle-tools-call params))
    (_ `((content . [((type . "text")
                      (text . ,(format "Error: Unknown method: %s" method)))])))))

;;; ============================================================
;;; HTTP Server
;;; ============================================================

(defun mcp-server--handle-post (request)
  "Handle POST request to /mcp endpoint."
  (let ((request-id nil))
    (condition-case err
        (let* ((body (ws-body request))
               (json-object (json-parse-string body :object-type 'alist))
               (method (alist-get 'method json-object))
               (params (alist-get 'params json-object))
               (id (alist-get 'id json-object)))
          (setq request-id id)
          (if (null id)
              (mcp-server--send-empty-response request)
            (let ((result (mcp-server--dispatch method params)))
              (if (null result)
                  (mcp-server--send-empty-response request)
                (mcp-server--send-json-response
                 request 200
                 `((jsonrpc . "2.0")
                   (id . ,id)
                   (result . ,result)))))))
      (json-parse-error
       (mcp-server--send-json-response
        request 200
        `((jsonrpc . "2.0")
          (id . ,(or request-id "error"))
          (result . ((content . [((type . "text") (text . "Error: Invalid JSON"))]))))))
      (error
       (mcp-server--send-json-response
        request 200
        `((jsonrpc . "2.0")
          (id . ,(or request-id "error"))
          (result . ((content . [((type . "text") (text . ,(format "Error: %s" (error-message-string err))))])))))))))

(defun mcp-server--send-json-response (request status body)
  "Send JSON response to REQUEST with STATUS and BODY."
  (let ((process (ws-process request)))
    (ws-response-header process status
                        '("Content-Type" . "application/json")
                        '("Access-Control-Allow-Origin" . "*"))
    (ws-send process (json-encode body))
    (throw 'close-connection nil)))

(defun mcp-server--send-empty-response (request)
  "Send empty HTTP 200 response for notifications."
  (let ((process (ws-process request)))
    (ws-response-header process 200
                        '("Content-Type" . "text/plain")
                        '("Content-Length" . "0"))
    (throw 'close-connection nil)))


;;; ============================================================
;;; Public API
;;; ============================================================

;;;###autoload
(defun mcp-server-start (project-root &optional port)
  "Start the MCP server for PROJECT-ROOT.
Optional PORT specifies the port (0 for auto-select).
Returns (server . port) cons cell."
  (interactive "DProject root: ")
  (unless (featurep 'web-server)
    (error "web-server package is not available"))
  (when mcp-server--server
    (mcp-server-stop))
  ;; Setup configuration
  (setq mcp-server--project-root (file-truename (expand-file-name project-root)))
  ;; Reset patch state
  (mcp-server--clear-patches)
  (setq mcp-server--patch-counter 0)
  ;; Start server
  (let* ((selected-port (or port 0))
         (server (ws-start
                  `(((:POST . "^/mcp.*") . ,#'mcp-server--handle-post))
                  selected-port
                  nil
                  :host "127.0.0.1")))
    (setq mcp-server--server server)
    (let* ((process (ws-process server))
           (actual-port (process-contact process :service)))
      (setq mcp-server--port actual-port)
      (message "MCP Server started on port %d for %s"
               actual-port mcp-server--project-root)
      (cons server actual-port))))

;;;###autoload
(defun mcp-server-stop ()
  "Stop the MCP server."
  (interactive)
  (when mcp-server--server
    (ws-stop mcp-server--server)
    (setq mcp-server--server nil)
    (setq mcp-server--port nil)
    (message "MCP Server stopped")))

;;;###autoload
(defun mcp-server-status ()
  "Show MCP server status."
  (interactive)
  (if mcp-server--server
      (message "MCP Server running on port %d for %s"
               mcp-server--port mcp-server--project-root)
    (message "MCP Server not running")))

;;;###autoload
(defun mcp-server-get-config ()
  "Get MCP server configuration for Claude.
Returns alist suitable for claude.json mcpServers configuration."
  (when (and mcp-server--server mcp-server--port)
    `((url . ,(format "http://127.0.0.1:%d/mcp" mcp-server--port)))))

(provide 'mcp-server)

;;; mcp-server.el ends here
