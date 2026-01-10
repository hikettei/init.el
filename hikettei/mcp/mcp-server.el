;;; mcp-server.el --- MCP Server for Emacs -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 1.1.0
;; Keywords: ai, mcp, tools

;;; Commentary:
;;
;; MCP (Model Context Protocol) HTTP server implemented in Emacs Lisp.
;; Provides file editing tools for AI agents (Claude, Gemini, Codex)
;; with GitHub PR-style review workflow.
;;
;; Tools:
;;   - emacs_read_file: Read file with line numbers
;;   - emacs_write_file: Create/overwrite files
;;   - emacs_edit_file: Partial file edit with diff review UI
;;
;; The emacs_edit_file tool blocks until user approves/rejects the edit.
;; The response contains the review result (approved/rejected with comments).
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-parse)
(require 'web-server)

(declare-function file-editor-open "file-editor")
(declare-function ws-start "web-server")
(declare-function ws-stop "web-server")
(declare-function ws-send "web-server")
(declare-function ws-response-header "web-server")
(declare-function ws-body "web-server")
(declare-function ws-process "web-server")

;;; ============================================================
;;; Server State
;;; ============================================================

(defvar mcp-server--server nil "The web-server instance.")
(defvar mcp-server--port nil "The port the server is running on.")
(defvar mcp-server--project-root nil "Project root directory.")

;;; ============================================================
;;; Review State (for blocking wait)
;;; ============================================================

(defvar mcp-server--review-result nil
  "Result from file-editor review. Set by callback.")

(defvar mcp-server--review-complete nil
  "Flag indicating review is complete.")

;;; ============================================================
;;; Path Security
;;; ============================================================

(define-error 'mcp-path-error "Path security error")

(defun mcp-server--resolve-path (file-path)
  "Resolve FILE-PATH within project root. Signals error if outside."
  (unless mcp-server--project-root
    (signal 'mcp-path-error '("Server not configured")))
  (let* ((root (file-truename mcp-server--project-root))
         (path (expand-file-name file-path root))
         (resolved (file-truename path)))
    (unless (string-prefix-p root resolved)
      (signal 'mcp-path-error
              (list (format "Access denied: %s" file-path))))
    resolved))

;;; ============================================================
;;; File Operations
;;; ============================================================

(defun mcp-server--read-file (path &optional offset limit)
  "Read file at PATH with line numbers. OFFSET and LIMIT for pagination."
  (let ((offset (or offset 0))
        (limit (or limit 3000)))
    (condition-case nil
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
            (push (format "... (%d more lines)" (- total end)) output))
          (string-join (nreverse output) "\n"))
      (file-missing (format "Error: File not found: %s" path)))))

(defun mcp-server--write-file (path content)
  "Write CONTENT to file at PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert content))
  (format "Wrote: %s" path))

(defun mcp-server--apply-edit (path start-line end-line new-content)
  "Apply edit to PATH replacing lines START-LINE to END-LINE with NEW-CONTENT."
  (let* ((file-content (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string)))
         (lines (split-string file-content "\n" nil))
         (start-idx (max 0 (1- start-line)))
         (new-lines (split-string new-content "\n" nil))
         (result (append (cl-subseq lines 0 start-idx)
                         new-lines
                         (cl-subseq lines end-line))))
    (with-temp-file path
      (insert (string-join result "\n")))))

(defun mcp-server--get-original-content (path start-line end-line)
  "Get original content from PATH between START-LINE and END-LINE."
  (let* ((content (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))
         (lines (split-string content "\n" nil))
         (start-idx (max 0 (1- start-line)))
         (end-idx (min (length lines) end-line)))
    (string-join (cl-subseq lines start-idx end-idx) "\n")))

;;; ============================================================
;;; Review UI (Blocking)
;;; ============================================================

(defun mcp-server--format-review-result (result file-path)
  "Format review RESULT for FILE-PATH as response text."
  (let* ((decision (alist-get 'decision result))
         (approved (string= decision "approve"))
         (feedback (or (alist-get 'feedback result) ""))
         (comments (alist-get 'line_comments result))
         (comment-str (if comments
                         (concat "\n\nLine comments:\n"
                                 (mapconcat (lambda (c)
                                              (format "  Line %s: %s"
                                                      (alist-get 'line c)
                                                      (alist-get 'comment c)))
                                            comments "\n"))
                       "")))
    (if approved
        (format "Edit approved and applied: %s%s%s"
                file-path
                (if (string-empty-p feedback) "" (format "\nFeedback: %s" feedback))
                comment-str)
      (format "Edit rejected: %s\nReason: %s%s"
              file-path
              (if (string-empty-p feedback) "(no reason given)" feedback)
              comment-str))))

(defun mcp-server--open-review-and-wait (file-path start end original new-content comment)
  "Open review UI and block until user decides.
Returns formatted response string."
  (setq mcp-server--review-result nil
        mcp-server--review-complete nil)
  (file-editor-open
   :file file-path
   :start-line start
   :end-line end
   :original original
   :new new-content
   :ai-comment comment
   :callback (lambda (result)
               (setq mcp-server--review-result result
                     mcp-server--review-complete t)
               (exit-recursive-edit)))
  ;; Block with recursive-edit (allows Emacs event loop to run)
  (condition-case nil
      (recursive-edit)
    (quit
     ;; User aborted with C-g, treat as rejection
     (setq mcp-server--review-result
           `((decision . "request-changes")
             (summary . "Review aborted by user")))))
  ;; Process result
  (let* ((result mcp-server--review-result)
         (decision (alist-get 'decision result))
         (approved (string= decision "approve")))
    (when approved
      (condition-case err
          (mcp-server--apply-edit file-path start end new-content)
        (error
         (setq mcp-server--review-result
               `((decision . "error")
                 (summary . ,(format "Apply failed: %s"
                                     (error-message-string err))))))))
    (mcp-server--format-review-result mcp-server--review-result file-path)))

;;; ============================================================
;;; MCP Tools
;;; ============================================================

(declare-function activity-panel-add-read-highlight "activity-panel")
(declare-function activity-panel-active-p "activity-panel")
(declare-function activity-panel-set-status "activity-panel")
(declare-function activity-panel-clear-status "activity-panel")

(defun mcp-server--tool-read-file (args)
  "Handle emacs_read_file tool with Activity Panel integration."
  (condition-case err
      (let ((path (mcp-server--resolve-path
                   (alist-get "file_path" args nil nil #'string=)))
            (offset (or (alist-get "offset" args nil nil #'string=) 0))
            (limit (or (alist-get "limit" args nil nil #'string=) 3000)))
        ;; Show reading status
        (when (and (fboundp 'activity-panel-active-p)
                   (activity-panel-active-p)
                   (fboundp 'activity-panel-set-status))
          (activity-panel-set-status
           (format "Reading %s ..." (file-name-nondirectory path)) 3))
        ;; Notify Activity Panel of read operation (if active)
        (when (and (fboundp 'activity-panel-active-p)
                   (fboundp 'activity-panel-add-read-highlight)
                   (activity-panel-active-p))
          (activity-panel-add-read-highlight path (1+ offset) (+ offset limit)))
        ;; Perform the actual read
        (mcp-server--read-file path offset limit))
    (mcp-path-error (format "Error: %s" (cadr err)))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-server--tool-write-file (args)
  "Handle emacs_write_file tool."
  (condition-case err
      (let* ((path (mcp-server--resolve-path
                    (alist-get "file_path" args nil nil #'string=)))
             (content (alist-get "content" args nil nil #'string=))
             (supersede (alist-get "supersede" args nil nil #'string=))
             (exists (file-exists-p path)))
        (if (and exists (not supersede))
            "Error: File exists. Use supersede=true to overwrite."
          (mcp-server--write-file path content)))
    (mcp-path-error (format "Error: %s" (cadr err)))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-server--tool-edit-file (args)
  "Handle emacs_edit_file tool. Blocks until user review completes."
  (condition-case err
      (let* ((path (mcp-server--resolve-path
                    (alist-get "file_path" args nil nil #'string=)))
             (start (alist-get "start_line" args nil nil #'string=))
             (end (alist-get "end_line" args nil nil #'string=))
             (content (alist-get "content" args nil nil #'string=))
             (comment (or (alist-get "comment" args nil nil #'string=) "")))
        (unless (file-exists-p path)
          (error "File does not exist. Use emacs_write_file"))
        (unless (and (integerp start) (integerp end))
          (error "start_line and end_line must be integers"))
        ;; Show requesting change status
        (when (and (fboundp 'activity-panel-active-p)
                   (activity-panel-active-p)
                   (fboundp 'activity-panel-set-status))
          (activity-panel-set-status
           (format "Requesting change: %s ..." (file-name-nondirectory path))))
        (let ((original (mcp-server--get-original-content path start end)))
          (prog1
              (mcp-server--open-review-and-wait path start end original content comment)
            ;; Clear status after review completes
            (when (fboundp 'activity-panel-clear-status)
              (activity-panel-clear-status)))))
    (mcp-path-error (format "Error: %s" (cadr err)))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-server--tool-eval (args)
  "Handle emacs_eval tool."
  (condition-case err
      (let ((code (alist-get "code" args nil nil #'string=)))
        (format "%S" (eval (read code) t)))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-server--tool-screenshot (_args)
  "Take screenshot and save to workspace/.hikettei/screen_shots/"
  (let* ((workspace (or mcp-server--project-root default-directory))
         (shots-dir (expand-file-name ".hikettei/screen_shots" workspace))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "screenshot-%s.png" timestamp))
         (filepath (expand-file-name filename shots-dir)))
    ;; Ensure directory exists
    (make-directory shots-dir t)
    ;; Cleanup old screenshots (keep last 10)
    (mcp-server--cleanup-screenshots shots-dir 10)
    ;; Capture frame to PNG (use appropriate function for platform)
    (let ((data (cond
                 ((fboundp 'mac-export-frames) (mac-export-frames nil 'png))
                 ((fboundp 'x-export-frames) (x-export-frames nil 'png))
                 (t (error "Screenshot not supported on this platform")))))
      (with-temp-file filepath
        (set-buffer-multibyte nil)
        (insert data)))
    (format "Screenshot saved: %s\nUse Read tool to view it." filepath)))

(defun mcp-server--cleanup-screenshots (dir keep-count)
  "Delete old screenshots in DIR, keeping only KEEP-COUNT most recent."
  (when (file-directory-p dir)
    (let* ((files (directory-files dir t "\\.png$"))
           (sorted (sort files (lambda (a b)
                                (time-less-p (nth 5 (file-attributes b))
                                            (nth 5 (file-attributes a))))))
           (to-delete (nthcdr keep-count sorted)))
      (dolist (f to-delete)
        (delete-file f)))))

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
                     (required . ["file_path" "start_line" "end_line" "content" "comment"]))))
    ((name . "emacs_eval")
     (description . "Evaluate Emacs Lisp code and return the result. Useful for debugging, checking server state, or running arbitrary elisp.")
     (inputSchema . ((type . "object")
                     (properties . ((code . ((type . "string")
                                             (description . "Emacs Lisp code to evaluate")))))
                     (required . ["code"]))))
    ((name . "emacs_screenshot")
     (description . "Capture current Emacs frame as PNG screenshot. Saves to workspace/.hikettei/screen_shots/. Returns path - use Read tool to view the image.")
     (inputSchema . ((type . "object")
                     (properties . ())
                     (required . []))))))

(defun mcp-server--handle-initialize (_params)
  "Handle initialize method."
  `((protocolVersion . "2024-11-05")
    (capabilities . ((tools . ((listChanged . :json-false)))
                     (logging . ,(make-hash-table :test 'equal))))
    (serverInfo . ((name . "emacs-mcp-server") (version . "1.1.0")))))

(defun mcp-server--handle-tools-call (params)
  "Handle tools/call method."
  (let* ((name (alist-get "name" params nil nil #'string=))
         (args (alist-get "arguments" params nil nil #'string=))
         (result (pcase name
                   ("emacs_read_file" (mcp-server--tool-read-file args))
                   ("emacs_write_file" (mcp-server--tool-write-file args))
                   ("emacs_edit_file" (mcp-server--tool-edit-file args))
                   ("emacs_eval" (mcp-server--tool-eval args))
                   ("emacs_screenshot" (mcp-server--tool-screenshot args))
                   (_ (format "Error: Unknown tool: %s" name)))))
    `((content . [((type . "text") (text . ,result))]))))

(defun mcp-server--dispatch (method params)
  "Dispatch MCP method."
  (pcase method
    ("initialize" (mcp-server--handle-initialize params))
    ("notifications/initialized" nil)
    ("tools/list" `((tools . ,(vconcat mcp-server--tools))))
    ("tools/call" (mcp-server--handle-tools-call params))
    (_ `((content . [((type . "text")
                      (text . ,(format "Error: Unknown method: %s" method)))])))))

;;; ============================================================
;;; HTTP Server
;;; ============================================================

(defun mcp-server--handle-reload (request)
  "Handle GET /reload."
  (let ((process (ws-process request)))
    (condition-case err
        (progn
          (load-file (expand-file-name "hikettei/mcp/mcp-server.el" user-emacs-directory))
          (ws-response-header process 200 '("Content-Type" . "text/plain"))
          (ws-send process "OK: reloaded"))
      (error
       (ws-response-header process 500 '("Content-Type" . "text/plain"))
       (ws-send process (format "Error: %s" (error-message-string err)))))
    (throw 'close-connection nil)))

(defun mcp-server--handle-post (request)
  "Handle POST /mcp."
  (let ((process (ws-process request))
        (id nil))
    (condition-case err
        (let* ((body (ws-body request))
               (json (json-parse-string body :object-type 'alist))
               (method (alist-get "method" json nil nil #'string=))
               (params (alist-get "params" json nil nil #'string=)))
          (setq id (alist-get "id" json nil nil #'string=))
          (if (null id)
              ;; Notification - no response needed
              (progn
                (ws-response-header process 200
                                    '("Content-Type" . "text/plain")
                                    '("Content-Length" . "0"))
                (throw 'close-connection nil))
            ;; Request - dispatch and respond
            (let ((result (mcp-server--dispatch method params)))
              (ws-response-header process 200
                                  '("Content-Type" . "application/json")
                                  '("Access-Control-Allow-Origin" . "*"))
              (ws-send process
                       (json-encode
                        (if result
                            `((jsonrpc . "2.0") (id . ,id) (result . ,result))
                          `((jsonrpc . "2.0") (id . ,id) (result . nil)))))
              (throw 'close-connection nil))))
      (error
       (ws-response-header process 200
                           '("Content-Type" . "application/json"))
       (ws-send process
                (json-encode
                 `((jsonrpc . "2.0")
                   (id . ,(or id "error"))
                   (result . ((content . [((type . "text")
                                           (text . ,(format "Error: %s"
                                                            (error-message-string err))))]))))))
       (throw 'close-connection nil)))))

;;; ============================================================
;;; Public API
;;; ============================================================

;;;###autoload
(defun mcp-server-start (project-root &optional port)
  "Start MCP server for PROJECT-ROOT on PORT (0 for auto)."
  (interactive "DProject root: ")
  (unless (featurep 'web-server)
    (error "web-server package not available"))
  (when mcp-server--server (mcp-server-stop))
  (setq mcp-server--project-root (file-truename (expand-file-name project-root)))
  (let* ((server (ws-start
                  `(((:GET . "^/reload$") . ,#'mcp-server--handle-reload)
                    ((:POST . "^/mcp.*") . ,#'mcp-server--handle-post))
                  (or port 0) nil :host "127.0.0.1")))
    (setq mcp-server--server server
          mcp-server--port (process-contact (ws-process server) :service))
    (message "MCP Server started on port %d" mcp-server--port)
    (cons server mcp-server--port)))

;;;###autoload
(defun mcp-server-stop ()
  "Stop MCP server."
  (interactive)
  (when mcp-server--server
    (ws-stop mcp-server--server)
    (setq mcp-server--server nil mcp-server--port nil)
    (message "MCP Server stopped")))

;;;###autoload
(defun mcp-server-status ()
  "Show MCP server status."
  (interactive)
  (message (if mcp-server--server
               (format "MCP Server on port %d for %s"
                       mcp-server--port mcp-server--project-root)
             "MCP Server not running")))

;;;###autoload
(defun mcp-server-get-config ()
  "Get MCP server URL configuration."
  (when (and mcp-server--server mcp-server--port)
    `((url . ,(format "http://127.0.0.1:%d/mcp" mcp-server--port)))))

(provide 'mcp-server)

;;; mcp-server.el ends here
