;;; mcp-session.el --- AI Development Session Management -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, session

;;; Commentary:
;;
;; Session management for AI-assisted development.
;; Features:
;; - Create sessions with AI agent selection (Gemini, Codex, Claude)
;; - MCP server integration per workspace
;; - Tab-based session management
;; - Delegates layout to multi-panel.el
;;

;;; Code:

(require 'cl-lib)
(require 'json)

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup ai-session nil
  "AI Development Session Management."
  :group 'tools
  :prefix "ai-session-")

(defcustom ai-session-mcp-tools
  '(((name . "read_file")
     (description . "Read file with line numbers. Use offset for pagination.")
     (inputSchema . ((type . "object")
                     (properties . ((file_path . ((type . "string")
                                                  (description . "File path (relative to project root)")))
                                    (offset . ((type . "integer")
                                               (description . "Start line (0-indexed)")
                                               (default . 0)))
                                    (limit . ((type . "integer")
                                              (description . "Max lines")
                                              (default . 3000)))))
                     (required . ["file_path"]))))
    ((name . "write_file")
     (description . "Create new file. Use supersede=true to overwrite existing.")
     (inputSchema . ((type . "object")
                     (properties . ((file_path . ((type . "string")
                                                  (description . "File path")))
                                    (content . ((type . "string")
                                                (description . "File content")))
                                    (supersede . ((type . "boolean")
                                                  (description . "Overwrite if exists")
                                                  (default . :json-false)))))
                     (required . ["file_path" "content"]))))
    ((name . "request_edit")
     (description . "Request partial file edit. Opens review UI in Emacs for approval.")
     (inputSchema . ((type . "object")
                     (properties . ((file_path . ((type . "string")
                                                  (description . "File path")))
                                    (start_line . ((type . "integer")
                                                   (description . "Start line (1-indexed, inclusive)")))
                                    (end_line . ((type . "integer")
                                                 (description . "End line (1-indexed, inclusive)")))
                                    (content . ((type . "string")
                                                (description . "New content")))
                                    (comment . ((type . "string")
                                                (description . "Explanation for reviewer")
                                                (default . "")))))
                     (required . ["file_path" "start_line" "end_line" "content"])))))
  "List of MCP tools to expose to AI agents."
  :type 'sexp
  :group 'ai-session)

(defcustom ai-session-enable-mcp-server t
  "Enable the Elisp-based MCP HTTP server for AI agent integration."
  :type 'boolean
  :group 'ai-session)

(defcustom ai-session-available-agents '("Claude" "Gemini" "Codex")
  "List of available AI agents."
  :type '(repeat string)
  :group 'ai-session)

;;; ============================================================
;;; Data Structures
;;; ============================================================

(cl-defstruct ai-session
  "Structure representing an AI development session."
  id                    ; Unique session ID
  title                 ; Session title
  agent                 ; AI agent name (Claude, Gemini, Codex)
  agent-config          ; Agent config plist from JSON
  workspace             ; Workspace directory path
  mcp-server            ; MCP server object (cons of server . port for Elisp)
  mcp-config-file       ; Path to MCP config file for this session
  tab-index             ; Tab-bar index
  created-at            ; Creation timestamp
  session-id            ; Agent's session ID for resume
  buffers)              ; List of session-related buffers

(defvar ai-session--sessions (make-hash-table :test 'equal)
  "Hash table of active sessions by ID.")

(defvar ai-session--current nil
  "Current active session.")

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun ai-session--generate-id ()
  "Generate a unique session ID."
  (format "session-%s" (format-time-string "%Y%m%d-%H%M%S")))

(defun ai-session--get-session (id)
  "Get session by ID."
  (gethash id ai-session--sessions))

(defun ai-session--register (session)
  "Register SESSION in the session table."
  (puthash (ai-session-id session) session ai-session--sessions))

(defun ai-session--unregister (session)
  "Unregister SESSION from the session table."
  (remhash (ai-session-id session) ai-session--sessions))

;;; ============================================================
;;; Session Persistence
;;; ============================================================

(defconst ai-session--hikettei-dir ".hikettei"
  "Directory name for session data within workspace.")

(defconst ai-session--sessions-file "sessions.json"
  "Filename for session history.")

(defun ai-session--ensure-hikettei-dir (workspace)
  "Ensure .hikettei directory exists in WORKSPACE."
  (let ((dir (expand-file-name ai-session--hikettei-dir workspace)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun ai-session--sessions-file-path (workspace)
  "Get the sessions.json file path for WORKSPACE."
  (expand-file-name ai-session--sessions-file
                    (expand-file-name ai-session--hikettei-dir workspace)))

(defun ai-session--load-workspace-sessions (workspace)
  "Load session history from WORKSPACE."
  (let ((file (ai-session--sessions-file-path workspace)))
    (if (file-exists-p file)
        (condition-case err
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'symbol))
              (cdr (assoc 'sessions (json-read-file file))))
          (error
           (message "Error loading sessions: %s" (error-message-string err))
           nil))
      nil)))

(defun ai-session--save-workspace-sessions (workspace sessions)
  "Save SESSIONS list to WORKSPACE."
  (ai-session--ensure-hikettei-dir workspace)
  (let ((file (ai-session--sessions-file-path workspace))
        (json-encoding-pretty-print t))
    (with-temp-file file
      (insert (json-encode `((sessions . ,sessions)))))))

(defun ai-session--add-to-history (session)
  "Add SESSION to workspace history."
  (let* ((workspace (ai-session-workspace session))
         (existing (ai-session--load-workspace-sessions workspace))
         (entry `((id . ,(ai-session-id session))
                  (agent . ,(ai-session-agent session))
                  (title . ,(ai-session-title session))
                  (created_at . ,(format-time-string "%Y-%m-%dT%H:%M:%S"
                                                     (ai-session-created-at session)))
                  (last_accessed . ,(format-time-string "%Y-%m-%dT%H:%M:%S"))
                  (session_id . ,(ai-session-session-id session))))
         (updated (cons entry (seq-remove (lambda (e)
                                            (string= (cdr (assoc 'id e))
                                                     (ai-session-id session)))
                                          existing))))
    (ai-session--save-workspace-sessions workspace updated)))

(defun ai-session--update-last-accessed (session)
  "Update last_accessed timestamp for SESSION in history."
  (let* ((workspace (ai-session-workspace session))
         (sessions (ai-session--load-workspace-sessions workspace))
         (updated (mapcar (lambda (entry)
                            (if (string= (cdr (assoc 'id entry))
                                         (ai-session-id session))
                                (cons `(last_accessed . ,(format-time-string "%Y-%m-%dT%H:%M:%S"))
                                      (assq-delete-all 'last_accessed entry))
                              entry))
                          sessions)))
    (ai-session--save-workspace-sessions workspace updated)))

(defun ai-session--update-session-id (session new-session-id)
  "Update SESSION's session_id to NEW-SESSION-ID in history."
  (setf (ai-session-session-id session) new-session-id)
  (let* ((workspace (ai-session-workspace session))
         (sessions (ai-session--load-workspace-sessions workspace))
         (updated (mapcar (lambda (entry)
                            (if (string= (cdr (assoc 'id entry))
                                         (ai-session-id session))
                                (cons `(session_id . ,new-session-id)
                                      (assq-delete-all 'session_id entry))
                              entry))
                          sessions)))
    (ai-session--save-workspace-sessions workspace updated)))

(defun ai-session--delete-from-history (workspace session-id)
  "Delete session SESSION-ID from WORKSPACE history."
  (let* ((sessions (ai-session--load-workspace-sessions workspace))
         (updated (seq-remove (lambda (e)
                                (string= (cdr (assoc 'id e)) session-id))
                              sessions)))
    (ai-session--save-workspace-sessions workspace updated)))

;;; ============================================================
;;; MCP Server Management
;;; ============================================================

;; Add mcp subdirectory to load path and require MCP server
(let ((mcp-dir (expand-file-name "mcp" (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-directory-p mcp-dir)
    (add-to-list 'load-path mcp-dir)))
(require 'mcp-server nil t)

(defun ai-session--get-mcp-config-path (session)
  "Get the MCP config file path for SESSION.
Config files are stored at workspace/.hikettei/mcp-config-<agent>.json"
  (let* ((workspace (ai-session-workspace session))
         (agent-name (downcase (ai-session-agent session)))
         (hikettei-dir (expand-file-name ai-session--hikettei-dir workspace)))
    (expand-file-name (format "mcp-config-%s.json" agent-name) hikettei-dir)))

(defun ai-session--generate-mcp-config (session)
  "Generate MCP config file for SESSION and return its path.
The config file is stored at workspace/.hikettei/mcp-config-<agent>.json"
  (ai-session--ensure-hikettei-dir (ai-session-workspace session))
  (let* ((mcp-server (ai-session-mcp-server session))
         (port (when (consp mcp-server) (cdr mcp-server)))
         (config-file (ai-session--get-mcp-config-path session))
         (agent-name (ai-session-agent session))
         ;; Generate MCP config with the server URL
         (mcp-config (if (and ai-session-enable-mcp-server port)
                         ;; HTTP-based Elisp server - same format works for Claude, Gemini, Codex
                         `((mcpServers
                            . ((emacs-editor
                                . ((url . ,(format "http://127.0.0.1:%d/mcp" port))
                                   (type . "http"))))))
                       ;; Fallback: no MCP server configured
                       `((mcpServers . ,(make-hash-table :test 'equal)))))
         (json-encoding-pretty-print t))
    (with-temp-file config-file
      (insert (json-encode mcp-config)))
    (message "MCP config generated at %s for %s" config-file agent-name)
    config-file))

(defun ai-session--cleanup-mcp-config (session)
  "Remove MCP config file for SESSION."
  (let ((config-file (ai-session--get-mcp-config-path session)))
    (when (file-exists-p config-file)
      (delete-file config-file)
      (message "MCP config removed: %s" config-file))))

(defun ai-session--start-mcp-server (session)
  "Start MCP server for SESSION.
Returns the server object (cons of server . port), or nil if MCP is disabled."
  (let* ((agent-config (ai-session-agent-config session))
         (mcp-enabled (plist-get agent-config :mcp-enabled))
         (workspace (ai-session-workspace session)))
    (if (not mcp-enabled)
        (progn
          (message "MCP disabled for agent %s" (ai-session-agent session))
          nil)
      (if (and ai-session-enable-mcp-server
               (fboundp 'mcp-server-start))
          ;; Use Elisp MCP server
          (let ((result (mcp-server-start workspace)))
            (message "Elisp MCP Server started on port %d for %s"
                     (cdr result) workspace)
            result)
        ;; No MCP server available
        (message "MCP Server not available (web-server package may not be installed)")
        nil))))

(defun ai-session--stop-mcp-server (session)
  "Stop MCP server for SESSION."
  (when-let ((server (ai-session-mcp-server session)))
    (if (and ai-session-enable-mcp-server
             (fboundp 'mcp-server-stop))
        ;; Elisp server
        (progn
          (mcp-server-stop)
          (message "MCP Server stopped for %s" (ai-session-title session)))
      ;; Legacy process-based server
      (when (processp server)
        (when (process-live-p server)
          (delete-process server)
          (message "MCP Server stopped for %s" (ai-session-title session)))))))

;;; ============================================================
;;; Tab-bar Integration (Top-level Session Tabs)
;;; ============================================================

(defface ai-session-tab-active-face
  '((t :foreground "#f8f8f2" :background "#44475a" :weight bold))
  "Face for active session tab."
  :group 'ai-session)

(defface ai-session-tab-inactive-face
  '((t :foreground "#6272a4" :background "#282a36"))
  "Face for inactive session tab."
  :group 'ai-session)

(defface ai-session-tab-add-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for [+] add tab button."
  :group 'ai-session)

(defun ai-session--get-agent-emoji (agent)
  "Get emoji for AGENT name."
  (pcase agent
    ("Claude" "🤖")
    ("Gemini" "✨")
    ("Codex" "💻")
    (_ "📝")))

(defun ai-session--tab-bar-tab-name-format (tab i)
  "Format TAB name at index I for display in tab-bar."
  (let* ((current-p (eq (car tab) 'current-tab))
         (name (alist-get 'name tab))
         (face (if current-p 'ai-session-tab-active-face 'ai-session-tab-inactive-face))
         (close-face '(:foreground "#ff5555")))
    (concat
     (propertize (format " %s " name) 'face face)
     (propertize " × " 'face close-face
                 'close-tab t))))

(defun ai-session--new-session-from-tab-bar ()
  "Create a new session from tab-bar [+] button."
  (interactive)
  (tab-bar-new-tab)
  (tab-bar-rename-tab "✨ New Session")
  (setq ai-session--creating-from-wizard t)
  (if (fboundp 'session-wizard)
      (session-wizard)
    (call-interactively #'ai-session-new)))

(defun ai-session--confirm-before-close-tab (orig-fun &optional tab-number to-tab)
  "Advice to confirm before closing a tab."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (tab-index (if tab-number (1- tab-number) (tab-bar--current-tab-index)))
         (tab (nth tab-index tabs))
         (tab-name (alist-get 'name tab)))
    (when (yes-or-no-p (format "Close session '%s'? " tab-name))
      (funcall orig-fun tab-number to-tab))))

(defun ai-session--on-tab-switch ()
  "Called when switching tabs. Updates current session."
  (when-let* ((current-tab (tab-bar--current-tab))
              (tab-name (alist-get 'name current-tab)))
    (let ((found-session nil))
      (maphash (lambda (_id session)
                 (let* ((agent (ai-session-agent session))
                        (emoji (ai-session--get-agent-emoji agent))
                        (expected-name (format "%s %s" emoji (ai-session-title session))))
                   (when (string= tab-name expected-name)
                     (setq found-session session))))
               ai-session--sessions)
      (if found-session
          (setq ai-session--current found-session)
        ;; Not a session tab - hide NeoTree
        (when (and (fboundp 'neo-global--window-exists-p)
                   (neo-global--window-exists-p))
          (neotree-hide))))))

(defun ai-session--setup-tab-bar ()
  "Enable and configure tab-bar-mode for session management."
  (tab-bar-mode 1)
  (setq tab-bar-show t)
  (setq tab-bar-new-tab-choice nil)
  (setq tab-bar-close-button-show t)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-auto-width nil)
  (setq tab-bar-tab-name-format-function #'ai-session--tab-bar-tab-name-format)
  (advice-add 'tab-bar-close-tab :around #'ai-session--confirm-before-close-tab)
  (add-hook 'tab-bar-tab-post-select-functions
            (lambda (&rest _) (ai-session--on-tab-switch)))
  (setq tab-bar-format '(tab-bar-format-tabs ai-session--tab-bar-add-button)))

(defun ai-session--tab-bar-add-button ()
  "Return the [+] button for creating new sessions."
  `((add-session menu-item
                 ,(propertize " [+] " 'face 'ai-session-tab-add-face)
                 ai-session--new-session-from-tab-bar
                 :help "Create new session")))

(defun ai-session--create-tab (session &optional reuse-current)
  "Create a new tab for SESSION.
If REUSE-CURRENT is non-nil, rename current tab instead of creating new one."
  (let* ((agent (ai-session-agent session))
         (emoji (ai-session--get-agent-emoji agent))
         (tab-name (format "%s %s" emoji (ai-session-title session))))
    (unless reuse-current
      (tab-bar-new-tab))
    (tab-bar-rename-tab tab-name)
    (setf (ai-session-tab-index session) (tab-bar--current-tab-index))))

(defvar ai-session--creating-from-wizard nil
  "Flag to indicate session is being created from wizard.")

(defun ai-session--close-tab (session)
  "Close the tab for SESSION."
  (when-let ((idx (ai-session-tab-index session)))
    (tab-bar-close-tab (1+ idx))))

;;; ============================================================
;;; Layout Management (delegates to multi-panel)
;;; ============================================================

(defun ai-session--get-mcp-config-flag (agent-config config-file)
  "Get the MCP config flag for AGENT-CONFIG with CONFIG-FILE.
Uses the mcp-config-flag from agent config, or falls back to --mcp-config."
  (when (and config-file (plist-get agent-config :mcp-enabled))
    (let ((flag (or (plist-get agent-config :mcp-config-flag) "--mcp-config")))
      (format "%s %s" flag (shell-quote-argument config-file)))))

(defun ai-session--build-env-prefix (env-alist)
  "Build environment variable prefix from ENV-ALIST.
ENV-ALIST is ((KEY . VALUE) ...) from agent config."
  (when env-alist
    (mapconcat (lambda (pair)
                 (format "%s=%s" (car pair) (cdr pair)))
               env-alist " ")))

(defcustom ai-session-system-prompt-file "prompt/SYSTEM_INSTRUCTION.md"
  "Path to system instruction file relative to `user-emacs-directory'.
This file is loaded and passed to AI agents via --append-system-prompt."
  :type 'string
  :group 'ai-session)

(defun ai-session--load-system-prompt ()
  "Load system prompt content from `ai-session-system-prompt-file'.
Returns the file content as a string, or nil if file doesn't exist."
  (let ((file (expand-file-name ai-session-system-prompt-file user-emacs-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun ai-session--build-system-prompt-args (agent-name)
  "Build system prompt arguments for AGENT-NAME.
Reads the prompt file content and returns --append-system-prompt with it."
  (when-let ((prompt-content (ai-session--load-system-prompt)))
    (format "--append-system-prompt %s" (shell-quote-argument prompt-content))))

(defun ai-session--build-agent-command (session &optional resume)
  "Build the command string to launch agent for SESSION.
If RESUME is non-nil, append resume_args and session_id.
Always includes args, MCP config flag, system prompt, and environment variables."
  (let* ((config (ai-session-agent-config session))
         (executable (plist-get config :executable))
         (args (plist-get config :args))
         (resume-args (plist-get config :resume-args))
         (env (plist-get config :env))
         (session-id (ai-session-session-id session))
         (mcp-config-file (ai-session-mcp-config-file session))
         (mcp-args (ai-session--get-mcp-config-flag config mcp-config-file))
         (env-prefix (ai-session--build-env-prefix env))
         ;; Load system prompt for new sessions only (not resume)
         (system-prompt-args (unless resume
                               (ai-session--build-system-prompt-args
                                (ai-session-agent session))))
         (quoted-args (mapconcat #'shell-quote-argument args " "))
         (quoted-resume-args (mapconcat #'shell-quote-argument resume-args " "))
         (base-cmd (if resume
                       (format "%s %s %s %s %s"
                               executable
                               (or mcp-args "")
                               quoted-args
                               quoted-resume-args
                               (or session-id ""))
                     (format "%s %s %s %s"
                             executable
                             (or mcp-args "")
                             (or system-prompt-args "")
                             quoted-args))))
    (string-trim
     (if env-prefix
         (format "%s %s" env-prefix base-cmd)
       base-cmd))))

(defun ai-session--setup-layout (session &optional resume)
  "Setup layout for SESSION. Delegates to multi-panel if available."
  (if (fboundp 'mp-initialize)
      (mp-initialize session resume)
    (ai-session--setup-fallback-layout session resume)))

(defun ai-session--setup-fallback-layout (session &optional resume)
  "Simple fallback layout when multi-panel is not available."
  (delete-other-windows)
  (let* ((workspace (ai-session-workspace session))
         (agent-cmd (ai-session--build-agent-command session resume)))
    ;; Just open vterm with the agent
    (when (fboundp 'vterm)
      (vterm)
      (rename-buffer (format "*AI Chat: %s*" (ai-session-title session)) t)
      (run-at-time 0.1 nil
                   (lambda (ws cmd)
                     (when (and (boundp 'vterm--process) vterm--process)
                       (vterm-send-string (format "cd %s && %s\n"
                                                  (shell-quote-argument ws) cmd))))
                   workspace agent-cmd))
    (setf (ai-session-buffers session) (list (current-buffer)))))

;;; ============================================================
;;; Interactive Commands
;;; ============================================================

;;;###autoload
(defun ai-session-new ()
  "Create a new AI development session interactively."
  (interactive)
  (ai-session--setup-tab-bar)
  (let* ((agent (completing-read "Select AI Agent: "
                                 ai-session-available-agents
                                 nil t nil nil "Claude"))
         (title (read-string "Session Title: " nil nil "Untitled"))
         (workspace (read-directory-name "Workspace Directory: "
                                         default-directory nil t)))
    (ai-session-create :agent agent :title title :workspace workspace)))

(cl-defun ai-session-create (&key agent agent-config title workspace)
  "Create a new session with AGENT, AGENT-CONFIG, TITLE, and WORKSPACE."
  (let* ((id (ai-session--generate-id))
         (reuse-tab ai-session--creating-from-wizard)
         (session (make-ai-session
                   :id id
                   :title title
                   :agent agent
                   :agent-config agent-config
                   :workspace (expand-file-name workspace)
                   :mcp-server nil
                   :mcp-config-file nil
                   :tab-index nil
                   :created-at (current-time)
                   :session-id nil
                   :buffers nil)))

    (setq ai-session--creating-from-wizard nil)
    (ai-session--register session)
    (setq ai-session--current session)
    (ai-session--setup-tab-bar)
    (ai-session--create-tab session reuse-tab)

    ;; Start MCP server first (needed for config generation)
    (setf (ai-session-mcp-server session)
          (ai-session--start-mcp-server session))
    ;; Then generate MCP config (uses server port)
    (setf (ai-session-mcp-config-file session)
          (ai-session--generate-mcp-config session))

    (ai-session--add-to-history session)
    (ai-session--setup-layout session nil)

    (message "Session '%s' created with %s" title agent)
    session))

(cl-defun ai-session-resume (&key id agent agent-config title workspace session-id)
  "Resume a session with ID, AGENT, AGENT-CONFIG, TITLE, WORKSPACE and SESSION-ID."
  (let* ((session (make-ai-session
                   :id id
                   :title title
                   :agent agent
                   :agent-config agent-config
                   :workspace (expand-file-name workspace)
                   :mcp-server nil
                   :mcp-config-file nil
                   :tab-index nil
                   :created-at (current-time)
                   :session-id session-id
                   :buffers nil)))

    (ai-session--register session)
    (setq ai-session--current session)
    (ai-session--setup-tab-bar)
    (ai-session--create-tab session)

    ;; Start MCP server first (needed for config generation)
    (setf (ai-session-mcp-server session)
          (ai-session--start-mcp-server session))
    ;; Then generate MCP config (uses server port)
    (setf (ai-session-mcp-config-file session)
          (ai-session--generate-mcp-config session))

    (ai-session--update-last-accessed session)
    (ai-session--setup-layout session t)

    (message "Session '%s' resumed with %s" title agent)
    session))

(defun ai-session-close (&optional session)
  "Close SESSION or current session."
  (interactive)
  (let ((sess (or session ai-session--current)))
    (when sess
      (when (y-or-n-p (format "Close session '%s'? " (ai-session-title sess)))
        (ai-session--stop-mcp-server sess)
        (ai-session--cleanup-mcp-config sess)
        (dolist (buf (ai-session-buffers sess))
          (when (buffer-live-p buf)
            (kill-buffer buf)))
        (ai-session--close-tab sess)
        (ai-session--unregister sess)
        (when (eq sess ai-session--current)
          (setq ai-session--current nil))
        (message "Session '%s' closed" (ai-session-title sess))))))

(defun ai-session-list ()
  "List all active sessions."
  (interactive)
  (let ((sessions '()))
    (maphash (lambda (_k v) (push v sessions)) ai-session--sessions)
    (if sessions
        (progn
          (message "Active sessions:")
          (dolist (s sessions)
            (message "  [%s] %s - %s"
                     (ai-session-agent s)
                     (ai-session-title s)
                     (ai-session-workspace s))))
      (message "No active sessions"))))

(defun ai-session-switch ()
  "Switch to another session using tab-bar."
  (interactive)
  (call-interactively #'tab-bar-switch-to-tab))

;;; ============================================================
;;; Navigation Commands
;;; ============================================================

(defun ai-session-open-file ()
  "Open a file in the current session's workspace."
  (interactive)
  (when ai-session--current
    (let ((default-directory (ai-session-workspace ai-session--current)))
      (call-interactively #'find-file))))

(defun ai-session-toggle-neotree ()
  "Toggle neotree in the current session's workspace."
  (interactive)
  (when (and ai-session--current (fboundp 'neotree-toggle))
    (neotree-dir (ai-session-workspace ai-session--current))))

(defun ai-session-focus-chat ()
  "Focus the AI chat window."
  (interactive)
  (when ai-session--current
    (let ((chat-buffer (get-buffer
                        (format "*AI Chat: %s*"
                                (ai-session-title ai-session--current)))))
      (when chat-buffer
        (pop-to-buffer chat-buffer)))))

(provide 'mcp-session)

;;; mcp-session.el ends here
