;;; mcp-session.el --- AI Development Session Management -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, ai, session

;;; Commentary:
;;
;; Session management for AI-assisted development.
;; Features:
;; - Create sessions with AI agent selection (Gemini, Codex, Claude)
;; - MCP server integration per workspace
;; - Tab-based session management
;; - Multi-pane layout for development
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

(defcustom ai-session-mcp-server-command "uv"
  "Command to run MCP server."
  :type 'string
  :group 'ai-session)

(defcustom ai-session-mcp-server-args '("run" "mcp-file-editor")
  "Arguments for MCP server command."
  :type '(repeat string)
  :group 'ai-session)

(defcustom ai-session-mcp-server-directory "~/.emacs.d"
  "Directory where MCP server pyproject.toml is located."
  :type 'string
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
  mcp-process           ; MCP server process
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

(defun ai-session--start-mcp-server (workspace)
  "Start MCP server for WORKSPACE and return the process."
  (let* ((default-directory (expand-file-name ai-session-mcp-server-directory))
         (process-name (format "mcp-server-%s" (md5 workspace)))
         (buffer-name (format "*MCP Server: %s*" (file-name-nondirectory workspace)))
         (args (append ai-session-mcp-server-args (list workspace))))
    ;; Kill existing process if any
    (when-let ((existing (get-process process-name)))
      (delete-process existing))
    ;; Start new process
    (let ((proc (apply #'start-process
                       process-name
                       buffer-name
                       ai-session-mcp-server-command
                       args)))
      (set-process-query-on-exit-flag proc nil)
      (message "MCP Server started for %s" workspace)
      proc)))

(defun ai-session--stop-mcp-server (session)
  "Stop MCP server for SESSION."
  (when-let ((proc (ai-session-mcp-process session)))
    (when (process-live-p proc)
      (delete-process proc)
      (message "MCP Server stopped for %s" (ai-session-title session)))))

;;; ============================================================
;;; Tab-bar Integration
;;; ============================================================

(defun ai-session--setup-tab-bar ()
  "Enable and configure tab-bar-mode for session management."
  (tab-bar-mode 1)
  (setq tab-bar-show t)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-tab-hints t))

(defun ai-session--create-tab (session)
  "Create a new tab for SESSION."
  (let ((tab-name (format "[%s] %s"
                          (ai-session-agent session)
                          (ai-session-title session))))
    (tab-bar-new-tab)
    (tab-bar-rename-tab tab-name)
    (setf (ai-session-tab-index session) (tab-bar--current-tab-index))))

(defun ai-session--close-tab (session)
  "Close the tab for SESSION."
  (when-let ((idx (ai-session-tab-index session)))
    (tab-bar-close-tab (1+ idx))))  ; tab-bar uses 1-indexed

;;; ============================================================
;;; Multi-pane Layout
;;; ============================================================

(defun ai-session--build-agent-command (session &optional resume)
  "Build the command string to launch agent for SESSION.
If RESUME is non-nil, use resume_args with session_id."
  (let* ((config (ai-session-agent-config session))
         (executable (plist-get config :executable))
         (args (if resume
                   (plist-get config :resume-args)
                 (plist-get config :args)))
         (session-id (ai-session-session-id session)))
    (if resume
        (format "%s %s %s"
                executable
                (mapconcat #'identity args " ")
                (or session-id ""))
      (format "%s %s"
              executable
              (mapconcat #'identity args " ")))))

(defun ai-session--setup-layout (session &optional resume)
  "Setup multi-pane layout for SESSION.
If RESUME is non-nil, resume existing session.
Layout:
+------------------+------------------+
|                  |                  |
|    File/Code     |    AI Chat       |
|    (Main)        |    (Right)       |
|                  |                  |
+------------------+------------------+
|                Terminal             |
+------------------------------------|"
  (delete-other-windows)

  (let* ((workspace (ai-session-workspace session))
         (main-buffer (get-buffer-create
                       (format "*Session: %s*" (ai-session-title session))))
         (agent-cmd (ai-session--build-agent-command session resume)))

    ;; Main window (left) - for file editing
    (switch-to-buffer main-buffer)
    (with-current-buffer main-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Session: %s\n" (ai-session-title session)))
        (insert (format "Agent: %s\n" (ai-session-agent session)))
        (insert (format "Workspace: %s\n\n" workspace))
        (insert "Press 'f' to open a file\n")
        (insert "Press 'n' to open neotree\n")
        (insert "Press 't' to focus terminal\n"))
      (ai-session-main-mode))

    ;; Right window - for AI chat (placeholder)
    (split-window-right)
    (other-window 1)
    (let ((chat-buffer (get-buffer-create
                        (format "*AI Chat: %s*" (ai-session-title session)))))
      (switch-to-buffer chat-buffer)
      (with-current-buffer chat-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "AI Chat (%s)\n" (ai-session-agent session)))
          (insert "─────────────────────────\n\n")
          (insert "(Chat interface placeholder)\n"))))

    ;; Bottom window - Terminal with AI agent
    (other-window -1)  ; back to main
    (split-window-below (- (window-height) 15))
    (other-window 1)
    (if (fboundp 'multi-term)
        (multi-term)
      (term "/bin/zsh"))

    ;; cd to workspace and launch AI agent
    (when workspace
      (term-send-raw-string (format "cd %s && %s\n"
                                    (shell-quote-argument workspace)
                                    agent-cmd)))

    ;; Return to main window
    (other-window -1)
    (other-window -1)

    ;; Track buffers
    (setf (ai-session-buffers session)
          (list main-buffer
                (get-buffer (format "*AI Chat: %s*" (ai-session-title session)))))))

;;; ============================================================
;;; Session Main Mode
;;; ============================================================

(defvar ai-session-main-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'ai-session-open-file)
    (define-key map (kbd "n") #'ai-session-toggle-neotree)
    (define-key map (kbd "t") #'ai-session-focus-terminal)
    (define-key map (kbd "c") #'ai-session-focus-chat)
    (define-key map (kbd "q") #'ai-session-close)
    map)
  "Keymap for `ai-session-main-mode'.")

(define-derived-mode ai-session-main-mode special-mode "AI-Session"
  "Major mode for AI session main buffer."
  (setq buffer-read-only t))

;;; ============================================================
;;; Interactive Commands
;;; ============================================================

;;;###autoload
(defun ai-session-new ()
  "Create a new AI development session interactively."
  (interactive)
  ;; Setup tab-bar if not already
  (ai-session--setup-tab-bar)

  ;; Prompt for session details
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
         (session (make-ai-session
                   :id id
                   :title title
                   :agent agent
                   :agent-config agent-config
                   :workspace (expand-file-name workspace)
                   :mcp-process nil
                   :tab-index nil
                   :created-at (current-time)
                   :session-id nil
                   :buffers nil)))

    ;; Register session
    (ai-session--register session)
    (setq ai-session--current session)

    ;; Setup tab-bar if not already
    (ai-session--setup-tab-bar)

    ;; Create tab
    (ai-session--create-tab session)

    ;; Start MCP server
    (setf (ai-session-mcp-process session)
          (ai-session--start-mcp-server workspace))

    ;; Save to history
    (ai-session--add-to-history session)

    ;; Setup layout and launch agent
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
                   :mcp-process nil
                   :tab-index nil
                   :created-at (current-time)
                   :session-id session-id
                   :buffers nil)))

    ;; Register session
    (ai-session--register session)
    (setq ai-session--current session)

    ;; Setup tab-bar if not already
    (ai-session--setup-tab-bar)

    ;; Create tab
    (ai-session--create-tab session)

    ;; Start MCP server
    (setf (ai-session-mcp-process session)
          (ai-session--start-mcp-server workspace))

    ;; Update last accessed
    (ai-session--update-last-accessed session)

    ;; Setup layout and launch agent with resume
    (ai-session--setup-layout session t)

    (message "Session '%s' resumed with %s" title agent)
    session))

(defun ai-session-close (&optional session)
  "Close SESSION or current session."
  (interactive)
  (let ((sess (or session ai-session--current)))
    (when sess
      (when (y-or-n-p (format "Close session '%s'? " (ai-session-title sess)))
        ;; Stop MCP server
        (ai-session--stop-mcp-server sess)

        ;; Kill session buffers
        (dolist (buf (ai-session-buffers sess))
          (when (buffer-live-p buf)
            (kill-buffer buf)))

        ;; Close tab
        (ai-session--close-tab sess)

        ;; Unregister
        (ai-session--unregister sess)

        ;; Clear current if this was it
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
;;; Layout Navigation
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
    (let ((workspace (ai-session-workspace ai-session--current)))
      (neotree-dir workspace))))

(defun ai-session-focus-terminal ()
  "Focus the terminal window."
  (interactive)
  (let ((term-buffer (seq-find (lambda (buf)
                                 (string-match-p "\\*terminal\\|\\*term"
                                                 (buffer-name buf)))
                               (buffer-list))))
    (when term-buffer
      (pop-to-buffer term-buffer))))

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
