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
  agent                 ; AI agent (Claude, Gemini, Codex)
  workspace             ; Workspace directory path
  mcp-process           ; MCP server process
  tab-index             ; Tab-bar index
  created-at            ; Creation timestamp
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

(defun ai-session--setup-layout (session)
  "Setup multi-pane layout for SESSION.
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
                       (format "*Session: %s*" (ai-session-title session)))))

    ;; Main window (left) - for file editing
    (switch-to-buffer main-buffer)
    (with-current-buffer main-buffer
      (erase-buffer)
      (insert (format "Session: %s\n" (ai-session-title session)))
      (insert (format "Agent: %s\n" (ai-session-agent session)))
      (insert (format "Workspace: %s\n\n" workspace))
      (insert "Press 'f' to open a file\n")
      (insert "Press 'n' to open neotree\n")
      (insert "Press 't' to focus terminal\n")
      (ai-session-main-mode))

    ;; Right window - for AI chat (placeholder)
    (split-window-right)
    (other-window 1)
    (let ((chat-buffer (get-buffer-create
                        (format "*AI Chat: %s*" (ai-session-title session)))))
      (switch-to-buffer chat-buffer)
      (with-current-buffer chat-buffer
        (erase-buffer)
        (insert (format "AI Chat (%s)\n" (ai-session-agent session)))
        (insert "─────────────────────────\n\n")
        (insert "(Chat interface placeholder)\n")))

    ;; Bottom window - Terminal
    (other-window -1)  ; back to main
    (split-window-below (- (window-height) 10))
    (other-window 1)
    (if (fboundp 'multi-term)
        (multi-term)
      (term "/bin/zsh"))
    (when workspace
      (term-send-raw-string (format "cd %s\n" (shell-quote-argument workspace))))

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

(cl-defun ai-session-create (&key agent title workspace)
  "Create a new session with AGENT, TITLE, and WORKSPACE."
  (let* ((id (ai-session--generate-id))
         (session (make-ai-session
                   :id id
                   :title title
                   :agent agent
                   :workspace (expand-file-name workspace)
                   :mcp-process nil
                   :tab-index nil
                   :created-at (current-time)
                   :buffers nil)))

    ;; Register session
    (ai-session--register session)
    (setq ai-session--current session)

    ;; Create tab
    (ai-session--create-tab session)

    ;; Start MCP server
    (setf (ai-session-mcp-process session)
          (ai-session--start-mcp-server workspace))

    ;; Setup layout
    (ai-session--setup-layout session)

    (message "Session '%s' created with %s" title agent)
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
