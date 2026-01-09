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

(defcustom ai-session-mcp-server-args '("run" "emacs-mcp-server")
  "Arguments for MCP server command."
  :type '(repeat string)
  :group 'ai-session)

(defcustom ai-session-mcp-server-directory "~/.emacs.d"
  "Directory where MCP server pyproject.toml is located."
  :type 'string
  :group 'ai-session)

(defcustom ai-session-mcp-config-dir "~/.emacs.d/.hikettei/mcp-configs"
  "Directory to store generated MCP config files."
  :type 'string
  :group 'ai-session)

(defcustom ai-session-available-agents '("Claude" "Gemini" "Codex")
  "List of available AI agents."
  :type '(repeat string)
  :group 'ai-session)

;;; ============================================================
;;; Data Structures
;;; ============================================================

(cl-defstruct ai-session-inner-tab
  "Structure representing an inner tab within a session."
  id                    ; Unique tab ID
  name                  ; Tab display name
  type                  ; Tab type: 'terminal, 'file, 'chat, 'main
  buffer                ; Associated buffer
  icon)                 ; Icon for the tab

(cl-defstruct ai-session
  "Structure representing an AI development session."
  id                    ; Unique session ID
  title                 ; Session title
  agent                 ; AI agent name (Claude, Gemini, Codex)
  agent-config          ; Agent config plist from JSON
  workspace             ; Workspace directory path
  mcp-process           ; MCP server process
  mcp-config-file       ; Path to MCP config file for this session
  tab-index             ; Tab-bar index
  created-at            ; Creation timestamp
  session-id            ; Agent's session ID for resume
  buffers               ; List of session-related buffers
  inner-tabs            ; List of inner tabs (ai-session-inner-tab)
  current-inner-tab)    ; Current active inner tab index

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

(defun ai-session--ensure-mcp-config-dir ()
  "Ensure MCP config directory exists."
  (let ((dir (expand-file-name ai-session-mcp-config-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun ai-session--generate-mcp-config (session)
  "Generate MCP config file for SESSION and return its path."
  (ai-session--ensure-mcp-config-dir)
  (let* ((workspace (ai-session-workspace session))
         (config-file (expand-file-name
                       (format "mcp-%s.json" (ai-session-id session))
                       ai-session-mcp-config-dir))
         (mcp-config `((mcpServers
                        . ((emacs-editor
                            . ((command . ,ai-session-mcp-server-command)
                               (args . ,(vconcat ai-session-mcp-server-args))
                               (cwd . ,(expand-file-name ai-session-mcp-server-directory))))))))
         (json-encoding-pretty-print t))
    (with-temp-file config-file
      (insert (json-encode mcp-config)))
    config-file))

(defun ai-session--cleanup-mcp-config (session)
  "Remove MCP config file for SESSION."
  (let ((config-file (expand-file-name
                      (format "mcp-%s.json" (ai-session-id session))
                      ai-session-mcp-config-dir)))
    (when (file-exists-p config-file)
      (delete-file config-file))))

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

(defun ai-session--confirm-close-tab (tab-index tab-name)
  "Confirm and close tab at TAB-INDEX with TAB-NAME."
  (when (yes-or-no-p (format "Close session '%s'? " tab-name))
    (tab-bar-close-tab tab-index)))

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
  ;; Create a new tab first to preserve current session
  (tab-bar-new-tab)
  (tab-bar-rename-tab "✨ New Session")
  ;; Set flag so ai-session-create will reuse this tab
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

(defun ai-session--setup-tab-bar ()
  "Enable and configure tab-bar-mode for session management."
  (tab-bar-mode 1)
  (setq tab-bar-show t)
  (setq tab-bar-new-tab-choice nil)
  (setq tab-bar-close-button-show t)  ; Use built-in close button
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-auto-width nil)
  (setq tab-bar-tab-name-format-function #'ai-session--tab-bar-tab-name-format)
  ;; Add confirmation advice
  (advice-add 'tab-bar-close-tab :around #'ai-session--confirm-before-close-tab)
  ;; Add [+] button at the end (our custom one that opens session wizard)
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
    (tab-bar-close-tab (1+ idx))))  ; tab-bar uses 1-indexed

;;; ============================================================
;;; Inner Tab System (Second-level tabs within session)
;;; ============================================================

(defface ai-session-inner-tab-active-face
  '((t :background "#44475a" :foreground "#f8f8f2" :weight bold :box (:line-width -1 :color "#6272a4")))
  "Face for active inner tab."
  :group 'ai-session)

(defface ai-session-inner-tab-inactive-face
  '((t :background "#282a36" :foreground "#6272a4"))
  "Face for inactive inner tab."
  :group 'ai-session)

(defface ai-session-inner-tab-bar-face
  '((t :background "#21222c" :extend t))
  "Face for inner tab bar background."
  :group 'ai-session)

(defvar-local ai-session--inner-tab-line nil
  "Header line format for inner tabs.")

(defun ai-session--generate-inner-tab-id ()
  "Generate unique inner tab ID."
  (format "itab-%s" (format-time-string "%H%M%S%3N")))

(defun ai-session--create-inner-tab (session name type buffer &optional icon)
  "Create an inner tab for SESSION with NAME, TYPE, BUFFER and optional ICON."
  (let ((tab (make-ai-session-inner-tab
              :id (ai-session--generate-inner-tab-id)
              :name name
              :type type
              :buffer buffer
              :icon (or icon (ai-session--get-icon-for-type type)))))
    (push tab (ai-session-inner-tabs session))
    (setf (ai-session-inner-tabs session)
          (nreverse (ai-session-inner-tabs session)))
    tab))

(defun ai-session--get-icon-for-type (type)
  "Get icon for inner tab TYPE."
  (pcase type
    ('terminal "")
    ('file "")
    ('chat "")
    ('main "")
    (_ "")))

(defface ai-session-inner-tab-hover-face
  '((t :underline (:color "#6272a4" :style line)))
  "Face for inner tab hover - subtle underline only."
  :group 'ai-session)

(defun ai-session--render-inner-tabs ()
  "Render inner tabs as header-line for current session."
  (when ai-session--current
    (let* ((tabs (ai-session-inner-tabs ai-session--current))
           (current-idx (or (ai-session-current-inner-tab ai-session--current) 0))
           (tab-strings '()))
      ;; Build tab strings
      (let ((idx 0))
        (dolist (tab tabs)
          (let* ((active (= idx current-idx))
                 (icon (ai-session-inner-tab-icon tab))
                 (name (ai-session-inner-tab-name tab))
                 (face (if active 'ai-session-inner-tab-active-face 'ai-session-inner-tab-inactive-face))
                 (tab-str (propertize (format " %s %s " icon name)
                                      'face face
                                      'pointer 'hand
                                      'local-map (ai-session--inner-tab-keymap idx)
                                      'help-echo (format "Click to switch to %s" name))))
            (push tab-str tab-strings))
          (setq idx (1+ idx))))
      ;; Add [+] button
      (push (propertize " + "
                        'face 'ai-session-tab-add-face
                        'pointer 'hand
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [header-line mouse-1] #'ai-session-add-inner-tab)
                                     map)
                        'help-echo "Add new tab")
            tab-strings)
      ;; Combine with background
      (concat (propertize " " 'face 'ai-session-inner-tab-bar-face)
              (mapconcat #'identity (nreverse tab-strings) " ")
              (propertize " " 'display '(space :align-to right) 'face 'ai-session-inner-tab-bar-face)))))

(defun ai-session--inner-tab-keymap (idx)
  "Create keymap for inner tab at IDX."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda () (interactive) (ai-session-switch-inner-tab idx)))
    map))

(defun ai-session-switch-inner-tab (idx)
  "Switch to inner tab at IDX."
  (interactive "nTab index: ")
  (when ai-session--current
    (let* ((tabs (ai-session-inner-tabs ai-session--current))
           (tab (nth idx tabs)))
      (when tab
        (setf (ai-session-current-inner-tab ai-session--current) idx)
        (let ((buf (ai-session-inner-tab-buffer tab)))
          (when (buffer-live-p buf)
            (switch-to-buffer buf)))
        (ai-session--update-all-header-lines)))))

(defun ai-session--update-all-header-lines ()
  "Update header lines in all session buffers."
  (when ai-session--current
    (dolist (tab (ai-session-inner-tabs ai-session--current))
      (let ((buf (ai-session-inner-tab-buffer tab)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq header-line-format '(:eval (ai-session--render-inner-tabs)))))))))

(defun ai-session-add-inner-tab ()
  "Add a new inner tab to current session."
  (interactive)
  (when ai-session--current
    (let ((choice (completing-read "New tab type: "
                                   '("Terminal" "Open File" "Chat")
                                   nil t)))
      (pcase choice
        ("Terminal" (ai-session--add-terminal-tab))
        ("Open File" (ai-session--add-file-tab))
        ("Chat" (ai-session--add-chat-tab))))))

(defun ai-session--add-terminal-tab ()
  "Add a new terminal tab."
  (when ai-session--current
    (let* ((workspace (ai-session-workspace ai-session--current))
           (buf-name (format "*Terminal %d: %s*"
                             (1+ (length (seq-filter
                                          (lambda (t) (eq (ai-session-inner-tab-type t) 'terminal))
                                          (ai-session-inner-tabs ai-session--current))))
                             (ai-session-title ai-session--current)))
           (buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (unless (derived-mode-p 'vterm-mode 'term-mode)
          (if (fboundp 'vterm-mode)
              (progn
                (vterm-mode)
                (vterm-send-string (format "cd %s\n" (shell-quote-argument workspace))))
            (term "/bin/zsh"))))
      (ai-session--create-inner-tab ai-session--current "Terminal" 'terminal buf)
      (setf (ai-session-current-inner-tab ai-session--current)
            (1- (length (ai-session-inner-tabs ai-session--current))))
      (switch-to-buffer buf)
      (ai-session--update-all-header-lines))))

(defun ai-session--add-file-tab ()
  "Add a new file tab."
  (when ai-session--current
    (let* ((workspace (ai-session-workspace ai-session--current))
           (default-directory workspace)
           (file (read-file-name "Open file: " workspace)))
      (when file
        (let ((buf (find-file-noselect file)))
          (ai-session--create-inner-tab
           ai-session--current
           (file-name-nondirectory file)
           'file
           buf
           "")
          (setf (ai-session-current-inner-tab ai-session--current)
                (1- (length (ai-session-inner-tabs ai-session--current))))
          (switch-to-buffer buf)
          (ai-session--update-all-header-lines))))))

(defun ai-session--add-chat-tab ()
  "Add a new chat tab."
  (when ai-session--current
    (let* ((buf-name (format "*Chat: %s*" (ai-session-title ai-session--current)))
           (buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Chat (%s)\n" (ai-session-agent ai-session--current)))
          (insert "─────────────────────────\n\n")))
      (ai-session--create-inner-tab ai-session--current "Chat" 'chat buf)
      (setf (ai-session-current-inner-tab ai-session--current)
            (1- (length (ai-session-inner-tabs ai-session--current))))
      (switch-to-buffer buf)
      (ai-session--update-all-header-lines))))

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
         (session-id (ai-session-session-id session))
         (mcp-config-file (ai-session-mcp-config-file session))
         ;; Build MCP args based on agent type
         (mcp-args (when mcp-config-file
                     (pcase executable
                       ("claude" (format "--mcp-config %s" mcp-config-file))
                       (_ "")))))  ; gemini/codex use different config method
    (string-trim
     (if resume
         (format "%s %s %s %s"
                 executable
                 (or mcp-args "")
                 (mapconcat #'identity args " ")
                 (or session-id ""))
       (format "%s %s %s"
               executable
               (or mcp-args "")
               (mapconcat #'identity args " "))))))

(defun ai-session--setup-layout (session &optional resume)
  "Setup multi-pane layout for SESSION.
If RESUME is non-nil, resume existing session.
Layout:
+------------------+------------------+
|  [Inner Tabs: Terminal | Files...]  |
+------------------+------------------+
|                  |                  |
|    Main/Files    |    AI Chat       |
|                  |                  |
+------------------+------------------+
|                Terminal             |
+------------------------------------+"
  (delete-other-windows)

  ;; Initialize inner tabs list
  (setf (ai-session-inner-tabs session) nil)
  (setf (ai-session-current-inner-tab session) 0)

  (let* ((workspace (ai-session-workspace session))
         (main-buffer (get-buffer-create
                       (format "*Session: %s*" (ai-session-title session))))
         (agent-cmd (ai-session--build-agent-command session resume))
         (terminal-buffer nil))

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
        (insert "Press 't' to focus terminal\n")
        (insert "\nUse inner tabs above to switch between Terminal, Files, etc.\n")
        (insert "Click [+] to add new tabs.\n"))
      (ai-session-main-mode))

    ;; Create inner tab for main buffer
    (ai-session--create-inner-tab session "Main" 'main main-buffer "")

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
          (insert "(Chat interface placeholder)\n")))
      ;; Create inner tab for chat
      (ai-session--create-inner-tab session "Chat" 'chat chat-buffer ""))

    ;; Bottom window - Terminal with AI agent
    (other-window -1)  ; back to main
    (split-window-below (- (window-height) 15))
    (other-window 1)

    (let ((cmd (format "cd %s && %s" (shell-quote-argument workspace) agent-cmd)))
      (cond
       ((fboundp 'vterm)
        (vterm)
        (setq terminal-buffer (current-buffer))
        ;; Wait for vterm to initialize before sending command
        (run-at-time 0.1 nil
                     (lambda (c)
                       (when (and (boundp 'vterm--process)
                                  vterm--process)
                         (vterm-send-string (concat c "\n"))))
                     cmd))
       ((fboundp 'multi-term)
        (multi-term)
        (setq terminal-buffer (current-buffer))
        (term-send-raw-string (concat cmd "\n")))
       (t
        (term "/bin/zsh")
        (setq terminal-buffer (current-buffer))
        (term-send-raw-string (concat cmd "\n")))))

    ;; Create inner tab for terminal
    (when terminal-buffer
      (ai-session--create-inner-tab session "Terminal" 'terminal terminal-buffer ""))

    ;; Return to main window
    (other-window -1)
    (other-window -1)

    ;; Track buffers
    (setf (ai-session-buffers session)
          (list main-buffer
                (get-buffer (format "*AI Chat: %s*" (ai-session-title session)))
                terminal-buffer))

    ;; Setup header-line for inner tabs in all buffers
    (ai-session--update-all-header-lines)))

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
    ;; Inner tab navigation
    (define-key map (kbd "M-<right>") #'ai-session-next-inner-tab)
    (define-key map (kbd "M-<left>") #'ai-session-prev-inner-tab)
    (define-key map (kbd "+") #'ai-session-add-inner-tab)
    map)
  "Keymap for `ai-session-main-mode'.")

(defun ai-session-next-inner-tab ()
  "Switch to next inner tab."
  (interactive)
  (when ai-session--current
    (let* ((tabs (ai-session-inner-tabs ai-session--current))
           (current (or (ai-session-current-inner-tab ai-session--current) 0))
           (next (mod (1+ current) (length tabs))))
      (ai-session-switch-inner-tab next))))

(defun ai-session-prev-inner-tab ()
  "Switch to previous inner tab."
  (interactive)
  (when ai-session--current
    (let* ((tabs (ai-session-inner-tabs ai-session--current))
           (current (or (ai-session-current-inner-tab ai-session--current) 0))
           (prev (mod (1- current) (length tabs))))
      (ai-session-switch-inner-tab prev))))

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
         (reuse-tab ai-session--creating-from-wizard)
         (session (make-ai-session
                   :id id
                   :title title
                   :agent agent
                   :agent-config agent-config
                   :workspace (expand-file-name workspace)
                   :mcp-process nil
                   :mcp-config-file nil
                   :tab-index nil
                   :created-at (current-time)
                   :session-id nil
                   :buffers nil
                   :inner-tabs nil
                   :current-inner-tab 0)))

    ;; Clear the flag
    (setq ai-session--creating-from-wizard nil)

    ;; Register session
    (ai-session--register session)
    (setq ai-session--current session)

    ;; Setup tab-bar if not already
    (ai-session--setup-tab-bar)

    ;; Create tab (reuse current if coming from wizard's [+] button)
    (ai-session--create-tab session reuse-tab)

    ;; Generate MCP config file for this session
    (setf (ai-session-mcp-config-file session)
          (ai-session--generate-mcp-config session))

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
                   :mcp-config-file nil
                   :tab-index nil
                   :created-at (current-time)
                   :session-id session-id
                   :buffers nil
                   :inner-tabs nil
                   :current-inner-tab 0)))

    ;; Register session
    (ai-session--register session)
    (setq ai-session--current session)

    ;; Setup tab-bar if not already
    (ai-session--setup-tab-bar)

    ;; Create tab
    (ai-session--create-tab session)

    ;; Generate MCP config file for this session
    (setf (ai-session-mcp-config-file session)
          (ai-session--generate-mcp-config session))

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

        ;; Cleanup MCP config file
        (ai-session--cleanup-mcp-config sess)

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
