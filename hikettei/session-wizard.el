;;; session-wizard.el --- Beautiful Session Creation Wizard -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; A beautiful, interactive session creation wizard for AI development.
;;

;;; Code:

(require 'cl-lib)
(require 'json)

;;; ============================================================
;;; Faces
;;; ============================================================

(defgroup session-wizard nil
  "Session creation wizard faces and settings."
  :group 'faces)

(defface sw-header-face
  '((t :foreground "#bd93f9" :weight bold :height 1.2))
  "Face for the wizard header."
  :group 'session-wizard)

(defface sw-subheader-face
  '((t :foreground "#6272a4" :weight bold :height 1.1))
  "Face for subheaders."
  :group 'session-wizard)

(defface sw-border-face
  '((t :foreground "#6272a4"))
  "Face for borders and lines."
  :group 'session-wizard)

(defface sw-section-face
  '((t :foreground "#bd93f9" :background "#44475a" :weight bold :extend t))
  "Face for section headers (3D button look) - Purple on dark."
  :group 'session-wizard)

(defface sw-section-cyan-face
  '((t :foreground "#8be9fd" :background "#44475a" :weight bold :extend t))
  "Face for section headers - Cyan."
  :group 'session-wizard)

(defface sw-section-green-face
  '((t :foreground "#50fa7b" :background "#44475a" :weight bold :extend t))
  "Face for section headers - Green."
  :group 'session-wizard)

(defface sw-section-orange-face
  '((t :foreground "#ffb86c" :background "#44475a" :weight bold :extend t))
  "Face for section headers - Orange."
  :group 'session-wizard)

(defface sw-card-face
  '((t :background "#2d303d" :extend t))
  "Face for inactive card background."
  :group 'session-wizard)

(defface sw-card-active-face
  '((t :background "#44475a" :extend t))
  "Face for active/selected card background."
  :group 'session-wizard)

(defface sw-card-hover-face
  '((t :background "#383b4a" :extend t))
  "Face for hovered/selected but not focused card."
  :group 'session-wizard)

(defface sw-label-face
  '((t :foreground "#ff79c6" :weight bold))
  "Face for labels - Pink."
  :group 'session-wizard)

(defface sw-value-face
  '((t :foreground "#50fa7b"))
  "Face for values."
  :group 'session-wizard)

(defface sw-selected-face
  '((t :foreground "#282a36" :background "#bd93f9" :weight bold))
  "Face for selected items."
  :group 'session-wizard)

(defface sw-unselected-face
  '((t :foreground "#6272a4"))
  "Face for unselected items."
  :group 'session-wizard)

(defface sw-agent-claude-face
  '((t :foreground "#ff9f43" :weight bold))
  "Face for Claude agent."
  :group 'session-wizard)

(defface sw-agent-gemini-face
  '((t :foreground "#48dbfb" :weight bold))
  "Face for Gemini agent."
  :group 'session-wizard)

(defface sw-agent-codex-face
  '((t :foreground "#0abf53" :weight bold))
  "Face for Codex agent."
  :group 'session-wizard)

(defface sw-agent-none-face
  '((t :foreground "#6272a4" :weight bold))
  "Face for No Agent option."
  :group 'session-wizard)

(defface sw-button-face
  '((t :foreground "#50fa7b" :background "#44475a" :box (:line-width -2 :color "#50fa7b") :weight bold))
  "Face for buttons."
  :group 'session-wizard)

(defface sw-hint-face
  '((t :foreground "#6272a4" :slant italic))
  "Face for hints."
  :group 'session-wizard)

(defface sw-input-face
  '((t :foreground "#f1fa8c" :background "#44475a" :box (:line-width 1 :color "#6272a4")))
  "Face for input fields."
  :group 'session-wizard)

;;; ============================================================
;;; Variables
;;; ============================================================

(defvar sw--buffer-name "*Session Wizard*"
  "Name of the session wizard buffer.")

(defcustom sw-agents-directory "~/.emacs.d/agents/"
  "Directory containing agent configuration files."
  :type 'string
  :group 'session-wizard)

(defvar sw--agents nil
  "Available AI agents loaded from config files.")

;;; ============================================================
;;; JSON Config Parser
;;; ============================================================

(defun sw--parse-config-file (file)
  "Parse a JSON config FILE and return an alist."
  (when (file-exists-p file)
    (json-read-file file)))

(defun sw--color-to-face (color-hex)
  "Convert COLOR-HEX string to a face symbol or create inline face."
  (cond
   ((string= color-hex "#ff9f43") 'sw-agent-claude-face)
   ((string= color-hex "#48dbfb") 'sw-agent-gemini-face)
   ((string= color-hex "#0abf53") 'sw-agent-codex-face)
   ((string= color-hex "#6272a4") 'sw-agent-none-face)
   (t `(:foreground ,color-hex :weight bold))))

(defun sw--load-agent-from-json (file)
  "Load agent configuration from JSON FILE."
  (let ((config (sw--parse-config-file file)))
    (when config
      (let* ((agent-section (cdr (assoc 'agent config)))
             (command-section (cdr (assoc 'command config)))
             (mcp-section (cdr (assoc 'mcp config)))
             (name (cdr (assoc 'name agent-section)))
             (icon (cdr (assoc 'icon agent-section)))
             (color (cdr (assoc 'color agent-section)))
             (desc (cdr (assoc 'description agent-section)))
             (executable (cdr (assoc 'executable command-section)))
             (args (cdr (assoc 'args command-section)))
             (resume-args (cdr (assoc 'resume_args command-section)))
             (mcp-enabled (cdr (assoc 'enabled mcp-section))))
        (when (and name icon desc)
          (cons name (list :icon icon
                           :color (sw--color-to-face color)
                           :desc desc
                           :executable executable
                           :args (append args nil)  ; convert vector to list
                           :resume-args (append resume-args nil)
                           :mcp-enabled mcp-enabled
                           :config-file file)))))))

(defconst sw--no-agent-entry
  '("None" :icon "📝"
           :color sw-agent-none-face
           :desc "No AI Agent - Use as a normal text editor"
           :executable nil
           :args nil
           :resume-args nil
           :mcp-enabled nil
           :config-file nil)
  "Special entry for 'No AI Agent' option.")

(defun sw--load-agents ()
  "Load all agent configurations from `sw-agents-directory'."
  (let ((dir (expand-file-name sw-agents-directory)))
    (unless (file-directory-p dir)
      (error "Agent configuration directory not found: %s. Run 'make install' first" dir))
    (let ((files (directory-files dir t "\\.json$")))
      (unless files
        (error "No agent configuration files (.json) found in %s" dir))
      ;; Load agents from JSON files, then add "None" option at the end
      (setq sw--agents
            (append (delq nil (mapcar #'sw--load-agent-from-json files))
                    (list sw--no-agent-entry)))))
  sw--agents)

(defvar sw--current-agent 0
  "Currently selected agent index.")

(defvar sw--session-title ""
  "Current session title.")

(defvar sw--workspace ""
  "Current workspace path.")

(defvar sw--current-field 'agent
  "Currently focused field: 'agent, 'title, 'workspace, or 'buttons.")

(defvar sw--past-sessions nil
  "List of past sessions for the current workspace.")

(defvar sw--selected-past-session 0
  "Currently selected past session index.")

(defvar sw--found-workspaces nil
  "List of workspaces with session history found by search.")

(defvar sw--all-sessions nil
  "Flat list of all sessions from all found workspaces.
Each entry is (workspace . session-data).")

(defvar sw--selected-session-idx 0
  "Currently selected session index in the flat list.")

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun sw--is-no-agent-selected ()
  "Return t if 'None' agent is currently selected."
  (let* ((agent-data (nth sw--current-agent sw--agents))
         (agent-name (car agent-data)))
    (string= agent-name "None")))

;;; ============================================================
;;; Workspace Search
;;; ============================================================

(defcustom sw-workspace-search-depth 3
  "Maximum depth to search for .hikettei directories."
  :type 'integer
  :group 'session-wizard)

(defcustom sw-workspace-search-paths '("~/" "~/Projects" "~/Programming" "~/work" "~/dev")
  "Default paths to search for workspaces with sessions."
  :type '(repeat string)
  :group 'session-wizard)

(defun sw--find-hikettei-dirs (dir depth)
  "Find all directories containing .hikettei/sessions.json under DIR up to DEPTH."
  (let ((results '()))
    (when (and (file-directory-p dir)
               (> depth 0)
               (not (string-prefix-p "." (file-name-nondirectory dir))))
      ;; Check if this directory has .hikettei/sessions.json
      (let ((sessions-file (expand-file-name ".hikettei/sessions.json" dir)))
        (when (file-exists-p sessions-file)
          (push dir results)))
      ;; Recurse into subdirectories
      (when (> depth 1)
        (condition-case nil
            (dolist (subdir (directory-files dir t "^[^.]" t))
              (when (and (file-directory-p subdir)
                         (not (member (file-name-nondirectory subdir)
                                      '("node_modules" ".git" "venv" "__pycache__"
                                        ".venv" "target" "build" "dist" ".cache"))))
                (setq results (append results (sw--find-hikettei-dirs subdir (1- depth))))))
          (error nil))))
    results))

(defun sw--build-all-sessions ()
  "Build flat list of all sessions from found workspaces."
  (setq sw--all-sessions nil)
  (dolist (workspace sw--found-workspaces)
    (when (fboundp 'ai-session--load-workspace-sessions)
      (let ((sessions (ai-session--load-workspace-sessions workspace)))
        (dolist (session sessions)
          (push (cons workspace session) sw--all-sessions)))))
  ;; Sort by last_accessed descending
  (setq sw--all-sessions
        (sort sw--all-sessions
              (lambda (a b)
                (string> (or (cdr (assoc 'last_accessed (cdr a))) "")
                         (or (cdr (assoc 'last_accessed (cdr b))) "")))))
  (setq sw--selected-session-idx 0))

(defun sw--search-workspaces-from-pwd ()
  "Search for workspaces with sessions starting from current directory."
  (interactive)
  (message "Searching...")
  (let* ((start-dir (or default-directory "~/"))
         (found (sw--find-hikettei-dirs start-dir sw-workspace-search-depth)))
    (setq sw--found-workspaces found)
    (sw--build-all-sessions)
    (if sw--all-sessions
        (message "Found %d session(s)" (length sw--all-sessions))
      (message "No sessions found"))
    (sw--refresh-info-buffer)))

(defun sw--search-workspaces-global ()
  "Search for workspaces with sessions in common project directories."
  (interactive)
  (message "Searching globally...")
  (let ((all-found '()))
    (dolist (path sw-workspace-search-paths)
      (let ((expanded (expand-file-name path)))
        (when (file-directory-p expanded)
          (setq all-found (append all-found
                                  (sw--find-hikettei-dirs expanded sw-workspace-search-depth))))))
    (setq sw--found-workspaces (delete-dups all-found))
    (sw--build-all-sessions)
    (if sw--all-sessions
        (message "Found %d session(s)" (length sw--all-sessions))
      (message "No sessions found"))
    (sw--refresh-info-buffer)))

(defun sw--next-session ()
  "Select the next session."
  (interactive)
  (when sw--all-sessions
    (setq sw--selected-session-idx
          (mod (1+ sw--selected-session-idx) (length sw--all-sessions)))
    (sw--refresh-info-buffer)))

(defun sw--prev-session ()
  "Select the previous session."
  (interactive)
  (when sw--all-sessions
    (setq sw--selected-session-idx
          (mod (1- sw--selected-session-idx) (length sw--all-sessions)))
    (sw--refresh-info-buffer)))

(defun sw--restore-selected-session ()
  "Restore the currently selected session."
  (interactive)
  (when (and sw--all-sessions
             (>= sw--selected-session-idx 0)
             (< sw--selected-session-idx (length sw--all-sessions)))
    (let* ((entry (nth sw--selected-session-idx sw--all-sessions))
           (workspace (car entry))
           (session-data (cdr entry))
           (id (cdr (assoc 'id session-data)))
           (agent-name (cdr (assoc 'agent session-data)))
           (title (cdr (assoc 'title session-data)))
           (session-id (cdr (assoc 'session_id session-data)))
           (agent-data (assoc agent-name sw--agents))
           (agent-props (cdr agent-data))
           (agent-config (when agent-props
                           (list :executable (plist-get agent-props :executable)
                                 :args (plist-get agent-props :args)
                                 :resume-args (plist-get agent-props :resume-args)))))
      (sw--stop-nyan-animation)
      (kill-buffer sw--buffer-name)
      (when (get-buffer "*Restore Session*")
        (kill-buffer "*Restore Session*"))
      (if (and (fboundp 'ai-session-resume) agent-config)
          (ai-session-resume :id id
                             :agent agent-name
                             :agent-config agent-config
                             :title title
                             :workspace workspace
                             :session-id session-id)
        (message "Cannot resume: agent config not found for %s" agent-name)))))

(defun sw--browse-workspace ()
  "Browse and select workspace directory using completing-read."
  (interactive)
  (let ((workspace (read-directory-name "Select Workspace: "
                                        default-directory nil t)))
    (setq sw--workspace workspace)
    (sw--load-past-sessions)
    (sw--refresh-info-buffer)
    (sw--render)))

;;; ============================================================
;;; ASCII Art
;;; ============================================================

(defconst sw--header-title "Create Workspace"
  "Simple header title.")

(defconst sw--divider
  "════════════════════════════════════════════════════════════════════════════════════════════════"
  "Divider line.")

;;; ============================================================
;;; Rendering Functions
;;; ============================================================

(defconst sw--line-width 83
  "Standard width for horizontal lines.")

(defvar sw--nyan-dir "~/.emacs.d/layers/+themes/colors/local/nyan-mode/img/"
  "Directory containing nyan-mode images.")

(defvar sw--nyan-frame 1
  "Current nyan animation frame.")

(defvar sw--nyan-timer nil
  "Timer for nyan animation.")

(defvar sw--nyan-marker nil
  "Marker for nyan cat position.")

(defconst sw--nyan-frame-count 6
  "Number of nyan animation frames.")

(defun sw--insert-nyan-image (name)
  "Insert nyan-mode image NAME if available."
  (let ((file (expand-file-name (concat name ".xpm") sw--nyan-dir)))
    (when (and (display-graphic-p) (file-exists-p file))
      (insert-image (create-image file 'xpm nil :ascent 'center)))))

(defun sw--nyan-animate ()
  "Animate the nyan cat by cycling frames."
  (when (and sw--nyan-marker
             (buffer-live-p (marker-buffer sw--nyan-marker))
             (get-buffer sw--buffer-name))
    (with-current-buffer sw--buffer-name
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char sw--nyan-marker)
          ;; Delete old image (1 char with display property)
          (delete-char 1)
          ;; Insert new frame
          (setq sw--nyan-frame (1+ (mod sw--nyan-frame sw--nyan-frame-count)))
          (let ((file (expand-file-name (format "nyan-frame-%d.xpm" sw--nyan-frame) sw--nyan-dir)))
            (when (file-exists-p file)
              (insert-image (create-image file 'xpm nil :ascent 'center)))))))))

(defun sw--start-nyan-animation ()
  "Start nyan cat animation."
  (sw--stop-nyan-animation)
  (when (display-graphic-p)
    (setq sw--nyan-timer (run-at-time 0.1 0.1 #'sw--nyan-animate))))

(defun sw--stop-nyan-animation ()
  "Stop nyan cat animation."
  (when sw--nyan-timer
    (cancel-timer sw--nyan-timer)
    (setq sw--nyan-timer nil)))

(defun sw--get-progress ()
  "Get current progress (0-4) based on field and filled values."
  (let ((base (pcase sw--current-field
                ('agent 0)
                ('title 1)
                ('workspace 2)
                ('buttons 3))))
    (+ base
       (if (not (string-empty-p sw--session-title)) 0.5 0)
       (if (not (string-empty-p sw--workspace)) 0.5 0))))

(defun sw--render-header ()
  "Render the header with 3D button style and Nyan Cat."
  (insert "\n")
  ;; Calculate rainbow length based on progress (min 1, max ~90)
  (let* ((progress (sw--get-progress))
         (rainbow-count (+ 1 (floor (* progress 22)))))
    ;; Try to insert nyan-mode images, fallback to ASCII
    (if (and (display-graphic-p) (file-exists-p (expand-file-name "rainbow.xpm" sw--nyan-dir)))
        (progn
          ;; Rainbow trail - length based on progress
          (dotimes (_ rainbow-count)
            (sw--insert-nyan-image "rainbow"))
          ;; Mark position for animated nyan cat (no space)
          (setq sw--nyan-marker (point-marker))
          ;; Insert first frame
          (let ((file (expand-file-name "nyan-frame-1.xpm" sw--nyan-dir)))
            (when (file-exists-p file)
              (insert-image (create-image file 'xpm nil :ascent 'center)))))
      ;; Fallback to ASCII rainbow - also dynamic
      (let ((colors '("#ff5555" "#ffb86c" "#f1fa8c" "#50fa7b" "#8be9fd" "#bd93f9")))
        (dotimes (i (min rainbow-count (length colors)))
          (insert (propertize "═══" 'face `(:foreground ,(nth (mod i 6) colors))))))
      (insert (propertize "(=^･ω･^=)" 'face '(:foreground "#ff79c6")))))
  (insert "\n")
  (insert (propertize "   " 'face 'default))
  (insert (propertize (format "%-77s" sw--header-title) 'face 'sw-section-face))
  (insert "\n")
  (insert (propertize (make-string sw--line-width ?─) 'face 'sw-border-face))
  (insert "\n\n"))

(defun sw--render-divider ()
  "Render a divider line."
  (insert (propertize (make-string sw--line-width ?─) 'face 'sw-border-face))
  (insert "\n\n"))

(defun sw--render-agent-card (agent index)
  "Render an agent card for AGENT at INDEX."
  (let* ((name (car agent))
         (props (cdr agent))
         (icon (plist-get props :icon))
         (color (plist-get props :color))
         (desc (plist-get props :desc))
         (selected (= index sw--current-agent))
         (focused (eq sw--current-field 'agent))
         (active (and selected focused))
         (bg-face (cond (active 'sw-card-active-face)
                        (selected 'sw-card-hover-face)
                        (t 'sw-card-face)))
         (name-face (cond (active color)
                          (selected color)
                          (t 'sw-unselected-face)))
         (desc-face (if (or active selected) 'sw-value-face 'sw-unselected-face))
         (indicator (if active " ▶ " "   "))
         (content-width 77))
    ;; Card with background - name line
    (insert (propertize indicator 'face (if active 'sw-selected-face 'default)))
    (let* ((name-str (format " %s  %s" icon name))
           (padding (make-string (max 0 (- content-width (string-width name-str))) ? )))
      (insert (propertize (concat name-str padding) 'face `(:inherit (,name-face ,bg-face)))))
    (insert "\n")
    ;; Description line
    (insert "   ")
    (let* ((desc-str (format "    %s" desc))
           (padding (make-string (max 0 (- content-width (string-width desc-str))) ? )))
      (insert (propertize (concat desc-str padding) 'face `(:inherit (,desc-face ,bg-face)))))
    (insert "\n")
    ;; Bottom separator
    (insert "   ")
    (insert (propertize (make-string (- sw--line-width 3) ?─) 'face 'sw-border-face))
    (insert "\n")))

(defun sw--render-agents ()
  "Render the agent selection section."
  (insert (propertize (format "   %-77s" "Select AI Agent") 'face 'sw-section-cyan-face))
  (insert "\n")
  (if (eq sw--current-field 'agent)
      (if (sw--is-no-agent-selected)
          (insert (propertize "   ↑/↓ to select, TAB/RET to start editor" 'face 'sw-hint-face))
        (insert (propertize "   ↑/↓ to select, TAB to continue" 'face 'sw-hint-face)))
    (insert (propertize "   TAB to focus" 'face 'sw-hint-face)))
  (insert "\n\n")
  ;; Top line before first card
  (insert "   ")
  (insert (propertize (make-string (- sw--line-width 3) ?─) 'face 'sw-border-face))
  (insert "\n")

  (let ((index 0))
    (dolist (agent sw--agents)
      (sw--render-agent-card agent index)
      (setq index (1+ index))))
  (insert "\n"))

(defun sw--render-input-field (label value field-type placeholder)
  "Render an input field with LABEL, VALUE, FIELD-TYPE and PLACEHOLDER."
  (let* ((focused (eq sw--current-field field-type))
         (content (if (string-empty-p value) placeholder value))
         (bg-face (if focused 'sw-card-active-face 'sw-card-face))
         (content-face (cond ((string-empty-p value) 'sw-hint-face)
                             (t 'sw-value-face)))
         (indicator (if focused " ▶ " "   "))
         (input-width 64))
    ;; Label line with indicator
    (insert (propertize indicator 'face (if focused 'sw-selected-face 'default)))
    (insert (propertize (format "%-12s" label) 'face 'sw-label-face))
    ;; Input area with background
    (let* ((padding (make-string (max 0 (- input-width (length content))) ? )))
      (insert (propertize (concat " " content padding) 'face `(:inherit (,content-face ,bg-face)))))
    (when focused
      (insert (propertize "  [RET]" 'face 'sw-hint-face)))
    (insert "\n")))

(defun sw--render-inputs ()
  "Render the input fields section."
  (insert (propertize (format "   %-77s" "Session Details") 'face 'sw-section-green-face))
  (insert "\n\n")

  (sw--render-input-field "Title:" sw--session-title 'title "Enter session title...")
  (insert "\n")
  (sw--render-input-field "Workspace:" sw--workspace 'workspace "Select workspace directory...")
  (insert "\n")
  (insert (propertize (make-string sw--line-width ?─) 'face 'sw-border-face))
  (insert "\n\n"))

(defun sw--render-preview ()
  "Render the session preview."
  (when (and (not (string-empty-p sw--session-title))
             (not (string-empty-p sw--workspace)))
    (insert (propertize (format "   %-77s" "Preview") 'face 'sw-section-orange-face))
    (insert "\n\n")
    (let* ((agent-name (car (nth sw--current-agent sw--agents)))
           (agent-props (cdr (nth sw--current-agent sw--agents)))
           (agent-icon (plist-get agent-props :icon))
           (agent-color (plist-get agent-props :color))
           (preview-width 71))
      ;; Session line - purple label, cyan value
      (insert "   ")
      (insert (propertize " Session: " 'face '(:foreground "#bd93f9" :background "#2d303d")))
      (let ((padding (make-string (max 0 (- preview-width 10 (length sw--session-title))) ? )))
        (insert (propertize (concat sw--session-title padding) 'face '(:foreground "#8be9fd" :background "#2d303d"))))
      (insert "\n")
      ;; Agent line - pink label, agent color value
      (insert "   ")
      (insert (propertize " Agent:   " 'face '(:foreground "#ff79c6" :background "#2d303d")))
      (insert (propertize (format "%s " agent-icon) 'face '(:background "#2d303d")))
      (let ((padding (make-string (max 0 (- preview-width 12 (string-width agent-name))) ? )))
        (insert (propertize (concat agent-name padding) 'face `(:inherit ,agent-color :background "#2d303d"))))
      (insert "\n")
      ;; Path line - green label, yellow value
      (insert "   ")
      (insert (propertize " Path:    " 'face '(:foreground "#50fa7b" :background "#2d303d")))
      (let* ((path-display (truncate-string-to-width sw--workspace (- preview-width 10) 0 nil "..."))
             (padding (make-string (max 0 (- preview-width 10 (length path-display))) ? )))
        (insert (propertize (concat path-display padding) 'face '(:foreground "#f1fa8c" :background "#2d303d"))))
      (insert "\n"))
    (insert "   ")
    (insert (propertize (make-string (- sw--line-width 3) ?─) 'face 'sw-border-face))
    (insert "\n\n")))

(defun sw--render-buttons ()
  "Render the action buttons."
  (let ((can-create (and (not (string-empty-p sw--session-title))
                         (not (string-empty-p sw--workspace)))))
    ;; Center the button with background
    (insert "                         ")
    (if can-create
        (insert (propertize "     Create Session     " 'face 'sw-button-face))
      (insert (propertize "     Create Session     " 'face 'sw-unselected-face)))
    (insert "\n\n")))

(defun sw--render-help ()
  "Render the help section."
  (insert (propertize (make-string sw--line-width ?─) 'face 'sw-border-face))
  (insert "\n")
  (insert (propertize "   " 'face 'default))
  (insert (propertize "TAB" 'face 'sw-value-face))
  (insert (propertize " next  " 'face 'sw-hint-face))
  (insert (propertize "S-TAB" 'face 'sw-value-face))
  (insert (propertize " prev  " 'face 'sw-hint-face))
  (insert (propertize "↑/↓" 'face 'sw-value-face))
  (insert (propertize " select agent  " 'face 'sw-hint-face))
  (insert (propertize "RET" 'face 'sw-value-face))
  (insert (propertize " edit/confirm  " 'face 'sw-hint-face))
  (insert (propertize "q" 'face 'sw-value-face))
  (insert (propertize " quit\n" 'face 'sw-hint-face)))

(defun sw--render ()
  "Render the entire wizard."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (sw--render-header)
    (sw--render-agents)
    (sw--render-inputs)
    (sw--render-preview)
    (sw--render-buttons)
    (sw--render-help)
    (goto-char (point-min))))

;;; ============================================================
;;; Navigation
;;; ============================================================

(defun sw--start-editor-mode ()
  "Start Emacs as a normal text editor without AI agent."
  (sw--stop-nyan-animation)
  (kill-buffer sw--buffer-name)
  (when (get-buffer "*Restore Session*")
    (kill-buffer "*Restore Session*"))

  ;; Rename tab if we came from [+] button
  (when (and (boundp 'ai-session--creating-from-wizard)
             ai-session--creating-from-wizard)
    (setq ai-session--creating-from-wizard nil)
    (tab-bar-rename-tab "📝 Editor"))

  (delete-other-windows)

  ;; Setup layout: neotree + main editor + terminal at bottom
  ;; Show neotree on the left
  (when (fboundp 'neotree-show)
    (neotree-show)
    (setq neo-window-width 40))

  ;; Split for terminal at bottom (4/5 editor, 1/5 terminal)
  (other-window 1)  ; Move to main window (right of neotree)
  (let ((height (/ (window-body-height) 5)))
    (split-window-vertically (* 4 height)))

  ;; Open terminal in bottom window
  (other-window 1)
  (cond
   ((fboundp 'vterm) (vterm))
   ((fboundp 'multi-term) (multi-term))
   (t (term "/bin/zsh")))

  ;; Return to main editor window
  (other-window -1)
  (switch-to-buffer "*scratch*")
  (message "Editor mode - No AI agent"))

(defun sw--next-field ()
  "Move to the next field."
  (interactive)
  ;; If "None" is selected and we're on agent field, skip to editor mode
  (if (and (eq sw--current-field 'agent) (sw--is-no-agent-selected))
      (sw--start-editor-mode)
    (setq sw--current-field
          (pcase sw--current-field
            ('agent 'title)
            ('title 'workspace)
            ('workspace 'buttons)
            ('buttons 'agent)))
    (sw--render)))

(defun sw--prev-field ()
  "Move to the previous field."
  (interactive)
  (setq sw--current-field
        (pcase sw--current-field
          ('agent 'buttons)
          ('title 'agent)
          ('workspace 'title)
          ('buttons 'workspace)))
  (sw--render))

(defun sw--next-agent ()
  "Select the next agent."
  (interactive)
  (when (eq sw--current-field 'agent)
    (setq sw--current-agent (mod (1+ sw--current-agent) (length sw--agents)))
    (sw--render)))

(defun sw--prev-agent ()
  "Select the previous agent."
  (interactive)
  (when (eq sw--current-field 'agent)
    (setq sw--current-agent (mod (1- sw--current-agent) (length sw--agents)))
    (sw--render)))

(defun sw--edit-current-field ()
  "Edit the currently focused field."
  (interactive)
  (pcase sw--current-field
    ('agent
     ;; If "None" is selected, start editor mode
     (when (sw--is-no-agent-selected)
       (sw--start-editor-mode)))
    ('title
     (setq sw--session-title
           (read-string "Session Title: " sw--session-title))
     (sw--render))
    ('workspace
     (setq sw--workspace
           (read-directory-name "Workspace Directory: "
                                (or (and (not (string-empty-p sw--workspace)) sw--workspace)
                                    default-directory)))
     (sw--load-past-sessions)
     (sw--refresh-info-buffer)
     (sw--render))
    ('buttons
     (sw--create-session))))

;;; ============================================================
;;; Actions
;;; ============================================================

(defun sw--create-session ()
  "Create the session with current settings."
  (interactive)
  (if (or (string-empty-p sw--session-title)
          (string-empty-p sw--workspace))
      (message "Please fill in all fields!")
    (let* ((agent-data (nth sw--current-agent sw--agents))
           (agent (car agent-data))
           (agent-props (cdr agent-data))
           (agent-config (list :executable (plist-get agent-props :executable)
                               :args (plist-get agent-props :args)
                               :resume-args (plist-get agent-props :resume-args)))
           (title sw--session-title)
           (workspace sw--workspace))
      (sw--stop-nyan-animation)
      (kill-buffer sw--buffer-name)
      (when (get-buffer "*Restore Session*")
        (kill-buffer "*Restore Session*"))
      ;; Call the session creation function from mcp-session.el
      (if (fboundp 'ai-session-create)
          (ai-session-create :agent agent
                             :agent-config agent-config
                             :title title
                             :workspace workspace)
        (message "Session: %s │ Agent: %s │ Workspace: %s" title agent workspace)))))

(defun sw--resume-session ()
  "Resume the selected past session."
  (interactive)
  (when (and sw--past-sessions
             (>= sw--selected-past-session 0)
             (< sw--selected-past-session (length sw--past-sessions)))
    (let* ((session-data (nth sw--selected-past-session sw--past-sessions))
           (id (cdr (assoc 'id session-data)))
           (agent-name (cdr (assoc 'agent session-data)))
           (title (cdr (assoc 'title session-data)))
           (session-id (cdr (assoc 'session_id session-data)))
           ;; Find agent config by name
           (agent-data (assoc agent-name sw--agents))
           (agent-props (cdr agent-data))
           (agent-config (when agent-props
                           (list :executable (plist-get agent-props :executable)
                                 :args (plist-get agent-props :args)
                                 :resume-args (plist-get agent-props :resume-args)))))
      (sw--stop-nyan-animation)
      (kill-buffer sw--buffer-name)
      (when (get-buffer "*Restore Session*")
        (kill-buffer "*Restore Session*"))
      (if (and (fboundp 'ai-session-resume) agent-config)
          (ai-session-resume :id id
                             :agent agent-name
                             :agent-config agent-config
                             :title title
                             :workspace sw--workspace
                             :session-id session-id)
        (message "Cannot resume: agent config not found for %s" agent-name)))))

(defun sw--cancel ()
  "Cancel and close the wizard."
  (interactive)
  (sw--stop-nyan-animation)
  (kill-buffer sw--buffer-name)
  (when (get-buffer "*Restore Session*")
    (kill-buffer "*Restore Session*"))
  ;; If we came from [+] button, close the "New Session" tab
  (when (and (boundp 'ai-session--creating-from-wizard)
             ai-session--creating-from-wizard)
    (setq ai-session--creating-from-wizard nil)
    (tab-bar-close-tab))
  (message "Session creation cancelled."))

(defun sw--next-past-session ()
  "Select the next past session."
  (interactive)
  (when sw--past-sessions
    (setq sw--selected-past-session
          (mod (1+ sw--selected-past-session) (length sw--past-sessions)))
    (sw--refresh-info-buffer)))

(defun sw--prev-past-session ()
  "Select the previous past session."
  (interactive)
  (when sw--past-sessions
    (setq sw--selected-past-session
          (mod (1- sw--selected-past-session) (length sw--past-sessions)))
    (sw--refresh-info-buffer)))

;;; ============================================================
;;; Major Mode
;;; ============================================================

;; Info buffer mode - simple navigation
(defvar sw-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") #'sw--prev-session)
    (define-key map (kbd "<down>") #'sw--next-session)
    (define-key map (kbd "k") #'sw--prev-session)
    (define-key map (kbd "j") #'sw--next-session)
    (define-key map (kbd "RET") #'sw--restore-selected-session)
    (define-key map (kbd "<return>") #'sw--restore-selected-session)
    (define-key map (kbd "s") #'sw--search-workspaces-from-pwd)
    (define-key map (kbd "S") #'sw--search-workspaces-global)
    (define-key map (kbd "q") #'sw--cancel)
    map)
  "Keymap for session info mode.")

(define-derived-mode sw-info-mode special-mode "RestoreSession"
  "Major mode for the restore session buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (display-line-numbers-mode -1))

;; Main wizard mode (left pane - create new session)
(defvar sw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'sw--next-field)
    (define-key map (kbd "<tab>") #'sw--next-field)
    (define-key map (kbd "<backtab>") #'sw--prev-field)
    (define-key map (kbd "S-TAB") #'sw--prev-field)
    (define-key map (kbd "<up>") #'sw--prev-agent)
    (define-key map (kbd "<down>") #'sw--next-agent)
    (define-key map (kbd "k") #'sw--prev-agent)
    (define-key map (kbd "j") #'sw--next-agent)
    (define-key map (kbd "RET") #'sw--edit-current-field)
    (define-key map (kbd "<return>") #'sw--edit-current-field)
    (define-key map (kbd "q") #'sw--cancel)
    (define-key map (kbd "C-c C-c") #'sw--create-session)
    map)
  "Keymap for session wizard mode.")

(define-derived-mode sw-mode special-mode "SessionWizard"
  "Major mode for the session creation wizard."
  (setq buffer-read-only t)
  (setq cursor-type nil)
  (setq truncate-lines t)
  (setq line-spacing 0)
  (display-line-numbers-mode -1)
  ;; Stop animation when buffer is killed
  (add-hook 'kill-buffer-hook #'sw--stop-nyan-animation nil t))

;;; ============================================================
;;; Public API
;;; ============================================================

;;;###autoload
(defvar sw--untitled-counter 1
  "Counter for untitled sessions.")

(defun session-wizard ()
  "Open the session creation wizard."
  (interactive)
  ;; Stop any existing animation
  (sw--stop-nyan-animation)
  ;; Load agents from config files
  (sw--load-agents)
  ;; Reset state with defaults
  (setq sw--current-agent 0)
  (setq sw--session-title (format "Untitled %d" sw--untitled-counter))
  (setq sw--untitled-counter (1+ sw--untitled-counter))
  (setq sw--workspace (expand-file-name default-directory))
  (setq sw--current-field 'agent)
  (setq sw--nyan-frame 1)
  (setq sw--past-sessions nil)
  (setq sw--selected-past-session 0)
  (setq sw--found-workspaces nil)
  (setq sw--all-sessions nil)
  (setq sw--selected-session-idx 0)

  ;; Delete other windows and split
  (delete-other-windows)

  ;; Create and setup buffer
  (let ((buf (get-buffer-create sw--buffer-name)))
    (switch-to-buffer buf)
    (sw-mode)
    (sw--render)
    ;; Start nyan animation
    (sw--start-nyan-animation))

  ;; Split window and show info on right
  (split-window-right)
  (other-window 1)
  (switch-to-buffer (sw--create-info-buffer))
  (other-window -1))

(defun sw--load-past-sessions ()
  "Load past sessions for current workspace."
  (when (and (not (string-empty-p sw--workspace))
             (fboundp 'ai-session--load-workspace-sessions))
    (setq sw--past-sessions
          (ai-session--load-workspace-sessions sw--workspace))
    (setq sw--selected-past-session 0)))

(defun sw--create-info-buffer ()
  "Create an info buffer for the right pane."
  (let ((buf (get-buffer-create "*Restore Session*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n")
        (insert (propertize "   Restore Session\n" 'face 'sw-section-face))
        (insert (propertize (make-string 40 ?─) 'face 'sw-border-face))
        (insert "\n\n")

        (if sw--all-sessions
            ;; Show flat list of all sessions
            (let ((idx 0))
              (dolist (entry sw--all-sessions)
                (let* ((workspace (car entry))
                       (session (cdr entry))
                       (selected (= idx sw--selected-session-idx))
                       (title (cdr (assoc 'title session)))
                       (agent (cdr (assoc 'agent session)))
                       (indicator (if selected " ▶ " "   "))
                       (title-face (if selected 'sw-value-face 'sw-hint-face))
                       (path-display (abbreviate-file-name workspace)))
                  (insert (propertize indicator 'face (if selected 'sw-selected-face 'default)))
                  (insert (propertize (format "%s" title) 'face title-face))
                  (insert (propertize (format " (%s)\n" agent) 'face 'sw-hint-face))
                  (insert (propertize (format "      %s\n" path-display)
                                      'face '(:foreground "#6272a4"))))
                (setq idx (1+ idx))))
          ;; No sessions - show search hint
          (insert (propertize "   No sessions found.\n\n" 'face 'sw-hint-face))
          (insert (propertize "   Press 's' to search from pwd\n" 'face 'sw-hint-face))
          (insert (propertize "   Press 'S' for global search\n" 'face 'sw-hint-face)))

        (insert "\n")
        (insert (propertize (make-string 40 ?─) 'face 'sw-border-face))
        (insert "\n")
        (insert (propertize "   ↑/↓" 'face 'sw-value-face))
        (insert (propertize " Navigate  " 'face 'sw-hint-face))
        (insert (propertize "RET" 'face 'sw-value-face))
        (insert (propertize " Restore\n" 'face 'sw-hint-face))
        (insert (propertize "   s" 'face 'sw-value-face))
        (insert (propertize " Search    " 'face 'sw-hint-face))
        (insert (propertize "S" 'face 'sw-value-face))
        (insert (propertize " Global search\n" 'face 'sw-hint-face)))

      (sw-info-mode)
      (goto-char (point-min)))
    buf))

(defun sw--refresh-info-buffer ()
  "Refresh the info buffer with updated past sessions."
  (when (get-buffer "*Restore Session*")
    (sw--create-info-buffer)))

;; Set as startup screen
(setq initial-buffer-choice #'session-wizard)

(provide 'session-wizard)

;;; session-wizard.el ends here
