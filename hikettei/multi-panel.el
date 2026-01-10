;;; multi-panel.el --- Multi-Panel WorkArea System -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; Multi-panel system for AI-assisted development.
;;
;; Layout (fixed structure):
;; +--------+----------------------------------+
;; |        | [Autopilot] [Terminal] [...]     |
;; |        +----------------------------------+
;; |        |                                  |
;; | Neo    |           WorkArea               |
;; | Tree   |    (can contain child panels)    |
;; |        |                                  |
;; +--------+----------------------------------+
;;
;; - NeoTree: Toggle with C-x j n (left side window)
;;

;;; Code:

(require 'cl-lib)

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup multi-panel nil
  "Multi-panel WorkArea system."
  :group 'tools
  :prefix "mp-")

(defcustom mp-neotree-width 30
  "NeoTree width in columns."
  :type 'integer
  :group 'multi-panel)

(defvar mp--source-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory where multi-panel.el is located.")

(defcustom mp-panel-directory
  (expand-file-name "panel" mp--source-directory)
  "Directory containing panel definitions."
  :type 'directory
  :group 'multi-panel)

;;; ============================================================
;;; Faces
;;; ============================================================

(defface mp-feat-tab-active-face
  '((t :foreground "#50fa7b" :background "#44475a" :weight bold))
  "Face for active feat tab."
  :group 'multi-panel)

(defface mp-feat-tab-inactive-face
  '((t :foreground "#6272a4" :background "#282a36"))
  "Face for inactive feat tab."
  :group 'multi-panel)

(defface mp-toggle-on-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for toggle button when ON."
  :group 'multi-panel)

(defface mp-toggle-off-face
  '((t :foreground "#ff5555"))
  "Face for toggle button when OFF."
  :group 'multi-panel)

(defface mp-feat-tab-bar-face
  '((t :background "#1e1f29" :extend t))
  "Face for feat tab bar background."
  :group 'multi-panel)

;;; ============================================================
;;; State Variables
;;; ============================================================

(defvar mp--neotree-visible t
  "Whether NeoTree side panel is visible.")

(defvar mp--neotree-window nil
  "Window reference for NeoTree.")

(defvar mp--workarea-window nil
  "Window reference for main WorkArea.")

(defvar mp--feat-tab-bar-window nil
  "Window reference for the Feat Tab bar.")

(defvar mp--feat-tab-bar-height 2
  "Height of the Feat Tab bar in lines.")

;;; ============================================================
;;; Feat Tab Definition System
;;; ============================================================

(cl-defstruct mp-feat-tab
  "Structure for a Feat Tab definition."
  id           ; Symbol identifier
  name         ; Display name
  key          ; Keybinding key after C-x j
  icon         ; Icon string
  setup-fn     ; Function to setup WorkArea
  teardown-fn  ; Optional teardown function
  state        ; Plist for tab-specific state
  buffers      ; List of buffers created by this tab
  window-state); Saved window configuration

(defvar mp--feat-tabs (make-hash-table :test 'eq)
  "Hash table of registered feat tabs by ID.")

(defvar mp--current-feat-tab nil
  "Currently active feat tab ID.")

(defvar mp--feat-tab-order '()
  "Ordered list of feat tab IDs for display.")

;;; ============================================================
;;; Keymap
;;; ============================================================

(defvar mp-prefix-map (make-sparse-keymap)
  "Keymap for multi-panel (C-x j prefix).")

(global-set-key (kbd "C-x j") mp-prefix-map)

;; Toggle keybindings
(define-key mp-prefix-map (kbd "n") #'mp-toggle-neotree)

;;; ============================================================
;;; Feat Tab Definition Macro
;;; ============================================================

(defmacro mp-define-feat-tab (id &rest props)
  "Define a new feat tab with ID and PROPS."
  (declare (indent 1))
  (let ((name (plist-get props :name))
        (key (plist-get props :key))
        (icon (plist-get props :icon))
        (setup (plist-get props :setup))
        (teardown (plist-get props :teardown)))
    `(progn
       (puthash ',id
                (make-mp-feat-tab
                 :id ',id
                 :name ,name
                 :key ,key
                 :icon (or ,icon "")
                 :setup-fn ,setup
                 :teardown-fn ,teardown
                 :state nil
                 :buffers nil
                 :window-state nil)
                mp--feat-tabs)
       (add-to-list 'mp--feat-tab-order ',id t)
       (define-key mp-prefix-map (kbd ,key)
                   (lambda () (interactive) (mp-switch-to ',id))))))

;;; ============================================================
;;; Side Panel Management (NeoTree)
;;; ============================================================

(defun mp-toggle-neotree ()
  "Toggle NeoTree visibility."
  (interactive)
  (if mp--neotree-visible
      (mp--hide-neotree)
    (mp--show-neotree))
  (mp--update-feat-tab-bar))

(defun mp--show-neotree ()
  "Show NeoTree side panel."
  (when (and (not mp--neotree-visible) (fboundp 'neotree-show))
    (let ((workspace (if (and (boundp 'ai-session--current) ai-session--current
                              (fboundp 'ai-session-workspace))
                         (ai-session-workspace ai-session--current)
                       default-directory)))
      (setq neo-window-width mp-neotree-width)
      (neotree-dir workspace)
      (setq mp--neotree-window (neo-global--get-window))
      (setq mp--neotree-visible t)
      ;; Return to workarea
      (when (window-live-p mp--workarea-window)
        (select-window mp--workarea-window)))))

(defun mp--hide-neotree ()
  "Hide NeoTree side panel."
  (when (and mp--neotree-visible (fboundp 'neotree-hide))
    (neotree-hide)
    (setq mp--neotree-window nil)
    (setq mp--neotree-visible nil)))

;;; ============================================================
;;; Layout Management
;;; ============================================================

(defun mp--setup-base-layout (session &optional resume)
  "Setup the base multi-panel layout for SESSION.
If RESUME is non-nil, resume the agent session."
  (delete-other-windows)
  (let ((workspace (if (and session (fboundp 'ai-session-workspace))
                       (ai-session-workspace session)
                     default-directory)))

    ;; Main window becomes WorkArea base
    (setq mp--workarea-window (selected-window))

    ;; Split for Feat Tab Bar at top
    (split-window-below mp--feat-tab-bar-height)
    (setq mp--feat-tab-bar-window (selected-window))
    (switch-to-buffer (mp--setup-feat-tab-bar))
    (set-window-dedicated-p mp--feat-tab-bar-window t)

    ;; Move to WorkArea below
    (other-window 1)
    (setq mp--workarea-window (selected-window))

    ;; Setup NeoTree (left side window)
    (when (fboundp 'neotree-dir)
      (setq neo-window-width mp-neotree-width)
      (neotree-dir workspace)
      (setq mp--neotree-window (neo-global--get-window))
      (setq mp--neotree-visible t)
      (select-window mp--workarea-window))

    ;; Setup AI Chat (right side window)
    (mp--show-ai-chat resume)))

;;; ============================================================
;;; Feat Tab Bar
;;; ============================================================

(defun mp--render-feat-tabs-content ()
  "Render feat tabs content for the tab bar buffer."
  (let ((tabs '()))
    ;; Feat Tabs
    (dolist (id mp--feat-tab-order)
      (let* ((tab (gethash id mp--feat-tabs))
             (active (eq id mp--current-feat-tab))
             (face (if active 'mp-feat-tab-active-face 'mp-feat-tab-inactive-face))
             (icon (mp-feat-tab-icon tab))
             (name (mp-feat-tab-name tab))
             (key (mp-feat-tab-key tab)))
        (push (propertize (format " %s %s [%s] " icon name key)
                          'face face
                          'mouse-face 'highlight
                          'keymap (mp--feat-tab-keymap id)
                          'help-echo (format "Switch to %s (C-x j %s)" name key))
              tabs)))
    (let ((tabs-str (mapconcat #'identity (nreverse tabs) ""))
          (toggles (mp--render-toggle-buttons)))
      (concat tabs-str "  " toggles))))

(defun mp--render-toggle-buttons ()
  "Render toggle buttons for NeoTree and AI Chat."
  (let* ((neo-face (if mp--neotree-visible 'mp-toggle-on-face 'mp-toggle-off-face))
         (ai-face (if mp--ai-chat-visible 'mp-toggle-on-face 'mp-toggle-off-face))
         (neo-btn (propertize "[N]"
                              'face neo-face
                              'mouse-face 'highlight
                              'keymap mp--toggle-neo-keymap
                              'help-echo "Toggle NeoTree (C-x j n)"))
         (ai-btn (propertize "[A]"
                             'face ai-face
                             'mouse-face 'highlight
                             'keymap mp--toggle-ai-keymap
                             'help-echo "Toggle AI Chat (C-x j c)")))
    (concat neo-btn " " ai-btn)))

(defvar mp--toggle-ai-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'mp-toggle-ai-chat)
    map))

(defvar mp--toggle-neo-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'mp-toggle-neotree)
    map))

(defun mp--feat-tab-keymap (id)
  "Create keymap for feat tab with ID."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
                (lambda () (interactive) (mp-switch-to id)))
    map))

(defun mp--setup-feat-tab-bar ()
  "Setup the Feat Tab bar buffer."
  (let ((buf (get-buffer-create "*MP Feat Tabs*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (mp--render-feat-tabs-content))
        (goto-char (point-min)))
      (setq mode-line-format nil)
      (setq header-line-format nil)
      (setq cursor-type nil)
      (setq buffer-read-only t)
      (face-remap-add-relative 'default 'mp-feat-tab-bar-face))
    buf))

(defun mp--update-feat-tab-bar ()
  "Update the Feat Tab bar content."
  (let ((buf (get-buffer "*MP Feat Tabs*")))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (mp--render-feat-tabs-content))
          (goto-char (point-min)))))))

;;; ============================================================
;;; Feat Tab Switching
;;; ============================================================

(defun mp-switch-to (feat-tab-id)
  "Switch to feat tab with FEAT-TAB-ID."
  (interactive)
  (let ((tab (gethash feat-tab-id mp--feat-tabs))
        (session (and (boundp 'ai-session--current) ai-session--current)))
    (unless tab
      (error "Unknown feat tab: %s" feat-tab-id))

    ;; Save current tab's window state
    (when mp--current-feat-tab
      (let ((current-tab (gethash mp--current-feat-tab mp--feat-tabs)))
        (when current-tab
          (mp--save-workarea-state current-tab)
          (when (mp-feat-tab-teardown-fn current-tab)
            (funcall (mp-feat-tab-teardown-fn current-tab) session)))))

    (setq mp--current-feat-tab feat-tab-id)

    ;; Restore or setup fresh
    (if (mp-feat-tab-window-state tab)
        (mp--restore-workarea-state tab)
      (mp--clear-workarea)
      (when (window-live-p mp--workarea-window)
        (select-window mp--workarea-window))
      (when (mp-feat-tab-setup-fn tab)
        (funcall (mp-feat-tab-setup-fn tab) session)))

    (mp--update-feat-tab-bar)
    (message "Switched to %s" (mp-feat-tab-name tab))))

(defun mp--save-workarea-state (tab)
  "Save the current WorkArea window state into TAB."
  (when (window-live-p mp--workarea-window)
    (setf (mp-feat-tab-window-state tab)
          (list :config (current-window-configuration)
                :buffer (window-buffer mp--workarea-window)
                :point (window-point mp--workarea-window)))))

(defun mp--restore-workarea-state (tab)
  "Restore the WorkArea window state from TAB."
  (let ((state (mp-feat-tab-window-state tab)))
    (when state
      (condition-case nil
          (set-window-configuration (plist-get state :config))
        (error
         (mp--clear-workarea)
         (when (window-live-p mp--workarea-window)
           (select-window mp--workarea-window)
           (when (mp-feat-tab-setup-fn tab)
             (funcall (mp-feat-tab-setup-fn tab)
                      (and (boundp 'ai-session--current) ai-session--current)))))))))

(defun mp--clear-workarea ()
  "Clear WorkArea, keeping side panels."
  (when (window-live-p mp--workarea-window)
    (select-window mp--workarea-window)
    (delete-other-windows)
    ;; Restore side panels and tab bar
    (split-window-below mp--feat-tab-bar-height)
    (setq mp--feat-tab-bar-window (selected-window))
    (switch-to-buffer (get-buffer-create "*MP Feat Tabs*"))
    (set-window-dedicated-p mp--feat-tab-bar-window t)
    (other-window 1)
    (setq mp--workarea-window (selected-window))
    ;; Re-show side panels if they were visible
    (when mp--neotree-visible
      (setq mp--neotree-visible nil)
      (mp--show-neotree))))

;;; ============================================================
;;; AI Chat (vterm + Claude)
;;; ============================================================

(defcustom mp-ai-chat-width-fraction 0.16
  "AI Chat window width as fraction of frame width (default 1/6)."
  :type 'number
  :group 'multi-panel)

(defvar mp--ai-chat-visible nil
  "Whether AI Chat is visible.")

(defvar mp--ai-chat-buffer nil
  "AI Chat vterm buffer.")

(defun mp-toggle-ai-chat ()
  "Toggle AI Chat visibility."
  (interactive)
  (if-let ((win (and mp--ai-chat-buffer
                     (buffer-live-p mp--ai-chat-buffer)
                     (get-buffer-window mp--ai-chat-buffer))))
      (progn
        (delete-window win)
        (setq mp--ai-chat-visible nil))
    (mp--show-ai-chat))
  (mp--update-feat-tab-bar))

(defun mp--show-ai-chat (&optional resume)
  "Show AI Chat in side window.
If RESUME is non-nil, resume the agent session."
  (unless (and mp--ai-chat-buffer (buffer-live-p mp--ai-chat-buffer))
    (let* ((session (and (boundp 'ai-session--current) ai-session--current))
           (workspace (if (and session (fboundp 'ai-session-workspace))
                          (ai-session-workspace session)
                        default-directory))
           (cmd (when (and session (fboundp 'ai-session--build-agent-command))
                  (ai-session--build-agent-command session resume)))
           (default-directory workspace)
           (vterm-shell "/bin/zsh")
           (vterm-kill-buffer-on-exit nil))
      (setq mp--ai-chat-buffer
            (save-window-excursion (vterm "*AI Chat*")))
      (when cmd
        (run-at-time 0.2 nil
                     (lambda (c b)
                       (when (buffer-live-p b)
                         (with-current-buffer b
                           (vterm-send-string (concat c "\n")))))
                     cmd mp--ai-chat-buffer))))
  (display-buffer-in-side-window
   mp--ai-chat-buffer
   `((side . right)
     (slot . 0)
     (window-width . ,mp-ai-chat-width-fraction)
     (window-parameters . ((no-delete-other-windows . t)))))
  (setq mp--ai-chat-visible t))

(define-key mp-prefix-map (kbd "c") #'mp-toggle-ai-chat)

;;; ============================================================
;;; Panel Loading
;;; ============================================================

(defun mp--load-panels ()
  "Load all panel definitions from `mp-panel-directory'."
  (when (file-directory-p mp-panel-directory)
    (add-to-list 'load-path mp-panel-directory)
    (dolist (file (directory-files mp-panel-directory t "\\.el$"))
      (condition-case err
          (load file nil t)
        (error (message "Failed to load panel %s: %s" file err))))))

;;; ============================================================
;;; Public API
;;; ============================================================

;;;###autoload
(defun mp-initialize (session &optional resume)
  "Initialize multi-panel layout for SESSION.
If RESUME is non-nil, resume the agent session."
  (interactive)
  (when (= 0 (hash-table-count mp--feat-tabs))
    (mp--load-panels))

  (mp--setup-base-layout session resume)

  (if (> (hash-table-count mp--feat-tabs) 0)
      (mp-switch-to (car mp--feat-tab-order))
    (when (window-live-p mp--workarea-window)
      (select-window mp--workarea-window)
      (switch-to-buffer (get-buffer-create "*WorkArea*")))))

(provide 'multi-panel)

;;; multi-panel.el ends here
