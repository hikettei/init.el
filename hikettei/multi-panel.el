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

(defvar mp--feat-tab-bar-height 1
  "Height of the Feat Tab bar in lines.")

;;; ============================================================
;;; Window Protection
;;; ============================================================

(defvar mp--bypass-window-protection nil
  "When non-nil, bypass window protection checks in delete-window advice.")

(defun mp--protected-window-p (window)
  "Return t if WINDOW is a protected multi-panel window."
  (and (window-live-p window)
       (or (window-parameter window 'mp-protected)
           (eq window mp--feat-tab-bar-window)
           (eq window mp--neotree-window)
           (eq window mp--ai-chat-window))))

(defun mp--delete-window-advice (orig-fn &optional window)
  "Advice for `delete-window' to protect multi-panel layout.
If WINDOW is protected or deletion would leave only protected windows, block it."
  (let* ((win (or window (selected-window)))
         (other-windows (delq win (window-list)))
         (non-protected-remaining (cl-remove-if #'mp--protected-window-p other-windows)))
    (cond
     ;; Bypass protection if internal toggle
     (mp--bypass-window-protection
      (funcall orig-fn window))
     ;; Don't delete protected windows
     ((mp--protected-window-p win)
      (message "Cannot delete protected window. Use toggle commands instead."))
     ;; Don't delete if it would leave only protected windows
     ((null non-protected-remaining)
      (message "Cannot delete last non-protected window."))
     (t
      (funcall orig-fn window)))))

(advice-add 'delete-window :around #'mp--delete-window-advice)

;;; ============================================================
;;; Feat Tab Definition System
;;; ============================================================
;;; ============================================================

(cl-defstruct mp-feat-tab
  "Structure for a Feat Tab definition."
  id           ; Symbol identifier
  name         ; Display name
  key          ; Keybinding key after C-x j
  icon         ; Icon string
  setup-fn     ; Function to setup WorkArea
  teardown-fn  ; Optional teardown function
  state        ; Plist for tab-specific state (panel can store anything here)
  buffers      ; List of buffers belonging to this tab
  workarea-state) ; Saved workarea internal state (buffers, splits)

(defvar mp--feat-tabs (make-hash-table :test 'eq)
  "Hash table of registered feat tabs by ID.")

(defvar mp--current-feat-tab nil
  "Currently active feat tab ID.")

(defvar mp--feat-tab-order '()
  "Ordered list of feat tab IDs for display.")

(defcustom mp-feat-tab-preferred-order
  '(autopilot hikettei terminal git memory explore discussion monitor freeform)
  "Preferred display order for feat tabs.
Tabs not in this list will be appended at the end."
  :type '(repeat symbol)
  :group 'multi-panel)

(defvar mp--review-pending nil
  "When non-nil, a review is pending in Autopilot.")

(defvar mp--ai-chat-window nil
  "Window reference for AI Chat (right side).")
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
                 :workarea-state nil)
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
    (let ((mp--bypass-window-protection t))
      (neotree-hide))
    (setq mp--neotree-window nil)
    (setq mp--neotree-visible nil)))

;;; ============================================================
;;; Layout Management  
;;; ============================================================

(defun mp--cleanup-stale-windows ()
  "Clean up any stale multi-panel windows and buffers."
  ;; Kill old feat tabs buffer to force fresh creation
  (let ((tab-buf (get-buffer "*MP Feat Tabs*")))
    (when tab-buf
      ;; First, undedicate and unprotect all windows showing this buffer
      (dolist (win (get-buffer-window-list tab-buf nil t))
        (set-window-parameter win 'mp-protected nil)
        (set-window-parameter win 'no-delete-other-windows nil)
        (set-window-dedicated-p win nil))
      ;; Kill the buffer
      (kill-buffer tab-buf)))
  ;; Reset all window references
  (setq mp--feat-tab-bar-window nil)
  (setq mp--workarea-window nil)
  (setq mp--neotree-window nil) 
  (setq mp--ai-chat-window nil)
  (setq mp--ai-chat-visible nil)
  (setq mp--neotree-visible nil))

(cl-defun mp--setup-base-layout (session &optional resume)
  "Setup the base multi-panel layout for SESSION.
If RESUME is non-nil, resume the agent session."
  ;; Clean up any stale windows/buffers first (this resets all refs)
  (mp--cleanup-stale-windows)
  
  ;; Delete other windows to start fresh
  (ignore-errors (delete-other-windows))
  
  (let ((workspace (if (and session (fboundp 'ai-session-workspace))
                       (ai-session-workspace session)
                     default-directory)))

    ;; Split for Feat Tab Bar at top (current window becomes small tab bar)
    (let ((workarea-win (split-window-below mp--feat-tab-bar-height)))
      ;; Current window is now the tab bar (small, at top)
      (setq mp--feat-tab-bar-window (selected-window))
      (switch-to-buffer (mp--setup-feat-tab-bar))
      (set-window-dedicated-p mp--feat-tab-bar-window t)
      (set-window-parameter mp--feat-tab-bar-window 'no-delete-other-windows t)
      (set-window-parameter mp--feat-tab-bar-window 'mp-protected t)
      (window-preserve-size mp--feat-tab-bar-window nil t)

      ;; Setup WorkArea (the larger window below)
      (setq mp--workarea-window workarea-win)
      (select-window mp--workarea-window)
      (set-window-parameter mp--workarea-window 'no-delete-other-windows t))

    ;; Setup NeoTree (left side window)
    (when (fboundp 'neotree-dir)
      (setq neo-window-width mp-neotree-width)
      (neotree-dir workspace)
      (setq mp--neotree-window (neo-global--get-window))
      (setq mp--neotree-visible t)
      ;; Protect NeoTree window
      (when (window-live-p mp--neotree-window)
        (set-window-parameter mp--neotree-window 'no-delete-other-windows t)
        (set-window-parameter mp--neotree-window 'mp-protected t))
      (select-window mp--workarea-window))

    ;; Setup AI Chat (right side window)
    (mp--show-ai-chat resume)))

;;; ============================================================
;;; Feat Tab Bar
;;; ============================================================
;;; Feat Tab Bar
;;; ============================================================

(defun mp--sort-feat-tab-order ()
  "Sort `mp--feat-tab-order' according to `mp-feat-tab-preferred-order'."
  (let ((preferred mp-feat-tab-preferred-order)
        (current mp--feat-tab-order))
    (setq mp--feat-tab-order
          (append
           ;; First: tabs in preferred order that exist
           (cl-remove-if-not (lambda (id) (memq id current)) preferred)
           ;; Then: remaining tabs not in preferred order
           (cl-remove-if (lambda (id) (memq id preferred)) current)))))

(defun mp--render-feat-tabs-content ()
  "Render feat tabs content for the tab bar buffer.
Active tab shows full info, inactive tabs show only key."
  (mp--sort-feat-tab-order)
  (let ((tabs '()))
    ;; Feat Tabs
    (dolist (id mp--feat-tab-order)
      (let* ((tab (gethash id mp--feat-tabs))
             (active (eq id mp--current-feat-tab))
             (face (if active 'mp-feat-tab-active-face 'mp-feat-tab-inactive-face))
             (key (mp-feat-tab-key tab)))
        (if active
            ;; Active tab: full display
            (let* ((icon (mp-feat-tab-icon tab))
                   (name (mp-feat-tab-name tab))
                   (indicator (if (and (eq id 'autopilot) mp--review-pending) "🔴 " ""))
                   (mode-indicator (if (and (eq id 'autopilot)
                                            (boundp 'mp-autopilot-review-mode))
                                       (format " [%s]"
                                               (pcase mp-autopilot-review-mode
                                                 ('manual "M")
                                                 ('hybrid "H")
                                                 ('auto-review "A")
                                                 ('no-review "N")
                                                 (_ "?")))
                                     ""))
                   (voice-indicator (if (and (eq id 'autopilot)
                                             (boundp 'mcp-voicebox-mode))
                                        (format " %s"
                                                (pcase mcp-voicebox-mode
                                                  ('disabled "🔇")
                                                  ('minimum "🔈")
                                                  ('maximum "🔊")
                                                  (_ "")))
                                      "")))
              (push (propertize (format " %s%s %s [%s]%s%s " indicator icon name key mode-indicator voice-indicator)
                                'face face
                                'mouse-face 'highlight
                                'keymap (mp--feat-tab-keymap id)
                                'help-echo (format "Switch to %s (C-x j %s)" name key))
                    tabs))
          ;; Inactive tab: abbreviated display (key only)
          (push (propertize (format " [%s] " key)
                            'face face
                            'mouse-face 'highlight
                            'keymap (mp--feat-tab-keymap id)
                            'help-echo (format "Switch to %s (C-x j %s)" (mp-feat-tab-name tab) key))
                tabs))))
    (let ((tabs-str (mapconcat #'identity (nreverse tabs) ""))
          (toggles (mp--render-toggle-buttons)))
      (concat tabs-str "  " toggles))))

(defvar mp--toggle-ai-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'mp-toggle-ai-chat)
    map)
  "Keymap for AI Chat toggle button.")

(defvar mp--toggle-neo-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'mp-toggle-neotree)
    map)
  "Keymap for NeoTree toggle button.")

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

(defun mp--workarea-windows ()
  "Get list of windows that belong to the WorkArea.
This includes mp--workarea-window and any windows split from it,
but excludes NeoTree, AI Chat, and Feat Tab bar."
  (let ((excluded (list mp--neotree-window mp--ai-chat-window mp--feat-tab-bar-window))
        (result '()))
    (dolist (win (window-list nil 'nomini))
      (unless (or (memq win excluded)
                  (window-dedicated-p win)
                  ;; Also exclude side windows
                  (window-parameter win 'window-side))
        (push win result)))
    (nreverse result)))

(defun mp--save-workarea-state (tab)
  "Save the current WorkArea state into TAB.
Saves complete window layout including splits using `window-state-get'."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (let* ((workarea-wins (mp--workarea-windows))
           (state (when workarea-wins
                    (list :window-state (window-state-get mp--workarea-window t)
                          :buffers (mapcar (lambda (w)
                                            (list :buffer (window-buffer w)
                                                  :file (buffer-file-name (window-buffer w))
                                                  :point (window-point w)
                                                  :start (window-start w)))
                                          workarea-wins)
                          :main-buffer (window-buffer mp--workarea-window)))))
      (setf (mp-feat-tab-workarea-state tab) state))))

(defun mp--restore-workarea-state (tab)
  "Restore the WorkArea state from TAB.
Restores complete window layout including splits using `window-state-put'."
  (let ((state (mp-feat-tab-workarea-state tab)))
    (when (and state (window-live-p mp--workarea-window))
      (let ((window-state (plist-get state :window-state)))
        (if window-state
            ;; Restore complete window configuration
            (condition-case err
                (window-state-put window-state mp--workarea-window 'safe)
              (error
               ;; Fallback: just restore main buffer
               (message "Failed to restore window state: %s" err)
               (let ((main-buf (plist-get state :main-buffer)))
                 (when (and main-buf (buffer-live-p main-buf))
                   (set-window-buffer mp--workarea-window main-buf)))))
          ;; No window-state, try old-style restore
          (let ((main-buf (plist-get state :main-buffer)))
            (when (and main-buf (buffer-live-p main-buf))
              (set-window-buffer mp--workarea-window main-buf))))))))

(defun mp--clear-workarea-windows ()
  "Delete extra windows in WorkArea, keeping only the main one."
  (let ((workarea-wins (mp--workarea-windows)))
    ;; Keep only mp--workarea-window, delete others
    (dolist (win workarea-wins)
      (unless (eq win mp--workarea-window)
        (when (window-live-p win)
          (delete-window win)))))
  ;; Ensure workarea window is valid
  (unless (window-live-p mp--workarea-window)
    ;; Try to find a valid workarea window
    (let ((wins (mp--workarea-windows)))
      (when wins
        (setq mp--workarea-window (car wins))))))

(defun mp--clear-workarea ()
  "Clear WorkArea to a blank state."
  (mp--clear-workarea-windows)
  (when (window-live-p mp--workarea-window)
    (select-window mp--workarea-window)
    (switch-to-buffer (get-buffer-create "*WorkArea*"))))

(defun mp-switch-to (feat-tab-id)
  "Switch to feat tab with FEAT-TAB-ID."
  (interactive)
  (let ((tab (gethash feat-tab-id mp--feat-tabs))
        (session (and (boundp 'ai-session--current) ai-session--current)))
    (unless tab
      (error "Unknown feat tab: %s" feat-tab-id))

    ;; Only switch if not already on this tab
    (unless (eq mp--current-feat-tab feat-tab-id)
      ;; Save current tab's state
      (when mp--current-feat-tab
        (let ((current-tab (gethash mp--current-feat-tab mp--feat-tabs)))
          (when current-tab
            (mp--save-workarea-state current-tab)
            (when (mp-feat-tab-teardown-fn current-tab)
              (funcall (mp-feat-tab-teardown-fn current-tab) session)))))

      (setq mp--current-feat-tab feat-tab-id)

      ;; Check if tab has saved state to restore
      (let ((saved-state (mp-feat-tab-workarea-state tab)))
        (if (and saved-state (plist-get saved-state :window-state))
            ;; Restore saved window state (preserves splits)
            (progn
              (mp--restore-workarea-state tab)
              (when (window-live-p mp--workarea-window)
                (select-window mp--workarea-window)))
          ;; No saved state - clear and setup fresh
          (mp--clear-workarea)
          (when (window-live-p mp--workarea-window)
            (select-window mp--workarea-window))
          (when (mp-feat-tab-setup-fn tab)
            (funcall (mp-feat-tab-setup-fn tab) session))))

      (mp--update-feat-tab-bar)
      (message "Switched to %s" (mp-feat-tab-name tab)))))

;;; ============================================================
;;; AI Chat (vterm + Claude)
;;; ============================================================

(defcustom mp-ai-chat-width-fraction 0.25
  "AI Chat window width as fraction of frame width (default 1/4)."
  :type 'number
  :group 'multi-panel)


(defvar mp--ai-chat-visible nil
  "Whether AI Chat is visible.")

(defvar mp--ai-chat-buffer nil
  "AI Chat vterm buffer.")

(defun mp-toggle-ai-chat ()
  "Toggle AI Chat visibility."
  (interactive)
  (if (and mp--ai-chat-window (window-live-p mp--ai-chat-window))
      (let ((mp--bypass-window-protection t))
        (delete-window mp--ai-chat-window)
        (setq mp--ai-chat-window nil)
        (setq mp--ai-chat-visible nil))
    (mp--show-ai-chat))
  (mp--update-feat-tab-bar))

(defun mp--show-ai-chat (&optional resume)
  "Show AI Chat in side window.
If RESUME is non-nil, resume the agent session."
  ;; When resuming, we need to start fresh with a new command
  (when (and resume mp--ai-chat-buffer (buffer-live-p mp--ai-chat-buffer))
    (kill-buffer mp--ai-chat-buffer)
    (setq mp--ai-chat-buffer nil))
  
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
  ;; Display and track window
  (setq mp--ai-chat-window
        (display-buffer-in-side-window
         mp--ai-chat-buffer
         `((side . right)
           (slot . 0)
           (window-width . ,mp-ai-chat-width-fraction)
           (window-parameters . ((no-delete-other-windows . t)
                                (mp-protected . t))))))
  ;; Preserve AI Chat window width
  (when (window-live-p mp--ai-chat-window)
    (window-preserve-size mp--ai-chat-window t t))  ; preserve width
  (setq mp--ai-chat-visible t))

(define-key mp-prefix-map (kbd "c") #'mp-toggle-ai-chat)
(define-key mp-prefix-map (kbd "v") #'mcp-voicebox-cycle-mode)

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
(defun mp-initialize (&optional session resume)
  "Initialize multi-panel layout for SESSION.
If RESUME is non-nil, resume the agent session.
SESSION defaults to `ai-session--current' if not provided."
  (interactive)
  (let ((session (or session (and (boundp 'ai-session--current) ai-session--current))))
    (when (= 0 (hash-table-count mp--feat-tabs))
      (mp--load-panels))

    (mp--setup-base-layout session resume)

    (if (> (hash-table-count mp--feat-tabs) 0)
        ;; Always start with autopilot if available, otherwise first tab
        (let ((start-tab (if (gethash 'autopilot mp--feat-tabs)
                             'autopilot
                           (car mp--feat-tab-order))))
          (mp-switch-to start-tab))
      (when (window-live-p mp--workarea-window)
        (select-window mp--workarea-window)
        (switch-to-buffer (get-buffer-create "*WorkArea*"))))))

(provide 'multi-panel)

;;; multi-panel.el ends here
