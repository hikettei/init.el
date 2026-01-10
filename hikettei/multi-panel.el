;;; multi-panel.el --- Multi-Panel WorkArea System -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; Multi-panel system for AI-assisted development.
;;
;; Layout (fixed structure):
;; +--------+----------------------------------+-------------+
;; |        | [Autopilot] [Terminal] [...] [N][A]           |
;; |        +----------------------------------+             |
;; |        |                                  |             |
;; | Neo    |           WorkArea               |  AI Chat    |
;; | Tree   |    (can contain child panels)    |  (vterm)    |
;; |        |                                  |             |
;; +--------+----------------------------------+-------------+
;;
;; - NeoTree: Toggle with C-x j n (left side window)
;; - AIChat: Toggle with C-x j c (right side window)
;; - [N][A]: Toggle buttons in tab bar
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

(defcustom mp-ai-chat-width 60
  "AI Chat width in columns."
  :type 'integer
  :group 'multi-panel)

(defcustom mp-ai-chat-min-width 30
  "Minimum AI Chat width in columns."
  :type 'integer
  :group 'multi-panel)

(defcustom mp-vterm-anti-flicker t
  "Enable intelligent flicker reduction for vterm display."
  :type 'boolean
  :group 'multi-panel)

(defcustom mp-vterm-render-delay 0.005
  "Rendering optimization delay for batched terminal updates (seconds)."
  :type 'number
  :group 'multi-panel)

(defcustom mp-prevent-reflow-glitch t
  "Workaround for terminal scrolling bug on height-only changes."
  :type 'boolean
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

(defvar mp--ai-chat-visible t
  "Whether AI Chat side panel is visible.")

(defvar mp--neotree-window nil
  "Window reference for NeoTree.")

(defvar mp--ai-chat-window nil
  "Window reference for AI Chat.")

(defvar mp--ai-chat-buffer nil
  "Buffer for AI Chat vterm.")

(defvar mp--workarea-window nil
  "Window reference for main WorkArea.")

(defvar mp--feat-tab-bar-window nil
  "Window reference for the Feat Tab bar.")

(defvar mp--feat-tab-bar-height 2
  "Height of the Feat Tab bar in lines.")

;;; ============================================================
;;; Vterm Anti-Flicker Optimization
;;; ============================================================

(defvar-local mp--vterm-render-queue nil
  "Queue for optimizing terminal rendering sequences.")

(defvar-local mp--vterm-render-timer nil
  "Timer for executing queued rendering operations.")

(defun mp--ai-chat-buffer-p (buffer)
  "Check if BUFFER is our AI Chat buffer."
  (when-let ((name (if (stringp buffer) buffer (buffer-name buffer))))
    (string-prefix-p "*AI Chat" name)))

(defun mp--vterm-smart-renderer (orig-fun process input)
  "Smart rendering filter for optimized vterm display updates.
ORIG-FUN is the underlying filter to enhance.
PROCESS is the terminal process being optimized.
INPUT contains the terminal output stream."
  (if (or (not mp-vterm-anti-flicker)
          (not (mp--ai-chat-buffer-p (process-buffer process))))
      (funcall orig-fun process input)
    (with-current-buffer (process-buffer process)
      (let* ((complex-redraw-detected
              (string-match-p "\033\\[[0-9]*A.*\033\\[K.*\033\\[[0-9]*A.*\033\\[K" input))
             (clear-count (cl-count-if (lambda (s) (string= s "\033[K"))
                                       (split-string input "\033\\[K" t)))
             (escape-count (cl-count ?\033 input))
             (input-length (length input))
             (escape-density (if (> input-length 0)
                                 (/ (float escape-count) input-length)
                               0)))
        (if (or complex-redraw-detected
                (and (> escape-density 0.3)
                     (>= clear-count 2))
                mp--vterm-render-queue)
            (progn
              (setq mp--vterm-render-queue
                    (concat mp--vterm-render-queue input))
              (when mp--vterm-render-timer
                (cancel-timer mp--vterm-render-timer))
              (setq mp--vterm-render-timer
                    (run-at-time mp-vterm-render-delay nil
                                 (lambda (buf)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (when mp--vterm-render-queue
                                         (let ((inhibit-redisplay t)
                                               (data mp--vterm-render-queue))
                                           (setq mp--vterm-render-queue nil
                                                 mp--vterm-render-timer nil)
                                           (funcall orig-fun
                                                    (get-buffer-process buf)
                                                    data))))))
                                 (current-buffer))))
          (funcall orig-fun process input))))))

;;; ============================================================
;;; Terminal Reflow Prevention
;;; ============================================================

(defun mp--terminal-reflow-filter (original-fn &rest args)
  "Filter terminal reflows to prevent height-only resize triggers.
Wraps ORIGINAL-FN to suppress reflow signals unless terminal width changed."
  (let* ((base-result (apply original-fn args))
         (dimensions-stable t))
    (dolist (win (window-list))
      (when-let* ((buf (window-buffer win))
                  ((mp--ai-chat-buffer-p buf)))
        (let* ((new-width (window-width win))
               (cached-width (window-parameter win 'mp-cached-width)))
          (unless (eql new-width cached-width)
            (setq dimensions-stable nil)
            (set-window-parameter win 'mp-cached-width new-width)))))
    (cond
     ((not (mp--ai-chat-buffer-p (current-buffer)))
      base-result)
     ((bound-and-true-p vterm-copy-mode)
      nil)
     ((not dimensions-stable)
      base-result)
     (t nil))))

(defvar mp--resize-timer nil
  "Timer for debounced resize.")

(defun mp--setup-vterm-optimizations ()
  "Setup vterm rendering optimizations."
  (when (and (fboundp 'vterm) mp-vterm-anti-flicker)
    (advice-add 'vterm--filter :around #'mp--vterm-smart-renderer))
  (when (and (fboundp 'vterm--window-adjust-process-window-size)
             mp-prevent-reflow-glitch)
    (advice-add 'vterm--window-adjust-process-window-size
                :around #'mp--terminal-reflow-filter))
  ;; Global hook for mouse resize - debounced force resize
  (add-hook 'window-size-change-functions #'mp--on-window-size-change))

(defun mp--on-window-size-change (_frame)
  "Debounced force resize for AI Chat vterm on window resize."
  ;; Cancel previous timer
  (when mp--resize-timer
    (cancel-timer mp--resize-timer))
  ;; Schedule resize after mouse drag settles
  (setq mp--resize-timer
        (run-at-time 0.1 nil #'mp--do-force-resize)))

(defun mp--do-force-resize ()
  "Actually perform the force resize (like C-x j R)."
  (setq mp--resize-timer nil)
  (when (and mp--ai-chat-buffer
             (buffer-live-p mp--ai-chat-buffer)
             mp--ai-chat-window
             (window-live-p mp--ai-chat-window))
    (with-current-buffer mp--ai-chat-buffer
      (when (and (eq major-mode 'vterm-mode)
                 (bound-and-true-p vterm--term))
        (let ((width (window-body-width mp--ai-chat-window))
              (height (window-body-height mp--ai-chat-window)))
          (vterm--set-size vterm--term height width)
          (when-let ((proc (get-buffer-process (current-buffer))))
            (signal-process proc 'SIGWINCH)))))))

(defun mp--cleanup-vterm-optimizations ()
  "Remove vterm rendering optimizations."
  (when (fboundp 'vterm--filter)
    (advice-remove 'vterm--filter #'mp--vterm-smart-renderer))
  (when (fboundp 'vterm--window-adjust-process-window-size)
    (advice-remove 'vterm--window-adjust-process-window-size
                   #'mp--terminal-reflow-filter))
  (remove-hook 'window-size-change-functions #'mp--on-window-size-change)
  (when mp--resize-timer
    (cancel-timer mp--resize-timer)
    (setq mp--resize-timer nil)))

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
(define-key mp-prefix-map (kbd "c") #'mp-toggle-ai-chat)
(define-key mp-prefix-map (kbd "[") #'mp-shrink-ai-chat)
(define-key mp-prefix-map (kbd "]") #'mp-enlarge-ai-chat)
(define-key mp-prefix-map (kbd "R") #'mp-force-resize-ai-chat)

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
;;; Side Panel Management (NeoTree & AI Chat)
;;; ============================================================

(defun mp-toggle-neotree ()
  "Toggle NeoTree visibility."
  (interactive)
  (if mp--neotree-visible
      (mp--hide-neotree)
    (mp--show-neotree))
  (mp--update-feat-tab-bar))

(defun mp-toggle-ai-chat ()
  "Toggle AI Chat visibility."
  (interactive)
  (if mp--ai-chat-visible
      (mp--hide-ai-chat)
    (mp--show-ai-chat))
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

(defun mp--show-ai-chat ()
  "Show AI Chat side panel."
  (when (not mp--ai-chat-visible)
    (let ((buf (or mp--ai-chat-buffer
                   (get-buffer "*AI Chat*"))))
      (if (and buf (buffer-live-p buf))
          ;; Reuse existing buffer
          (progn
            (setq mp--ai-chat-window
                  (display-buffer-in-side-window
                   buf
                   `((side . right)
                     (window-width . ,mp-ai-chat-width)
                     (dedicated . t)
                     (window-parameters . ((no-delete-other-windows . t))))))
            ;; Make window resizable
            (set-window-parameter mp--ai-chat-window 'window-size-fixed nil)
            (setq mp--ai-chat-visible t))
        ;; Create new vterm
        (setq mp--ai-chat-buffer (get-buffer-create "*AI Chat*"))
        (setq mp--ai-chat-window
              (display-buffer-in-side-window
               mp--ai-chat-buffer
               `((side . right)
                 (window-width . ,mp-ai-chat-width)
                 (dedicated . t)
                 (window-parameters . ((no-delete-other-windows . t))))))
        ;; Make window resizable
        (set-window-parameter mp--ai-chat-window 'window-size-fixed nil)
        (select-window mp--ai-chat-window)
        (when (fboundp 'vterm)
          (vterm)
          (setq mp--ai-chat-buffer (current-buffer)))
        (setq mp--ai-chat-visible t))
      ;; Return to workarea
      (when (window-live-p mp--workarea-window)
        (select-window mp--workarea-window)))))

(defun mp--hide-ai-chat ()
  "Hide AI Chat side panel."
  (when mp--ai-chat-visible
    (when (and mp--ai-chat-window (window-live-p mp--ai-chat-window))
      (delete-window mp--ai-chat-window))
    (setq mp--ai-chat-window nil)
    (setq mp--ai-chat-visible nil)))

(defun mp-shrink-ai-chat (&optional delta)
  "Shrink AI Chat window by DELTA columns (default 5)."
  (interactive "P")
  (let ((delta (or delta 5)))
    (when (and mp--ai-chat-window (window-live-p mp--ai-chat-window))
      (let ((current-width (window-width mp--ai-chat-window)))
        (when (> current-width mp-ai-chat-min-width)
          (window-resize mp--ai-chat-window (- delta) t)
          ;; Sync vterm size after resize
          (mp--sync-ai-chat-vterm-size))))))

(defun mp-enlarge-ai-chat (&optional delta)
  "Enlarge AI Chat window by DELTA columns (default 5)."
  (interactive "P")
  (let ((delta (or delta 5)))
    (when (and mp--ai-chat-window (window-live-p mp--ai-chat-window))
      (window-resize mp--ai-chat-window delta t)
      ;; Sync vterm size after resize
      (mp--sync-ai-chat-vterm-size))))

(defun mp--sync-ai-chat-vterm-size ()
  "Sync AI Chat vterm terminal size with its window."
  (when (and mp--ai-chat-buffer
             (buffer-live-p mp--ai-chat-buffer)
             mp--ai-chat-window
             (window-live-p mp--ai-chat-window))
    (with-current-buffer mp--ai-chat-buffer
      (when (and (eq major-mode 'vterm-mode)
                 (bound-and-true-p vterm--term))
        (let ((width (window-body-width mp--ai-chat-window))
              (height (window-body-height mp--ai-chat-window)))
          (vterm--set-size vterm--term height width)
          ;; Send SIGWINCH to notify shell of size change
          (when-let ((proc (get-buffer-process (current-buffer))))
            (signal-process proc 'SIGWINCH)))))))

;;; ============================================================
;;; Layout Management
;;; ============================================================

(defun mp--setup-base-layout (session)
  "Setup the base multi-panel layout for SESSION."
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
    (mp--setup-ai-chat-buffer session)
    (setq mp--ai-chat-window
          (display-buffer-in-side-window
           mp--ai-chat-buffer
           `((side . right)
             (window-width . ,mp-ai-chat-width)
             (dedicated . t)
             (window-parameters . ((no-delete-other-windows . t))))))
    ;; Make the window resizable
    (set-window-parameter mp--ai-chat-window 'window-size-fixed nil)
    (setq mp--ai-chat-visible t)

    ;; Initialize vterm in the AI Chat window
    (mp--initialize-ai-chat-vterm)

    ;; Return to WorkArea
    (select-window mp--workarea-window)))

(defvar mp--ai-chat-init-info nil
  "Initialization info for AI Chat (workspace, cmd, buf-name).")

(defun mp--setup-ai-chat-buffer (session)
  "Prepare AI Chat initialization info for SESSION."
  (setq mp--ai-chat-buffer (get-buffer-create "*AI Chat*"))
  (when session
    (let* ((workspace (and (fboundp 'ai-session-workspace)
                           (ai-session-workspace session)))
           (title (or (and (fboundp 'ai-session-title)
                           (ai-session-title session))
                      "Session"))
           (cmd (and (fboundp 'ai-session--build-agent-command)
                     (ai-session--build-agent-command session nil)))
           (buf-name (format "*AI Chat: %s*" title)))
      (setq mp--ai-chat-init-info
            (list :workspace workspace :cmd cmd :buf-name buf-name)))))

(defun mp--initialize-ai-chat-vterm ()
  "Initialize vterm in the AI Chat window."
  (when (and mp--ai-chat-window
             (window-live-p mp--ai-chat-window)
             mp--ai-chat-init-info)
    (let* ((workspace (plist-get mp--ai-chat-init-info :workspace))
           (cmd (plist-get mp--ai-chat-init-info :cmd))
           (buf-name (plist-get mp--ai-chat-init-info :buf-name)))

      (select-window mp--ai-chat-window)

      (if (and (fboundp 'vterm) cmd (not (string-empty-p cmd)))
          ;; Create vterm and send command after initialization
          (let ((vterm-buffer-name buf-name)
                (vterm-kill-buffer-on-exit nil)
                (ws workspace)
                (agent-cmd cmd))
            (vterm buf-name)
            (setq mp--ai-chat-buffer (current-buffer))
            ;; Configure vterm buffer for better rendering
            (mp--configure-ai-chat-vterm-buffer)
            ;; Send command after vterm is ready
            (run-at-time 0.3 nil
                         (lambda (buffer ws cmd)
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (when (and (derived-mode-p 'vterm-mode)
                                          (boundp 'vterm--process)
                                          vterm--process
                                          (process-live-p vterm--process))
                                 (vterm-send-string
                                  (format "cd %s && %s\n"
                                          (shell-quote-argument ws) cmd))))))
                         mp--ai-chat-buffer ws agent-cmd))
        ;; Fallback: plain vterm
        (when (fboundp 'vterm)
          (vterm buf-name)
          (setq mp--ai-chat-buffer (current-buffer))))

      (setq mp--ai-chat-init-info nil))))

(defun mp--configure-ai-chat-vterm-buffer ()
  "Configure vterm buffer settings for AI Chat."
  (when (derived-mode-p 'vterm-mode)
    ;; Add small delay to batch updates and prevent flicker
    ;; nil causes immediate updates which flickers
    (setq-local vterm-timer-delay 0.01)
    ;; Prevent auto-scroll flicker
    (setq-local vterm-scroll-to-bottom-on-output nil)
    ;; Disable immediate redraw to batch updates
    (when (boundp 'vterm--redraw-immididately)
      (setq-local vterm--redraw-immididately nil))
    ;; Let vterm control cursor entirely
    (setq-local cursor-in-non-selected-windows nil)
    (setq-local blink-cursor-mode nil)
    (setq-local cursor-type nil)
    ;; Disable hl-line to reduce flicker
    (setq-local global-hl-line-mode nil)
    (when (featurep 'hl-line)
      (hl-line-mode -1))
    ;; Remove margins/fringes that affect terminal width calculation
    (setq-local left-margin-width 0)
    (setq-local right-margin-width 0)
    (set-window-margins (selected-window) 0 0)
    (set-window-fringes (selected-window) 0 0)
    ;; Fix nobreak-space theming in prompt
    (face-remap-add-relative 'nobreak-space :underline nil :foreground nil)
    ;; Increase process read buffering
    (when-let ((proc (get-buffer-process (current-buffer))))
      (set-process-query-on-exit-flag proc nil)
      (when (fboundp 'process-put)
        (process-put proc 'read-output-max 4096)))
    ;; Add hook for window resize sync
    (add-hook 'window-configuration-change-hook
              #'mp--vterm-sync-size nil t)))

(defun mp--vterm-sync-size ()
  "Sync vterm terminal size with window size."
  (when (and (eq major-mode 'vterm-mode)
             (bound-and-true-p vterm--term))
    (let ((width (window-body-width))
          (height (window-body-height)))
      (vterm--set-size vterm--term height width)
      ;; Send SIGWINCH to notify shell of size change
      (when-let ((proc (get-buffer-process (current-buffer))))
        (signal-process proc 'SIGWINCH)))))

(defun mp-force-resize-ai-chat ()
  "Force AI Chat vterm to recalculate size."
  (interactive)
  (when (and mp--ai-chat-buffer (buffer-live-p mp--ai-chat-buffer))
    (with-current-buffer mp--ai-chat-buffer
      (when (and (eq major-mode 'vterm-mode)
                 (bound-and-true-p vterm--term))
        (let ((width (window-body-width mp--ai-chat-window))
              (height (window-body-height mp--ai-chat-window)))
          (vterm--set-size vterm--term height width)
          ;; Send SIGWINCH to notify shell
          (when-let ((proc (get-buffer-process (current-buffer))))
            (signal-process proc 'SIGWINCH))
          (message "AI Chat resized to %dx%d" width height))))))

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

(defvar mp--toggle-neo-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'mp-toggle-neotree)
    map))

(defvar mp--toggle-ai-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'mp-toggle-ai-chat)
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
      (mp--show-neotree))
    (when mp--ai-chat-visible
      (setq mp--ai-chat-visible nil)
      (mp--show-ai-chat))))

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
(defun mp-initialize (session)
  "Initialize multi-panel layout for SESSION."
  (interactive)
  ;; Setup vterm optimizations
  (mp--setup-vterm-optimizations)

  ;; Allow narrower windows for AI Chat
  (setq window-min-width (min window-min-width mp-ai-chat-min-width))

  (when (= 0 (hash-table-count mp--feat-tabs))
    (mp--load-panels))

  (mp--setup-base-layout session)

  (if (> (hash-table-count mp--feat-tabs) 0)
      (mp-switch-to (car mp--feat-tab-order))
    (when (window-live-p mp--workarea-window)
      (select-window mp--workarea-window)
      (switch-to-buffer (get-buffer-create "*WorkArea*")))))

(provide 'multi-panel)

;;; multi-panel.el ends here
