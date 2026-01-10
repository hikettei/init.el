;;; activity-panel.el --- Activity Panel for Autopilot Mode -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.1.1
;; Version: 0.2.2

;;; Commentary:
;;
;; Activity Panel displays real-time file operations from AI agents.
;; Shows file content with:
;; - Read highlights that fade after 3 seconds
;; - Diff overlays for pending edits
;;
;; Integrates with file-editor.el for overlay-based review.

;;; Code:

(require 'cl-lib)

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup activity-panel nil
  "Activity Panel for AI file operations."
  :group 'tools
  :prefix "activity-")

(defcustom activity-highlight-duration 3.0
  "Seconds before read highlights start fading."
  :type 'number
  :group 'activity-panel)

(defcustom activity-fade-steps 10
  "Number of steps in fade animation."
  :type 'integer
  :group 'activity-panel)

(defcustom activity-fade-interval 0.1
  "Seconds between fade steps."
  :type 'number
  :group 'activity-panel)

;;; ============================================================
;;; Faces
;;; ============================================================

(defface activity-read-highlight
  '((t :background "#3d4966"))
  "Face for read operation highlights."
  :group 'activity-panel)

(defface activity-read-highlight-faded
  '((t :background "#2d3654"))
  "Face for faded read highlights."
  :group 'activity-panel)

(defface activity-header
  '((t :foreground "#bd93f9" :weight bold :height 1.1))
  "Face for Activity Panel headers."
  :group 'activity-panel)

(defface activity-info
  '((t :foreground "#6272a4" :slant italic))
  "Face for Activity Panel info text."
  :group 'activity-panel)

;;; ============================================================
;;; State
;;; ============================================================

(cl-defstruct activity-state
  "State for Activity Panel."
  current-file       ; Path to currently displayed file
  current-buffer     ; Buffer showing the file
  review-session     ; Active file-editor-session (if any)
  read-overlays      ; List of (overlay . timer) for read highlights
  panel-window       ; Activity panel window reference
  file-window        ; File display window reference
  status-message     ; Current status message
  status-timer       ; Timer for clearing status
  initialized)       ; Whether panel is initialized

(defvar activity--state nil
  "Current Activity Panel state.")

(defvar activity--background-color "#282a36"
  "Background color for fading (Dracula theme).")

;;; ============================================================
;;; Color Utilities
;;; ============================================================

(defun activity--hex-to-rgb (hex)
  "Convert HEX color string to RGB list."
  (let ((hex (if (string-prefix-p "#" hex) (substring hex 1) hex)))
    (list (string-to-number (substring hex 0 2) 16)
          (string-to-number (substring hex 2 4) 16)
          (string-to-number (substring hex 4 6) 16))))

(defun activity--rgb-to-hex (rgb)
  "Convert RGB list to hex color string."
  (format "#%02x%02x%02x"
          (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))

(defun activity--blend-colors (fg bg alpha)
  "Blend FG color with BG color at ALPHA opacity (0-1)."
  (let ((fg-rgb (activity--hex-to-rgb fg))
        (bg-rgb (activity--hex-to-rgb bg)))
    (activity--rgb-to-hex
     (cl-mapcar (lambda (f b)
                  (min 255 (max 0 (round (+ (* f alpha) (* b (- 1 alpha)))))))
                fg-rgb bg-rgb))))

;;; ============================================================
;;; Panel Initialization
;;; ============================================================

(defun activity-panel-initialize (panel-window file-window)
  "Initialize Activity Panel with PANEL-WINDOW and FILE-WINDOW."
  (setq activity--state
        (make-activity-state
         :current-file nil
         :current-buffer nil
         :review-session nil
         :read-overlays nil
         :panel-window panel-window
         :file-window file-window
         :status-message nil
         :status-timer nil
         :initialized t))
  ;; Show initial status
  (activity-panel--update-header-line "Watching..." nil)
  (message "Activity Panel initialized"))

(defun activity-panel-cleanup ()
  "Clean up all overlays and state."
  (when activity--state
    ;; Cancel all fade timers and delete overlays
    (dolist (entry (activity-state-read-overlays activity--state))
      (when (timerp (cdr entry))
        (cancel-timer (cdr entry)))
      (when (overlayp (car entry))
        (delete-overlay (car entry))))
    (setf (activity-state-read-overlays activity--state) nil)
    (setf (activity-state-initialized activity--state) nil)))

(defun activity-panel-active-p ()
  "Return t if Activity Panel is active and initialized."
  (and activity--state
       (activity-state-initialized activity--state)
       (activity-state-file-window activity--state)
       (window-live-p (activity-state-file-window activity--state))))

;;; ============================================================
;;; Status Display
;;; ============================================================

(defun activity-panel--get-agent-name ()
  "Get current AI agent name from session."
  (if (and (boundp 'ai-session--current) ai-session--current
           (fboundp 'ai-session-agent))
      (ai-session-agent ai-session--current)
    "AI"))

(defun activity-panel--update-header-line (status &optional highlight)
  "Update header-line with STATUS. If HIGHLIGHT, use highlight color."
  (when-let ((win (and activity--state (activity-state-file-window activity--state))))
    (when (window-live-p win)
      (with-current-buffer (window-buffer win)
        (setq-local header-line-format
                    (propertize (format " [%s] %s"
                                       (activity-panel--get-agent-name)
                                       status)
                               'face (if highlight
                                        '(:background "#50fa7b" :foreground "#282a36" :weight bold)
                                      '(:background "#6272a4" :foreground "#f8f8f2" :weight bold))))))))

(defun activity-panel-set-status (status &optional clear-after)
  "Set STATUS message in the activity panel.
If CLEAR-AFTER is non-nil, reset to idle status after that many seconds."
  (when activity--state
    ;; Cancel existing timer
    (when-let ((timer (activity-state-status-timer activity--state)))
      (when (timerp timer)
        (cancel-timer timer)))
    ;; Update status
    (setf (activity-state-status-message activity--state) status)
    ;; Update header-line with highlight
    (activity-panel--update-header-line status t)
    ;; Set reset timer if requested
    (when clear-after
      (setf (activity-state-status-timer activity--state)
            (run-at-time clear-after nil #'activity-panel-reset-status)))))

(defun activity-panel-reset-status ()
  "Reset status to idle (Watching...)."
  (when activity--state
    (setf (activity-state-status-message activity--state) nil)
    (when-let ((timer (activity-state-status-timer activity--state)))
      (when (timerp timer)
        (cancel-timer timer))
      (setf (activity-state-status-timer activity--state) nil))
    ;; Show idle status
    (activity-panel--update-header-line "Watching..." nil)))

(defun activity-panel-clear-status ()
  "Clear the status message (alias for reset)."
  (activity-panel-reset-status))

;;; ============================================================
;;; File Display
;;; ============================================================

(defun activity-panel-show-file (file-path &optional start-line end-line)
  "Display FILE-PATH in the file window.
Optionally scroll to show lines START-LINE to END-LINE."
  (unless (activity-panel-active-p)
    (message "Activity Panel not active")
    (cl-return-from activity-panel-show-file nil))
  (let* ((file-window (activity-state-file-window activity--state))
         (existing-buffer (get-file-buffer file-path))
         (buffer (if existing-buffer
                     (progn
                       ;; Silently revert if buffer exists and file changed
                       (with-current-buffer existing-buffer
                         (when (and (buffer-file-name)
                                    (file-exists-p (buffer-file-name))
                                    (not (verify-visited-file-modtime existing-buffer)))
                           (revert-buffer t t t)))
                       existing-buffer)
                   (find-file-noselect file-path t)))) ; nowarn = t
    ;; Update state
    (setf (activity-state-current-file activity--state) file-path)
    (setf (activity-state-current-buffer activity--state) buffer)
    ;; Display in file window
    (with-selected-window file-window
      (switch-to-buffer buffer)
      ;; Scroll to show the relevant lines
      (when start-line
        (goto-char (point-min))
        (forward-line (1- start-line))
        (recenter 3)))
    buffer))

;;; ============================================================
;;; Read Highlight System
;;; ============================================================

(defun activity-panel-add-read-highlight (file-path start-line end-line)
  "Add read highlight for FILE-PATH from START-LINE to END-LINE.
Highlight fades after `activity-highlight-duration' seconds."
  (unless (activity-panel-active-p)
    (cl-return-from activity-panel-add-read-highlight nil))
  ;; Clear previous read highlights
  (activity--clear-read-overlays)
  ;; Show the file
  (let ((buffer (activity-panel-show-file file-path start-line end-line)))
    (when buffer
      ;; Create highlight overlays
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- start-line))
          (let ((start-pos (line-beginning-position)))
            (forward-line (- end-line start-line))
            (let* ((end-pos (line-end-position))
                   (ov (make-overlay start-pos end-pos buffer)))
              (overlay-put ov 'face 'activity-read-highlight)
              (overlay-put ov 'activity-type 'read-highlight)
              (overlay-put ov 'priority 50)
              ;; Start fade timer
              (let ((timer (run-at-time activity-highlight-duration nil
                                        #'activity--start-fade ov)))
                (push (cons ov timer)
                      (activity-state-read-overlays activity--state))))))))))

(defun activity--clear-read-overlays ()
  "Clear all read highlight overlays and timers."
  (when activity--state
    (dolist (entry (activity-state-read-overlays activity--state))
      (when (timerp (cdr entry))
        (cancel-timer (cdr entry)))
      (when (overlayp (car entry))
        (delete-overlay (car entry))))
    (setf (activity-state-read-overlays activity--state) nil)))

(defun activity--start-fade (overlay)
  "Begin fading OVERLAY."
  (when (and (overlayp overlay) (overlay-buffer overlay))
    (activity--fade-step overlay 0)))

(defun activity--fade-step (overlay step)
  "Fade OVERLAY at STEP of animation."
  (when (and (overlayp overlay) (overlay-buffer overlay))
    (if (>= step activity-fade-steps)
        ;; Fade complete - delete overlay
        (progn
          (delete-overlay overlay)
          (when activity--state
            (setf (activity-state-read-overlays activity--state)
                  (cl-remove-if (lambda (entry) (eq (car entry) overlay))
                                (activity-state-read-overlays activity--state)))))
      ;; Continue fading
      (let* ((alpha (- 1.0 (/ (float step) activity-fade-steps)))
             (faded-color (activity--blend-colors "#3d4966"
                                                   activity--background-color
                                                   alpha)))
        (overlay-put overlay 'face `(:background ,faded-color))
        (run-at-time activity-fade-interval nil
                     #'activity--fade-step overlay (1+ step))))))

;;; ============================================================
;;; Edit Review Integration
;;; ============================================================

(defun activity-panel-start-edit-review (session)
  "Start overlay-based edit review for SESSION.
SESSION is a file-editor-session struct."
  (unless (activity-panel-active-p)
    (cl-return-from activity-panel-start-edit-review nil))
  ;; Clear any existing read highlights
  (activity--clear-read-overlays)
  ;; Store session
  (setf (activity-state-review-session activity--state) session)
  ;; Show the file being edited
  (let* ((file-path (file-editor-session-file-path session))
         (start-line (file-editor-session-start-line session)))
    (activity-panel-show-file file-path start-line)))

(defun activity-panel-end-edit-review ()
  "End the current edit review session."
  (when activity--state
    (setf (activity-state-review-session activity--state) nil)))

;;; ============================================================
;;; Public API
;;; ============================================================

(defun activity-panel-get-file-window ()
  "Get the file display window if Activity Panel is active."
  (when (activity-panel-active-p)
    (activity-state-file-window activity--state)))

(defun activity-panel-get-current-buffer ()
  "Get the current file buffer if Activity Panel is active."
  (when (activity-panel-active-p)
    (activity-state-current-buffer activity--state)))

(provide 'activity-panel)

;;; activity-panel.el ends here
