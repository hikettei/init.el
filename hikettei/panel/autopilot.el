;;; autopilot.el --- Autopilot Panel with Activity Display -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.2.0

;;; Commentary:
;;
;; Autopilot mode with Activity Panel for real-time AI operation visualization.
;; Shows file content with read highlights and edit overlays.
;; Layout: 60% Activity Panel (left) | 40% remaining (right)
;;

;;; Code:

(require 'multi-panel)

(declare-function activity-panel-initialize "activity-panel")
(declare-function activity-panel-cleanup "activity-panel")

;;; ============================================================
;;; Customization
;;; ============================================================

(defcustom mp-autopilot-panel-ratio 0.6
  "Ratio of Activity Panel width to total WorkArea width."
  :type 'number
  :group 'multi-panel)

;;; ============================================================
;;; State
;;; ============================================================

(defvar mp--autopilot-file-window nil
  "Window for file display in Autopilot mode.")

(defvar mp--autopilot-status-buffer nil
  "Buffer for Autopilot status display.")

;;; ============================================================
;;; Status Buffer
;;; ============================================================

(defun mp--create-autopilot-status-buffer ()
  "Create or get the Autopilot status buffer."
  (let ((buf (get-buffer-create "*Autopilot Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize " Autopilot Mode\n"
                           'face '(:foreground "#bd93f9" :weight bold :height 1.2)))
        (insert (propertize (make-string 40 ?─) 'face '(:foreground "#44475a")))
        (insert "\n\n")
        (insert (propertize " Watching AI Operations\n" 'face '(:foreground "#f8f8f2")))
        (insert "\n")
        (insert (propertize " Read Operations:\n" 'face '(:foreground "#8be9fd" :weight bold)))
        (insert "   (none yet)\n\n")
        (insert (propertize " Pending Edits:\n" 'face '(:foreground "#ffb86c" :weight bold)))
        (insert "   (none)\n\n")
        (insert (propertize (make-string 40 ?─) 'face '(:foreground "#44475a")))
        (insert "\n")
        (insert (propertize " Keys:\n" 'face '(:foreground "#6272a4")))
        (insert "   C-c C-c  Approve edit\n")
        (insert "   C-c C-k  Reject edit\n")
        (insert "   M-n/M-p  Navigate diffs\n"))
      (special-mode)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil))
    (setq mp--autopilot-status-buffer buf)
    buf))

;;; ============================================================
;;; Setup / Teardown
;;; ============================================================

(defun mp--setup-autopilot (session)
  "Setup Autopilot mode with Activity Panel.
SESSION is the current mcp-session."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    ;; Calculate split widths
    (let* ((total-width (window-width))
           (file-width (floor (* total-width mp-autopilot-panel-ratio))))
      ;; Left window shows the file with overlays
      (let ((file-window (selected-window)))
        ;; Create a scratch buffer for now
        (switch-to-buffer (get-buffer-create "*File Display*"))
        (with-current-buffer "*File Display*"
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "\n\n  Waiting for AI file access...\n\n"
                               'face '(:foreground "#6272a4" :slant italic)))
            (insert "  When the AI reads or edits a file,\n")
            (insert "  it will be displayed here with\n")
            (insert "  highlights and diff overlays.\n"))
          (special-mode))
        ;; Split for status panel on the right
        (let ((status-window (split-window-right file-width)))
          ;; Store window references
          (setq mp--autopilot-file-window file-window)
          ;; Setup status buffer in right window
          (select-window status-window)
          (switch-to-buffer (mp--create-autopilot-status-buffer))
          ;; Initialize Activity Panel with our windows
          (when (fboundp 'activity-panel-initialize)
            (require 'activity-panel nil t)
            (activity-panel-initialize status-window file-window))
          ;; Return to file window
          (select-window file-window))))))

(defun mp--teardown-autopilot (session)
  "Teardown Autopilot mode, cleaning up Activity Panel.
SESSION is the current mcp-session."
  (when (fboundp 'activity-panel-cleanup)
    (activity-panel-cleanup))
  (setq mp--autopilot-file-window nil))

;;; ============================================================
;;; Feat Tab Registration
;;; ============================================================

(mp-define-feat-tab autopilot
  :name "Autopilot"
  :key "a"
  :icon ""
  :setup #'mp--setup-autopilot
  :teardown #'mp--teardown-autopilot)

(provide 'panel-autopilot)

;;; autopilot.el ends here
