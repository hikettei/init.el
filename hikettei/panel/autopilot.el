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

;; Ensure mcp directory is in load-path for activity-panel
(let ((mcp-dir (expand-file-name "mcp" (file-name-directory load-file-name))))
  (when (file-directory-p mcp-dir)
    (add-to-list 'load-path mcp-dir)))

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

(defvar mp--autopilot-saved-revert-settings nil
  "Saved auto-revert settings to restore on teardown.")

;;; ============================================================
;;; Status Buffer
;;; ============================================================

(defun mp--create-autopilot-status-buffer ()
  "Create or get the Autopilot status buffer."
  (let ((buf (get-buffer-create "*Autopilot Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
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
  ;; Save current auto-revert settings and enable silent auto-revert
  (setq mp--autopilot-saved-revert-settings
        (list (cons 'global-auto-revert-mode global-auto-revert-mode)
              (cons 'auto-revert-verbose auto-revert-verbose)
              (cons 'auto-revert-check-vc-info auto-revert-check-vc-info)))
  (setq auto-revert-verbose nil)
  (setq auto-revert-check-vc-info nil)
  (global-auto-revert-mode 1)
  
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
          (require 'activity-panel nil t)
          (when (fboundp 'activity-panel-initialize)
            (activity-panel-initialize status-window file-window))
          ;; Return to file window
          (select-window file-window))))))

(defun mp--teardown-autopilot (session)
  "Teardown Autopilot mode, cleaning up Activity Panel.
SESSION is the current mcp-session."
  ;; Restore auto-revert settings
  (when mp--autopilot-saved-revert-settings
    (let ((saved-global (alist-get 'global-auto-revert-mode mp--autopilot-saved-revert-settings))
          (saved-verbose (alist-get 'auto-revert-verbose mp--autopilot-saved-revert-settings))
          (saved-vc (alist-get 'auto-revert-check-vc-info mp--autopilot-saved-revert-settings)))
      (setq auto-revert-verbose saved-verbose)
      (setq auto-revert-check-vc-info saved-vc)
      (unless saved-global
        (global-auto-revert-mode -1)))
    (setq mp--autopilot-saved-revert-settings nil))
  
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
