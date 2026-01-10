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
(let* ((this-file (or load-file-name buffer-file-name))
       (panel-dir (and this-file (file-name-directory this-file)))
       (hikettei-dir (and panel-dir (file-name-directory (directory-file-name panel-dir))))
       (mcp-dir (and hikettei-dir (expand-file-name "mcp" hikettei-dir))))
  (when (and mcp-dir (file-directory-p mcp-dir))
    (add-to-list 'load-path mcp-dir)))

;; Now require activity-panel
(require 'activity-panel)
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
  "Setup Autopilot mode - single window for AI file display.
SESSION is the current mcp-session."
  ;; Ensure autorevert is loaded
  (require 'autorevert nil t)
  ;; Save current auto-revert settings and enable silent auto-revert
  (setq mp--autopilot-saved-revert-settings
        (list (cons 'global-auto-revert-mode (and (boundp 'global-auto-revert-mode) global-auto-revert-mode))
              (cons 'auto-revert-verbose (and (boundp 'auto-revert-verbose) auto-revert-verbose))
              (cons 'auto-revert-check-vc-info (and (boundp 'auto-revert-check-vc-info) auto-revert-check-vc-info))))
  (setq auto-revert-verbose nil)
  (setq auto-revert-check-vc-info nil)
  (global-auto-revert-mode 1)
  
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    ;; Single window for file display
    (setq mp--autopilot-file-window (selected-window))
    
    ;; Check for pending review
    (if (and (boundp 'mp--review-pending) mp--review-pending)
        ;; Display pending review
        (let* ((pending-session mp--review-pending)
               (buffer (file-editor-session-file-buffer pending-session))
               (start-line (file-editor-session-start-line pending-session)))
          ;; Clear pending flag first
          (setq mp--review-pending nil)
          (mp--update-feat-tab-bar)
          ;; Display if buffer is live
          (when (buffer-live-p buffer)
            (set-window-buffer mp--autopilot-file-window buffer)
            (with-selected-window mp--autopilot-file-window
              (goto-char (point-min))
              (forward-line (1- start-line))
              (recenter 3))))
      ;; No pending review - show default buffer
      (switch-to-buffer (get-buffer-create "*Autopilot*"))
      (with-current-buffer "*Autopilot*"
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "\n  Autopilot Mode\n\n" 'face '(:foreground "#50fa7b" :weight bold)))
          (insert "  Waiting for AI file access...\n\n")
          (insert "  Files read or edited by AI will appear here.\n"))
        (special-mode)))))

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
