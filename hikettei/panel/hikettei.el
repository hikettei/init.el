;;; hikettei.el --- Hikettei Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; 手動コーディングモード - AI Codingが解けなさそうな問題を手作業でコーディング
;; C-c a: Ask AI
;; C-c i: Impl AI
;; C-c r: Review AI
;; C-c SPC: Set region

;;; Code:

(require 'multi-panel)

;;; ============================================================
;;; Hikettei Minor Mode
;;; ============================================================

(defvar mp-hikettei-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a") #'mp-hikettei-ask)
    (define-key map (kbd "C-c i") #'mp-hikettei-impl)
    (define-key map (kbd "C-c r") #'mp-hikettei-review)
    (define-key map (kbd "C-c SPC") #'mp-hikettei-set-region)
    map)
  "Keymap for Hikettei mode.")

(defvar-local mp-hikettei--selected-region nil
  "Selected region for AI operations.")

(defun mp-hikettei-set-region ()
  "Set the selected region for AI operations."
  (interactive)
  (if (use-region-p)
      (progn
        (setq mp-hikettei--selected-region
              (buffer-substring-no-properties (region-beginning) (region-end)))
        (message "Region set (%d chars)" (length mp-hikettei--selected-region)))
    (message "No region selected")))

(defun mp--send-to-ai-chat (text)
  "Send TEXT to AI Chat buffer."
  (when (and (boundp 'ai-session--current) ai-session--current)
    (let ((chat-buf (get-buffer (format "*AI Chat: %s*"
                                        (ai-session-title ai-session--current)))))
      (when (and chat-buf (buffer-live-p chat-buf))
        (with-current-buffer chat-buf
          (when (fboundp 'vterm-send-string)
            (vterm-send-string text)
            (vterm-send-return)))))))

(defun mp-hikettei-ask ()
  "Ask AI about selected region."
  (interactive)
  (let ((question (read-string "Ask AI: ")))
    (if mp-hikettei--selected-region
        (mp--send-to-ai-chat
         (format "Question about this code:\n```\n%s\n```\n\n%s"
                 mp-hikettei--selected-region question))
      (mp--send-to-ai-chat question))))

(defun mp-hikettei-impl ()
  "Request AI to implement code for selected region."
  (interactive)
  (let ((instruction (read-string "Implementation request: ")))
    (if mp-hikettei--selected-region
        (mp--send-to-ai-chat
         (format "Please implement the following:\n```\n%s\n```\n\nInstructions: %s"
                 mp-hikettei--selected-region instruction))
      (mp--send-to-ai-chat (format "Please implement: %s" instruction)))))

(defun mp-hikettei-review ()
  "Request AI to review selected region."
  (interactive)
  (if mp-hikettei--selected-region
      (mp--send-to-ai-chat
       (format "Please review this code:\n```\n%s\n```"
               mp-hikettei--selected-region))
    (message "No region selected. Use C-c SPC to set region first.")))

(define-minor-mode mp-hikettei-mode
  "Minor mode for manual coding with AI assistance."
  :lighter " Hikettei"
  :keymap mp-hikettei-mode-map)

;;; ============================================================
;;; Panel Setup
;;; ============================================================

(defun mp--setup-hikettei (session)
  "Setup Hikettei mode - manual coding with AI assist."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    (let* ((workspace (if (and session (fboundp 'ai-session-workspace))
                          (ai-session-workspace session)
                        default-directory))
           (default-directory workspace))
      (scratch-buffer)
      (mp-hikettei-mode 1)
      (message "Hikettei mode: C-c SPC=set region, C-c a=ask, C-c i=impl, C-c r=review"))))

(mp-define-feat-tab hikettei
  :name "Hikettei"
  :key "f"
  :icon ""
  :setup #'mp--setup-hikettei)

(provide 'panel-hikettei)

;;; hikettei.el ends here
