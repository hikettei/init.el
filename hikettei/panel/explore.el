;;; explore.el --- Explore Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Google in EAF Browser

;;; Code:

(require 'multi-panel)

(defcustom mp-explore-url "https://www.google.com"
  "URL for Explore panel."
  :type 'string
  :group 'multi-panel)

(defun mp--setup-explore (session)
  "Setup Explore mode - reuse existing webkit or open new browser."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    ;; Try to find existing xwidget-webkit buffer first
    (let ((existing-webkit-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (when (with-current-buffer buf
                       (derived-mode-p 'xwidget-webkit-mode))
                 (throw 'found buf))))))
      (if existing-webkit-buf
          ;; Reuse existing webkit buffer
          (switch-to-buffer existing-webkit-buf)
        ;; No existing webkit, open new browser
        (cond
         ((fboundp 'eaf-open-browser)
          (eaf-open-browser mp-explore-url))
         ((fboundp 'xwidget-webkit-browse-url)
          (xwidget-webkit-browse-url mp-explore-url))
         (t
          (browse-url mp-explore-url)))))
    ;; Disable line numbers
    (let ((buf (current-buffer)))
      (run-at-time 0.1 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (display-line-numbers-mode -1))))))))

(mp-define-feat-tab explore
  :name "Explore"
  :key "r"
  :icon ""
  :setup #'mp--setup-explore)

(provide 'panel-explore)

;;; explore.el ends here
