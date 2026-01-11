;;; discussion.el --- Discussion Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; GPT5.2 Pro in xwidget or EAF Browser

;;; Code:

(require 'multi-panel)

(defcustom mp-discussion-url "https://chat.openai.com"
  "URL for Discussion panel."
  :type 'string
  :group 'multi-panel)

(defun mp--setup-discussion (session)
  "Setup Discussion mode - GPT5.2 Pro in browser."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    (cond
     ((fboundp 'eaf-open-browser)
      (eaf-open-browser mp-discussion-url))
     ((fboundp 'xwidget-webkit-browse-url)
      (xwidget-webkit-browse-url mp-discussion-url)
      ;; Disable line numbers in xwidget buffer
      (let ((buf (current-buffer)))
        (run-at-time 0.1 nil
                     (lambda ()
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (display-line-numbers-mode -1)))))))
     (t
      (browse-url mp-discussion-url)))))

(mp-define-feat-tab discussion
  :name "Discussion"
  :key "e"
  :icon ""
  :setup #'mp--setup-discussion)

(provide 'panel-discussion)

;;; discussion.el ends here
