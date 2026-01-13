;;; explore.el --- Explore Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; WebKit Browser Panel for web exploration

;;; Code:

(require 'multi-panel)

(defcustom mp-explore-url "https://www.google.com"
  "URL for Explore panel."
  :type 'string
  :group 'multi-panel)

(defvar mp--explore-webkit-buffer nil
  "Current webkit buffer for Explore panel.")

(defun mp--setup-explore (session)
  "Setup Explore mode - reuse existing webkit or open new browser."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    ;; Try to find existing xwidget-webkit buffer first
    (let ((existing-webkit-buf
           (or (and mp--explore-webkit-buffer
                    (buffer-live-p mp--explore-webkit-buffer)
                    mp--explore-webkit-buffer)
               (catch 'found
                 (dolist (buf (buffer-list))
                   (when (with-current-buffer buf
                           (derived-mode-p 'xwidget-webkit-mode))
                     (throw 'found buf)))))))
      (if existing-webkit-buf
          ;; Reuse existing webkit buffer
          (progn
            (setq mp--explore-webkit-buffer existing-webkit-buf)
            (switch-to-buffer existing-webkit-buf))
        ;; No existing webkit, open new browser
        (cond
         ((fboundp 'xwidget-webkit-browse-url)
          (xwidget-webkit-browse-url mp-explore-url)
          ;; Store the new webkit buffer after creation
          (run-at-time 0.5 nil
                       (lambda ()
                         (when-let ((buf (catch 'found
                                           (dolist (b (buffer-list))
                                             (when (with-current-buffer b
                                                     (derived-mode-p 'xwidget-webkit-mode))
                                               (throw 'found b))))))
                           (setq mp--explore-webkit-buffer buf)))))
         ((fboundp 'eaf-open-browser)
          (eaf-open-browser mp-explore-url))
         (t
          (browse-url mp-explore-url)))))
    ;; Disable line numbers
    (let ((buf (current-buffer)))
      (run-at-time 0.1 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (display-line-numbers-mode -1))))))))

(defun mp--teardown-explore (_session)
  "Teardown Explore panel - clear saved state to ensure fresh setup."
  ;; Clear saved workarea state - always run fresh setup
  (when-let ((tab (gethash 'explore mp--feat-tabs)))
    (setf (mp-feat-tab-workarea-state tab) nil)))

(mp-define-feat-tab explore
  :name "Explore"
  :key "r"
  :icon ""
  :setup #'mp--setup-explore
  :teardown #'mp--teardown-explore)

(provide 'panel-explore)

;;; explore.el ends here