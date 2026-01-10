;;; terminal.el --- Terminal Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; 汎用ターミナル

;;; Code:

(require 'multi-panel)

(defun mp--setup-terminal (session)
  "Setup Terminal mode - general vterm."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    (let* ((workspace (if (and session (fboundp 'ai-session-workspace))
                          (ai-session-workspace session)
                        default-directory))
           (buf-name "*Terminal*"))
      (if (get-buffer buf-name)
          (switch-to-buffer buf-name)
        (if (fboundp 'vterm)
            (progn
              (vterm)
              (rename-buffer buf-name t)
              (run-at-time 0.1 nil
                           (lambda (ws)
                             (when (and (boundp 'vterm--process) vterm--process)
                               (vterm-send-string (format "cd %s\n"
                                                          (shell-quote-argument ws)))))
                           workspace))
          (term "/bin/zsh"))))))

(mp-define-feat-tab terminal
  :name "Terminal"
  :key "w"
  :icon ""
  :setup #'mp--setup-terminal)

(provide 'panel-terminal)

;;; terminal.el ends here
