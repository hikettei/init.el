;;; git.el --- Git Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Git用ターミナル

;;; Code:

(require 'multi-panel)

(defun mp--setup-git (session)
  "Setup Git mode - vterm for git operations."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    (let* ((workspace (if (and session (fboundp 'ai-session-workspace))
                          (ai-session-workspace session)
                        default-directory))
           (buf-name "*Git Terminal*"))
      (if (get-buffer buf-name)
          (switch-to-buffer buf-name)
        (if (fboundp 'vterm)
            (progn
              (vterm)
              (rename-buffer buf-name t)
              (run-at-time 0.1 nil
                           (lambda (ws)
                             (when (and (boundp 'vterm--process) vterm--process)
                               (vterm-send-string (format "cd %s && git status\n"
                                                          (shell-quote-argument ws)))))
                           workspace))
          (term "/bin/zsh"))))))

(mp-define-feat-tab git
  :name "Git"
  :key "q"
  :icon ""
  :setup #'mp--setup-git)

(provide 'panel-git)

;;; git.el ends here
