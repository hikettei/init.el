;;; freeform.el --- Freeform Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO.mdエディタ - .hikettei/TODO.md を編集

;;; Code:

(require 'multi-panel)

(defun mp--setup-freeform (session)
  "Setup Freeform mode - TODO.md editor."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    (let* ((workspace (if (and session (fboundp 'ai-session-workspace))
                          (ai-session-workspace session)
                        default-directory))
           (todo-file (expand-file-name ".hikettei/TODO.md" workspace)))
      ;; Ensure directory exists
      (make-directory (file-name-directory todo-file) t)
      ;; Create file if not exists
      (unless (file-exists-p todo-file)
        (with-temp-file todo-file
          (insert "# TODO\n\n- [ ] \n")))
      ;; Open file
      (find-file todo-file)
      (when (fboundp 'markdown-mode)
        (markdown-mode)))))

(mp-define-feat-tab freeform
  :name "Freeform"
  :key "s"
  :icon ""
  :setup #'mp--setup-freeform)

(provide 'panel-freeform)

;;; freeform.el ends here
