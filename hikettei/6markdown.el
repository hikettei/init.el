(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (custom-set-variables
   '(markdown-command '("pandoc" "--from=markdown" "--to=html5"))
   '(markdown-fontify-code-blocks-natively t)
   '(markdown-header-scaling nil)
   '(markdown-indent-on-enter 'indent-and-new-item))
  (custom-set-faces
   '(markdown-bold-face ((t (:foreground "#f0f8ff" :inherit bold))))
   '(markdown-header-face ((t (:foreground "#87cefa" :inherit bold))))
   '(markdown-link-face ((t (:foreground "#c0c0c0"))))
   '(markdown-list-face ((t (:foreground "#ff7f50")))))
  (define-key markdown-mode-map (kbd "<S-tab>") #'markdown-shifttab))
