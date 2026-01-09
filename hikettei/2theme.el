;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Theme Configuration
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Extra icons provided by all-the-icons package, neotree may need them.
(use-package all-the-icons
  :ensure t
  :pin melpa
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package cnfonts
  :ensure t
  :after all-the-icons
  :hook (cnfonts-set-font-finish
         . (lambda (fontsizes-list)
             (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)))
  :config
  (cnfonts-enable))

(use-package doom-themes
  :pin melpa
  :ensure t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (setq doom-neotree-file-icons t)
  (setq doom-themes-neotree-file-icons t)
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  
  (doom-themes-neotree-config)
  ;; or for treemacs users
 ; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Using Rich Preference for Discord
(use-package elcord
  :ensure t
  :config
  (elcord-mode))

;; Running nyan-cat in the editor
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))

(use-package doom-modeline
  :ensure t
  :commands (doom-modeline-def-modeline)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline
    'main
    '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name debug minor-modes input-method major-mode process vcs
                ;; checker
                ))
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-buffer-encoding t))

(use-package org-bullets
  :ensure t
  :custom (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
  :hook (org-mode . org-bullets-mode))

(use-package beacon
  :ensure t
  :custom
  (beacon-color "yellow")
  :config
  (beacon-mode 1))

(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package nerd-icons :ensure t)
(use-package page-break-lines :ensure t)
(use-package projectile :ensure t)

(use-package neotree
  :pin melpa
  :ensure t
  :after (projectile doom-themes nerd-icons all-the-icons)
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme 'nerd)
  :bind
  ("<f9>" . neotree-projectile-toggle)
  ("\C-q" . neotree-toggle) ;; control + q neotree
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
         ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
	      (neotree-find file-name))))))
  :config
  (add-hook 'neotree-mode-hook
          #'(lambda ()
              (display-line-numbers-mode -1)))
  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "\\.fasl$" "~$" "^#.*#$" "\\.elc$"))
  (setq neo-show-hidden-files nil))
;; Rich M-x
(use-package amx
  :ensure t
  :config
  (amx-mode 1))

(when nil
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-c <prior>" . centaur-tabs-backward)
  ("C-c <next>"  . centaur-tabs-forward)
  ;;("C-c <left>" . centaur-tabs-move-current-tab-to-left)
  ;;("C-c <right>" . centaur-tabs-move-current-tab-to-right)
  :config
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-type 'all-the-icons)
  (setq centaur-tabs-set-bar 'under)
  ;; Note: If you're not using Spacmeacs, in order for the underline to display
  ;; correctly you must add the following line:
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-close-button "X")
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")))

(use-package vundo
  :ensure t
  :bind
  ("C-x u" . vundo)
  :config
  (setq vundo-window-max-height 10)
  (setq vundo-glyph-alist vundo-unicode-symbols)
  ;; (set-face-attribute 'vundo-default nil :family "Symbola")
  )

(use-package ultra-scroll
  ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of using vc
  :vc (:url "https://github.com/jdtsmith/ultra-scroll") ; For Emacs>=30
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Native Emacs UI Configuration
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(customize-set-variable 'scroll-bar-mode nil)
(setq scroll-bar-mode nil)
;; Now Included in Emacs 30!
(if (version<= "29.0.0" emacs-version)
    (use-package which-key
      :ensure t
      :diminish which-key-mode
      :hook (after-init . which-key-mode)))

(if (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      ;;(set-face-attribute 'line-number nil
      ;;                    :foreground "DarkOliveGreen"
      ;;                    :background "#131521")
      ;;(set-face-attribute 'line-number-current-line nil
      ;;                    :foreground "gold")
      ))
	
(cua-mode t)
(setq cua-enable-cua-keys nil) 

(tool-bar-mode -1)
(display-time-mode t)
(column-number-mode t)
;;(global-linum-mode t)
(setq inhibit-startup-message t) 
(setq initial-scratch-message "")
; (mac-auto-ascii-mode 1)
