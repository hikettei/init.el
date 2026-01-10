;; [TODO] Use setup.el + Elpaca
(require 'package)
(require 'use-package)
;; Preparing MELPA
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("org" . "http://orgmode.org/elpa/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; web-server - HTTP server for MCP integration
(use-package web-server
  :ensure t)

;; vterm - Fast terminal emulator (iTerm2 Pro theme)
(use-package vterm
  :ensure t
  :init
  (setq vterm-shell "/bin/zsh")
  :custom
  (vterm-max-scrollback 10000)
  (vterm-timer-delay nil)
  (vterm-always-compile-module t)
  (vterm-term-environment-variable "xterm-256color")
  (vterm-copy-mode-remove-fake-newlines t)
  :custom-face
  ;; iTerm2 Pro theme colors
  (vterm-color-default ((t (:background "#1e1e1e" :foreground "#e4e4e4"))))
  (vterm-color-black ((t (:foreground "#000000" :background "#555555"))))
  (vterm-color-red ((t (:foreground "#bb0000" :background "#ff5555"))))
  (vterm-color-green ((t (:foreground "#00bb00" :background "#55ff55"))))
  (vterm-color-yellow ((t (:foreground "#bbbb00" :background "#ffff55"))))
  (vterm-color-blue ((t (:foreground "#6a85ff" :background "#5d65dc"))))
  (vterm-color-magenta ((t (:foreground "#bb00bb" :background "#ff55ff"))))
  (vterm-color-cyan ((t (:foreground "#00bbbb" :background "#55ffff"))))
  (vterm-color-white ((t (:foreground "#bbbbbb" :background "#ffffff"))))
  :hook
  (vterm-mode . (lambda ()
                  (display-line-numbers-mode -1)
                  (face-remap-add-relative 'default :background "#1e1e1e" :foreground "#e4e4e4")))
  :config
  ;; Pass these keys to Emacs instead of terminal
  (define-key vterm-mode-map (kbd "M-x") #'execute-extended-command)
  (define-key vterm-mode-map (kbd "C-x") nil)
  (define-key vterm-mode-map (kbd "C-h") nil)

  ;; C-SPC: Start mark and auto-enter copy-mode
  (define-key vterm-mode-map (kbd "C-SPC")
              (lambda ()
                (interactive)
                (vterm-copy-mode 1)
                (set-mark-command nil)))

  ;; M-w: Copy region (or enter copy-mode if no region)
  (define-key vterm-mode-map (kbd "M-w")
              (lambda ()
                (interactive)
                (if (use-region-p)
                    (progn
                      (kill-ring-save (region-beginning) (region-end))
                      (vterm-copy-mode -1)
                      (message "Copied"))
                  (vterm-copy-mode 1))))

  ;; C-w: Kill region (or send C-w to terminal if no region)
  (define-key vterm-mode-map (kbd "C-w")
              (lambda ()
                (interactive)
                (if (use-region-p)
                    (progn
                      (kill-region (region-beginning) (region-end))
                      (vterm-copy-mode -1))
                  (vterm-send-key "w" nil nil t))))

  ;; C-y: Paste from kill-ring
  (define-key vterm-mode-map (kbd "C-y") #'vterm-yank)

  ;; C-g: Cancel (exit copy-mode or send C-g to terminal)
  (define-key vterm-mode-map (kbd "C-g")
              (lambda ()
                (interactive)
                (if vterm-copy-mode
                    (vterm-copy-mode -1)
                  (vterm-send-key "g" nil nil t))))

  ;; Fix vterm color handling (workaround for broken commit)
  (defun old-version-of-vterm--get-color (index &rest args)
    "Old version of vterm--get-color before it was broken."
    (cond
     ((and (>= index 0) (< index 16))
      (face-foreground
       (elt vterm-color-palette index)
       nil 'default))
     ((= index -11)
      (face-foreground 'vterm-color-underline nil 'default))
     ((= index -12)
      (face-background 'vterm-color-inverse-video nil 'default))
     (t nil)))
  (advice-add 'vterm--get-color :override #'old-version-of-vterm--get-color))
