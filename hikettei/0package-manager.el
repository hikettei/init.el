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
  :custom
  (vterm-max-scrollback 10000)
  (vterm-timer-delay nil)
  (vterm-always-compile-module t)
  (vterm-shell "/bin/zsh")
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
  ;; Fix vterm color handling (workaround for broken commit)
  (defun old-version-of-vterm--get-color (index &rest args)
    "Old version of vterm--get-color before it was broken.
Re-introducing the old version fixes auto-dim-other-buffers for vterm buffers."
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
