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

;; vterm - Fast terminal emulator
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-timer-delay nil)
  (setq vterm-always-compile-module t)
  (setq vterm-shell "/bin/zsh"))
