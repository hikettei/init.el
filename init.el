(defun include (name)
  (let ((pathname (concat (file-name-as-directory "~/.emacs.d/hikettei/") name)))
    (format "Loading %s...\n" pathname)
    (if (file-exists-p pathname)
        (load-file pathname)
        (message "File %s does not exist." pathname))))

(include "0package-manager.el") ;; Setup Elpaca and use-package
(include "1multi-term.el")      ;; Multi-term setup
(include "2theme.el")           ;; Doom-Theme, Fonts, Configuration
(include "3autocomp.el")        ;; Auto Completions
(include "4common-lisp.el")     ;; Emacs as a Common Lisp IDE
(include "5python.el")          ;; Python Development Environment
(include "6markdown.el")        ;; Markdown Editing Environment
(include "7eaf.el")             ;; Emacs Application Framework Setup
;(include "8claude-code.el")     ;; Claude Code Configurations
(include "mcp/file-editor.el")  ;; MCP File Editor Review UI
(include "multi-panel.el")      ;; Multi-Panel WorkArea System
(include "mcp-session.el")      ;; AI Session Management
(include "style.el")            ;; Style Configuration
(include "keybindings.el")      ;; Keybindings for Emacs
(include "session-wizard.el")   ;; Startup Screen & Session Creation
;; ↑ Open Browser Graphically!
;; Using Discord from Emacs is possible?
;; Emacs Across Multiple Desktop Window?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(use-package-hydra blackout hydra leaf-keywords el-get pandoc-mode pandoc markdown-mode markdown-preview-mode jedi haskell-mode readline-complete py-isort nerd-icons page-break-lines dashboard ivy rust-mode slime-theme slime-repl-ansi-color highlight color-identifiers-mode clang-format+ swift3-mode swift-mode elpy better-defaults elcord all-the-icons-gnus nyan-mode cnfonts minimap beacon lsp-mode ccls ivy-rich counsel amx which-key hide-mode-line doom-modeline doom-themes ztree use-package neotree all-the-icons-ivy all-the-icons-dired)))

(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282a36" :foreground "#dcdcdc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "nil" :family "Monaco"))))
 '(doom-modeline-bar ((t (:background "#6272a4"))) t)
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
