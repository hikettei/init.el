;;; 7eaf.el --- EAF (Emacs Application Framework) Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; EAF provides a way to run GUI applications inside Emacs.
;; Install with: make install-eaf
;;

;;; Code:

(defconst eaf-install-dir "~/.emacs.d/site-lisp/eaf"
  "Directory where EAF is installed.")

(defconst eaf-browser-dir (expand-file-name "app/browser" eaf-install-dir)
  "Directory where EAF browser app is installed.")

(defun eaf-browse-url (url)
  "Open URL in EAF browser."
  (interactive "sURL: ")
  (if (fboundp 'eaf-open-browser)
      (eaf-open-browser url)
    (error "EAF browser not available. Run 'make install-eaf' to install")))

(global-set-key (kbd "C-c b") 'eaf-browse-url)

;; Load EAF if installed
(if (not (file-exists-p (expand-file-name "eaf.el" eaf-install-dir)))
    (message "EAF not installed. Run 'make install-eaf' to install")
  (if (not (file-exists-p (expand-file-name "eaf-browser.el" eaf-browser-dir)))
      (message "EAF browser app not installed. Run 'make install-eaf' to install")
    ;; EAF and browser are installed, load them
    (add-to-list 'load-path eaf-install-dir)
    (add-to-list 'load-path eaf-browser-dir)

    (condition-case err
        (progn
          (require 'eaf)
          (require 'eaf-browser)

          ;; Browser settings
          (setq eaf-browser-continue-where-left-off t)
          (setq eaf-browser-enable-adblocker t)
          (setq eaf-browser-chrome-browser-name "Google Chrome")

          ;; Set EAF browser as default
          (setq browse-url-browser-function 'eaf-open-browser))
      (error
       (message "EAF failed to load: %s" (error-message-string err))))))

(provide '7eaf)

;;; 7eaf.el ends here
