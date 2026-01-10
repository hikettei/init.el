;;; explore.el --- Explore Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Google in EAF Browser

;;; Code:

(require 'multi-panel)

(defcustom mp-explore-url "https://www.google.com"
  "URL for Explore panel."
  :type 'string
  :group 'multi-panel)

(defun mp--setup-explore (session)
  "Setup Explore mode - Google in EAF browser."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    (cond
     ((fboundp 'eaf-open-browser)
      (eaf-open-browser mp-explore-url))
     ((fboundp 'xwidget-webkit-browse-url)
      (xwidget-webkit-browse-url mp-explore-url))
     (t
      (browse-url mp-explore-url)))))

(mp-define-feat-tab explore
  :name "Explore"
  :key "r"
  :icon ""
  :setup #'mp--setup-explore)

(provide 'panel-explore)

;;; explore.el ends here
