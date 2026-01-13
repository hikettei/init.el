;;; hikettei.el --- Hikettei Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; 手動コーディングモード - AI Codingが解けなさそうな問題を手作業でコーディング
;; C-c SPC: Set mark / Save region (1回目でマーク、2回目で保存)
;; C-c a: Ask AI
;; C-c i: Impl AI (実装依頼)
;; C-c r: Review AI (コードレビュー)
;;
;; Neotree でファイルをクリックすると、自動的に Hikettei パネルに切り替わり、
;; ファイルが WorkArea に表示される。

;;; Code:

(require 'multi-panel)

;;; ============================================================
;;; Hikettei Minor Mode
;;; ============================================================

(defvar mp-hikettei-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a") #'mp-hikettei-ask)
    (define-key map (kbd "C-c i") #'mp-hikettei-impl)
    (define-key map (kbd "C-c r") #'mp-hikettei-review)
    (define-key map (kbd "C-c SPC") #'mp-hikettei-set-region)
    map)
  "Keymap for Hikettei mode.")

;; Global region info (not buffer-local so it persists across buffers)
(defvar mp-hikettei--region-info nil
  "Plist containing selected region info: :file :start-line :end-line :content.")

(defvar-local mp-hikettei--region-overlay nil
  "Overlay for highlighting selected region.")

(defun mp-hikettei-set-region ()
  "Set mark or save region for AI operations.
If no region is active, set mark at point (like C-SPC).
If region is active, save the region info for AI operations."
  (interactive)
  (if (use-region-p)
      ;; Region is active - save it
      (let* ((beg (region-beginning))
             (end (region-end))
             (start-line (line-number-at-pos beg))
             (end-line (line-number-at-pos end))
             (file-name (or (buffer-file-name) (buffer-name)))
             (content (buffer-substring-no-properties beg end)))
        ;; Save region info globally
        (setq mp-hikettei--region-info
              (list :file file-name
                    :start-line start-line
                    :end-line end-line
                    :content content))
        ;; Create visual overlay
        (when mp-hikettei--region-overlay
          (delete-overlay mp-hikettei--region-overlay))
        (setq mp-hikettei--region-overlay (make-overlay beg end))
        (overlay-put mp-hikettei--region-overlay 'face '(:background "#44475a"))
        (overlay-put mp-hikettei--region-overlay 'evaporate t)
        (deactivate-mark)
        (message "Region saved: %s L%d-%d (%d chars)"
                 (file-name-nondirectory file-name) start-line end-line (length content)))
    ;; No region - set mark (same as C-SPC)
    (push-mark (point) t t)
    (message "Mark set. Select text then C-c SPC again to save region.")))


(defun mp-hikettei--get-region-info ()
  "Get region info from active region or saved region.
Returns plist with :file :start-line :end-line :content, or nil."
  (cond
   ;; First priority: active region
   ((use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (start-line (line-number-at-pos beg))
           (end-line (line-number-at-pos end))
           (file-name (or (buffer-file-name) (buffer-name)))
           (content (buffer-substring-no-properties beg end)))
      (list :file file-name
            :start-line start-line
            :end-line end-line
            :content content)))
   ;; Second priority: saved region
   (mp-hikettei--region-info
    mp-hikettei--region-info)
   ;; No region
   (t nil)))

(defun mp-hikettei--format-region-context ()
  "Format the selected region with file and line context."
  (when-let ((info (mp-hikettei--get-region-info)))
    (let ((file (plist-get info :file))
          (start (plist-get info :start-line))
          (end (plist-get info :end-line))
          (content (plist-get info :content)))
      (format "File: %s (lines %d-%d)\n```\n%s\n```"
              (abbreviate-file-name file) start end content))))

(defun mp--send-to-ai-chat (text)
  "Send TEXT to AI Chat buffer."
  (let ((chat-buf (or (get-buffer "*AI Chat*")
                      (and (boundp 'mp--ai-chat-buffer)
                           (buffer-live-p mp--ai-chat-buffer)
                           mp--ai-chat-buffer))))
    (when chat-buf
      (with-current-buffer chat-buf
        (when (fboundp 'vterm-send-string)
          (vterm-send-string text)
          (vterm-send-return))))))

(defun mp-hikettei-ask ()
  "Ask AI about selected region."
  (interactive)
  (let ((question (read-string "Ask AI: "))
        (context (mp-hikettei--format-region-context)))
    (if context
        (mp--send-to-ai-chat
         (format "Question about this code:\n%s\n\n%s" context question))
      (mp--send-to-ai-chat question))))

(defun mp-hikettei-impl ()
  "Request AI to implement code for selected region."
  (interactive)
  (let* ((info (mp-hikettei--get-region-info))
         (instruction (read-string "Implementation request: "))
         (context (mp-hikettei--format-region-context)))
    (if (and context info)
        (mp--send-to-ai-chat
         (format "Please implement the following. IMPORTANT: Only modify the code around lines %d-%d in %s. Do not change other parts of the file.\n\n%s\n\nInstructions: %s"
                 (plist-get info :start-line)
                 (plist-get info :end-line)
                 (file-name-nondirectory (plist-get info :file))
                 context
                 instruction))
      (mp--send-to-ai-chat (format "Please implement: %s" instruction)))))

(defun mp-hikettei-review ()
  "Request AI to review selected region."
  (interactive)
  (let ((context (mp-hikettei--format-region-context)))
    (if context
        (mp--send-to-ai-chat
         (format "Please review this code:\n%s" context))
      (message "No region selected. Use C-c SPC to set region first."))))

(defvar mp-hikettei--header-line
  (concat
   (propertize " Hikettei " 'face '(:foreground "#282a36" :background "#50fa7b" :weight bold))
   " "
   (propertize "C-c SPC" 'face '(:foreground "#ff79c6" :weight bold))
   (propertize ":Mark/Save " 'face '(:foreground "#6272a4"))
   (propertize "C-c a" 'face '(:foreground "#ff79c6" :weight bold))
   (propertize ":Ask " 'face '(:foreground "#6272a4"))
   (propertize "C-c i" 'face '(:foreground "#ff79c6" :weight bold))
   (propertize ":Impl " 'face '(:foreground "#6272a4"))
   (propertize "C-c r" 'face '(:foreground "#ff79c6" :weight bold))
   (propertize ":Review" 'face '(:foreground "#6272a4")))
  "Header line content for Hikettei mode.")

(define-minor-mode mp-hikettei-mode
  "Minor mode for manual coding with AI assistance."
  :lighter " Hikettei"
  :keymap mp-hikettei-mode-map
  (if mp-hikettei-mode
      (setq-local header-line-format mp-hikettei--header-line)
    (setq-local header-line-format nil)))

;;; ============================================================
;;; Neotree Integration
;;; ============================================================

(defun mp-hikettei--find-workarea-window ()
  "Find the actual workarea window (not neotree, ai-chat, or feat-tab-bar)."
  (let ((excluded (list mp--neotree-window mp--ai-chat-window mp--feat-tab-bar-window)))
    (cl-find-if (lambda (win)
                  (and (not (memq win excluded))
                       (not (window-dedicated-p win))
                       (not (window-parameter win 'window-side))))
                (window-list nil 'nomini))))

(defun mp-hikettei--neo-open-file-advice (orig-fn full-path &optional arg)
  "Advice for neo-open-file to open files in Hikettei panel.
When in multi-panel mode, switch to Hikettei panel and open the file there."
  (if (and (boundp 'mp--current-feat-tab)
           mp--current-feat-tab)
      ;; We're in multi-panel mode
      (progn
        ;; Properly switch to hikettei tab (runs teardown/setup)
        (unless (eq mp--current-feat-tab 'hikettei)
          (mp-switch-to 'hikettei))
        ;; Now open file in workarea window
        (let ((workarea-win (mp-hikettei--find-workarea-window)))
          (when workarea-win
            (select-window workarea-win)
            (find-file full-path)
            ;; Enable hikettei mode
            (mp-hikettei-mode 1))))
    ;; Not in multi-panel mode, use default behavior
    (funcall orig-fn full-path arg)))

;; Add advice when neotree is loaded
(with-eval-after-load 'neotree
  (advice-add 'neo-open-file :around #'mp-hikettei--neo-open-file-advice))

;;; ============================================================
;;; Panel Setup
;;; ============================================================

(defun mp--setup-hikettei (session)
  "Setup Hikettei mode - manual coding with AI assist.
Shows a helpful welcome buffer with keybinding hints."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    (let* ((workspace (if (and session (fboundp 'ai-session-workspace))
                          (ai-session-workspace session)
                        default-directory))
           (default-directory workspace)
           (buf (get-buffer-create "*Hikettei*")))
      (switch-to-buffer buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "\n  Hikettei Mode\n" 
                              'face '(:foreground "#50fa7b" :weight bold :height 1.3)))
          (insert (propertize "  Manual Coding with AI Assistance\n\n" 
                              'face '(:foreground "#6272a4")))
          (insert (propertize "  ─────────────────────────────────────────────\n\n" 
                              'face '(:foreground "#44475a")))
          (insert "  ")
          (insert (propertize "Click files in NeoTree" 'face '(:foreground "#f1fa8c")))
          (insert " to open them here.\n\n")
          ;; Keybindings section
          (insert (propertize "  Keybindings:\n" 'face '(:foreground "#bd93f9" :weight bold)))
          (insert "\n")
          (insert "    ")
          (insert (propertize "C-c SPC" 'face '(:foreground "#ff79c6" :weight bold)))
          (insert "  Set mark, then select text, press again to save\n")
          (insert "    ")
          (insert (propertize "C-c a  " 'face '(:foreground "#ff79c6" :weight bold)))
          (insert "  Ask AI about code/region\n")
          (insert "    ")
          (insert (propertize "C-c i  " 'face '(:foreground "#ff79c6" :weight bold)))
          (insert "  Request AI implementation\n")
          (insert "    ")
          (insert (propertize "C-c r  " 'face '(:foreground "#ff79c6" :weight bold)))
          (insert "  Request AI code review\n")
          (insert "\n")
          ;; Window operations section
          (insert (propertize "  Window operations:\n" 'face '(:foreground "#bd93f9" :weight bold)))
          (insert "\n")
          (insert "    ")
          (insert (propertize "C-x 2  " 'face '(:foreground "#8be9fd")))
          (insert "  Split window horizontally\n")
          (insert "    ")
          (insert (propertize "C-x 3  " 'face '(:foreground "#8be9fd")))
          (insert "  Split window vertically\n")
          (insert "    ")
          (insert (propertize "C-x 0  " 'face '(:foreground "#8be9fd")))
          (insert "  Close current window\n")
          (insert "\n")
          (insert (propertize "  ─────────────────────────────────────────────\n" 
                              'face '(:foreground "#44475a")))
          (insert (propertize "\n  Workspace: " 'face '(:foreground "#6272a4")))
          (insert (propertize (abbreviate-file-name workspace) 'face '(:foreground "#50fa7b")))
          (insert "\n"))
        (special-mode)
        (setq-local mode-line-format nil))
      (message "Hikettei mode: C-c SPC=set region, C-c a=ask, C-c i=impl, C-c r=review"))))

(mp-define-feat-tab hikettei
  :name "Hikettei"
  :key "f"
  :icon ""
  :setup #'mp--setup-hikettei)

(provide 'panel-hikettei)

;;; hikettei.el ends here
