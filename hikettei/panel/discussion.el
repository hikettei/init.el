;;; discussion.el --- Discussion Panel - Expert Q&A Board -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Expert Q&A Board for asynchronous consultation with external AI experts.
;; Features collapsible entries, click-to-copy questions, and inline answer input.

;;; Code:

(require 'multi-panel)

;;; ============================================================
;;; Customization
;;; ============================================================

(defcustom mp-discussion-fallback-url "https://chat.openai.com"
  "Fallback URL when no questions are pending."
  :type 'string
  :group 'multi-panel)

(defface discussion-board-header
  '((t :inherit font-lock-keyword-face :height 1.3 :weight bold))
  "Face for discussion board header."
  :group 'multi-panel)

(defface discussion-board-title
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for entry title."
  :group 'multi-panel)

(defface discussion-board-id
  '((t :inherit font-lock-comment-face))
  "Face for question ID."
  :group 'multi-panel)

(defface discussion-board-pending
  '((t :inherit warning :weight bold))
  "Face for pending status."
  :group 'multi-panel)

(defface discussion-board-answered
  '((t :inherit success :weight bold))
  "Face for answered status."
  :group 'multi-panel)

(defface discussion-board-question-box
  '((t :inherit default :background "#1a1a2e" :extend t))
  "Face for question box."
  :group 'multi-panel)

(defface discussion-board-answer-box
  '((t :inherit default :background "#162447" :extend t))
  "Face for answer box."
  :group 'multi-panel)

(defface discussion-board-button
  '((t :inherit button :box (:line-width 2 :style released-button)))
  "Face for clickable buttons."
  :group 'multi-panel)

(defface discussion-board-arrow
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for collapse/expand arrows."
  :group 'multi-panel)

;;; ============================================================
;;; State
;;; ============================================================

(defvar discussion-board--buffer-name "*Discussion Board*")
(defvar discussion-board--current-file nil)
(defvar discussion-board--entries nil)
(defvar discussion-board--expanded (make-hash-table :test 'equal)
  "Hash table tracking which entries are expanded.")

;;; ============================================================
;;; Parsing
;;; ============================================================

(defun discussion-board--get-file-path ()
  "Get path to DISCUSSION.md in current workspace."
  (let ((workspace (or (bound-and-true-p mcp-server--project-root)
                       default-directory)))
    (expand-file-name ".hikettei/DISCUSSION.md" workspace)))

(defun discussion-board--parse-entries ()
  "Parse DISCUSSION.md and return list of Q&A entries."
  (let ((path (discussion-board--get-file-path))
        (entries '()))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (re-search-forward "^## \\(Q-[0-9]+\\)" nil t)
          (let ((id (match-string 1))
                title timestamp question context answer)
            ;; Get title
            (save-excursion
              (when (re-search-forward "^\\*\\*Title:\\*\\* \\(.+\\)$" nil t)
                (setq title (match-string 1))))
            ;; Get timestamp
            (when (re-search-forward "^\\*\\*Timestamp:\\*\\* \\(.+\\)$" nil t)
              (setq timestamp (match-string 1)))
            ;; Get question
            (when (re-search-forward "^### Question\n\n```\n" nil t)
              (let ((q-start (point)))
                (when (re-search-forward "^```$" nil t)
                  (setq question (buffer-substring-no-properties
                                  q-start (match-beginning 0))))))
            ;; Get context (optional)
            (save-excursion
              (when (re-search-forward "^### Context\n\n" nil t)
                (let ((c-start (point)))
                  (when (re-search-forward "^### Answer" nil t)
                    (setq context (string-trim
                                   (buffer-substring-no-properties
                                    c-start (match-beginning 0))))))))
            ;; Get answer
            (when (re-search-forward "^### Answer\n\n" nil t)
              (let ((a-start (point))
                    (a-end (or (save-excursion
                                 (when (re-search-forward "^---$" nil t)
                                   (match-beginning 0)))
                               (point-max))))
                (setq answer (string-trim
                              (buffer-substring-no-properties a-start a-end)))))
            (push `((id . ,id)
                    (title . ,(or title "Untitled Question"))
                    (timestamp . ,timestamp)
                    (question . ,question)
                    (context . ,context)
                    (answer . ,answer)
                    (pending . ,(or (null answer)
                                    (string-empty-p answer)
                                    (string-match-p "^_Awaiting response" answer))))
                  entries)))))
    (nreverse entries)))

;;; ============================================================
;;; File Operations
;;; ============================================================

(defun discussion-board--save-answer (entry-id answer)
  "Save ANSWER for ENTRY-ID to DISCUSSION.md."
  (let ((path (discussion-board--get-file-path)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (when (re-search-forward (format "^## %s" (regexp-quote entry-id)) nil t)
          (when (re-search-forward "^### Answer\n\n" nil t)
            (let ((answer-start (point))
                  (answer-end (or (save-excursion
                                    (when (re-search-forward "^---$" nil t)
                                      (match-beginning 0)))
                                  (point-max))))
              (delete-region answer-start answer-end)
              (goto-char answer-start)
              (insert answer "\n\n"))))
        (write-region (point-min) (point-max) path nil 'silent))
      (message "Answer saved for %s!" entry-id)
      (discussion-board-refresh))))

(defun discussion-board--edit-file ()
  "Open DISCUSSION.md for editing with easy return to board."
  (interactive)
  (let ((path (discussion-board--get-file-path)))
    (if (file-exists-p path)
        (progn
          (find-file path)
          (local-set-key (kbd "C-c C-c")
                         (lambda () (interactive)
                           (save-buffer)
                           (mp-switch-to 'discussion)))
          (local-set-key (kbd "C-c C-k")
                         (lambda () (interactive)
                           (revert-buffer t t)
                           (mp-switch-to 'discussion)))
          (message "C-c C-c: save & return, C-c C-k: cancel & return"))
      (message "No DISCUSSION.md file yet."))))

;;; ============================================================
;;; Actions
;;; ============================================================

(defun discussion-board--copy-question (question)
  "Copy QUESTION to clipboard."
  (kill-new question)
  (message "Question copied to clipboard!"))

(defun discussion-board--toggle-entry (id)
  "Toggle expanded state of entry with ID."
  (if (gethash id discussion-board--expanded)
      (remhash id discussion-board--expanded)
    (puthash id t discussion-board--expanded))
  (discussion-board-refresh))

(defun discussion-board--prompt-answer (entry-id current-answer)
  "Prompt for answer input for ENTRY-ID with CURRENT-ANSWER as default."
  (let* ((initial (if (and current-answer
                           (not (string-match-p "^_Awaiting" current-answer)))
                      current-answer
                    ""))
         (answer (read-string "Paste expert's answer: " initial)))
    (when (and answer (not (string-empty-p answer)))
      (discussion-board--save-answer entry-id answer))))

;;; ============================================================
;;; Rendering
;;; ============================================================

(defun discussion-board--insert-button (label action &optional face)
  "Insert clickable LABEL that runs ACTION."
  (insert-text-button label
                      'face (or face 'discussion-board-button)
                      'action action
                      'follow-link t))

(defun discussion-board--render-entry-header (entry)
  "Render collapsible header for ENTRY."
  (let* ((id (alist-get 'id entry))
         (title (alist-get 'title entry))
         (pending (alist-get 'pending entry))
         (expanded (gethash id discussion-board--expanded)))
    ;; Arrow
    (let ((arrow-start (point)))
      (insert (propertize (if expanded "▼ " "▶ ")
                          'face 'discussion-board-arrow))
      (add-text-properties arrow-start (point)
                           `(mouse-face highlight
                             help-echo "Click to expand/collapse"
                             keymap ,(let ((map (make-sparse-keymap)))
                                       (define-key map [mouse-1]
                                         (lambda () (interactive)
                                           (discussion-board--toggle-entry id)))
                                       (define-key map (kbd "RET")
                                         (lambda () (interactive)
                                           (discussion-board--toggle-entry id)))
                                       (define-key map (kbd "TAB")
                                         (lambda () (interactive)
                                           (discussion-board--toggle-entry id)))
                                       map))))
    ;; ID
    (insert (propertize (format "%s " id) 'face 'discussion-board-id))
    ;; Title (also clickable to toggle)
    (let ((title-start (point)))
      (insert (propertize (truncate-string-to-width title 50 nil nil "...")
                          'face 'discussion-board-title))
      (add-text-properties title-start (point)
                           `(mouse-face highlight
                             help-echo ,title
                             keymap ,(let ((map (make-sparse-keymap)))
                                       (define-key map [mouse-1]
                                         (lambda () (interactive)
                                           (discussion-board--toggle-entry id)))
                                       (define-key map (kbd "RET")
                                         (lambda () (interactive)
                                           (discussion-board--toggle-entry id)))
                                       map))))
    ;; Status badge
    (insert "  ")
    (if pending
        (insert (propertize "[pending]" 'face 'discussion-board-pending))
      (insert (propertize "[answered]" 'face 'discussion-board-answered)))
    (insert "\n")))

(defun discussion-board--render-entry-body (entry)
  "Render expanded body for ENTRY."
  (let* ((id (alist-get 'id entry))
         (question (alist-get 'question entry))
         (answer (alist-get 'answer entry))
         (pending (alist-get 'pending entry))
         (indent "    "))
    ;; Question box
    (insert indent)
    (insert (propertize "┌─ Question " 'face 'font-lock-comment-face))
    (discussion-board--insert-button
     "[Copy]"
     (lambda (_btn) (discussion-board--copy-question question)))
    (insert (propertize " ─────────────────────────────────────────┐\n"
                        'face 'font-lock-comment-face))
    ;; Question content
    (let ((q-lines (split-string (or question "") "\n")))
      (dolist (line (seq-take q-lines 10))  ; Show max 10 lines
        (insert indent)
        (insert (propertize (format "│ %s\n" (truncate-string-to-width line 60 nil nil "..."))
                            'face 'discussion-board-question-box)))
      (when (> (length q-lines) 10)
        (insert indent)
        (insert (propertize (format "│ ... (%d more lines)\n" (- (length q-lines) 10))
                            'face 'font-lock-comment-face))))
    (insert indent)
    (insert (propertize "└──────────────────────────────────────────────────────────┘\n"
                        'face 'font-lock-comment-face))
    ;; Answer box
    (insert indent)
    (insert (propertize "┌─ Answer ────────────────────────────────────────────────┐\n"
                        'face 'font-lock-comment-face))
    ;; Answer content or input prompt
    (if pending
        (progn
          (insert indent)
          (insert (propertize "│ " 'face 'font-lock-comment-face))
          (discussion-board--insert-button
           "[Click to paste expert's answer]"
           (lambda (_btn) (discussion-board--prompt-answer id answer)))
          (insert "\n"))
      (let ((a-lines (split-string (or answer "") "\n")))
        (dolist (line (seq-take a-lines 10))
          (insert indent)
          (insert (propertize (format "│ %s\n" (truncate-string-to-width line 60 nil nil "..."))
                              'face 'discussion-board-answer-box)))
        (when (> (length a-lines) 10)
          (insert indent)
          (insert (propertize (format "│ ... (%d more lines)\n" (- (length a-lines) 10))
                              'face 'font-lock-comment-face)))))
    (insert indent)
    (insert (propertize "└──────────────────────────────────────────────────────────┘\n"
                        'face 'font-lock-comment-face))
    (insert "\n")))

(defun discussion-board--render ()
  "Render the discussion board buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Header
    (insert (propertize "🎓 Expert Discussion Board\n" 'face 'discussion-board-header))
    (insert "Asynchronous Q&A with external AI experts\n\n")
    ;; Toolbar
    (discussion-board--insert-button "[Refresh]"
                                     (lambda (_btn) (discussion-board-refresh)))
    (insert "  ")
    (discussion-board--insert-button "[Open ChatGPT]"
                                     (lambda (_btn) (browse-url mp-discussion-fallback-url)))
    (insert "  ")
    (discussion-board--insert-button "[Edit File]"
                                     (lambda (_btn) (discussion-board--edit-file)))
    (insert "\n\n")
    ;; Entries
    (setq discussion-board--entries (discussion-board--parse-entries))
    (if (null discussion-board--entries)
        (progn
          (insert (propertize "No questions yet.\n\n" 'face 'font-lock-comment-face))
          (insert "When an AI agent calls `ask_expert`, questions appear here.\n")
          (insert "• Click ▶ to expand/collapse entries\n")
          (insert "• Click [Copy] to copy question to clipboard\n")
          (insert "• Paste answer from external AI expert\n"))
      ;; Render entries (newest first)
      (dolist (entry (reverse discussion-board--entries))
        (discussion-board--render-entry-header entry)
        (when (gethash (alist-get 'id entry) discussion-board--expanded)
          (discussion-board--render-entry-body entry))))
    (goto-char (point-min))))

;;; ============================================================
;;; Major Mode
;;; ============================================================

(defvar discussion-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'discussion-board-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "o") (lambda () (interactive) (browse-url mp-discussion-fallback-url)))
    (define-key map (kbd "e") #'discussion-board--edit-file)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    map))

(define-derived-mode discussion-board-mode special-mode "Discussion"
  "Major mode for Expert Discussion Board."
  (setq buffer-read-only t
        truncate-lines nil
        word-wrap t)
  (display-line-numbers-mode -1))

;;; ============================================================
;;; Public API
;;; ============================================================

;;;###autoload
(defun discussion-board-refresh ()
  "Refresh the discussion board."
  (interactive)
  (when-let ((buf (get-buffer discussion-board--buffer-name)))
    (with-current-buffer buf
      (discussion-board--render))))

;;;###autoload
(defun discussion-board-open ()
  "Open discussion board buffer."
  (interactive)
  (let ((buf (get-buffer-create discussion-board--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'discussion-board-mode)
        (discussion-board-mode))
      (discussion-board--render))
    buf))

;;; ============================================================
;;; Panel Integration
;;; ============================================================

(defun mp--setup-discussion (session)
  "Setup Discussion panel."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    (switch-to-buffer (discussion-board-open))))

(mp-define-feat-tab discussion
  :name "Discussion"
  :key "e"
  :icon ""
  :setup #'mp--setup-discussion)

(provide 'panel-discussion)

;;; discussion.el ends here
