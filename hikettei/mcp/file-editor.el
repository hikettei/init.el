;;; file-editor.el --- GitHub PR Review style file editor UI -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Keywords: tools, mcp, review, diff

;;; Commentary:
;;
;; GitHub PR Review style interface for reviewing file edits.
;; Features:
;; - Full file display with diff highlighting
;; - Line-by-line commenting
;; - Approve / Request Changes workflow
;;

;;; Code:

(require 'cl-lib)
(require 'json)

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup file-editor nil
  "GitHub PR Review style file editor."
  :group 'tools
  :prefix "file-editor-")

(defcustom file-editor-result-file "/tmp/emacs-file-editor-result.json"
  "Path to write review result JSON."
  :type 'string
  :group 'file-editor)

;;; ============================================================
;;; Faces
;;; ============================================================

(defface file-editor-added-face
  '((t :background "#1e3a1e" :extend t))
  "Face for added lines."
  :group 'file-editor)

(defface file-editor-removed-face
  '((t :background "#3a1e1e" :extend t))
  "Face for removed lines."
  :group 'file-editor)

(defface file-editor-line-number-face
  '((t :foreground "#6272a4" :weight bold))
  "Face for line numbers."
  :group 'file-editor)

(defface file-editor-diff-line-number-face
  '((t :foreground "#ffb86c" :weight bold))
  "Face for line numbers in diff region."
  :group 'file-editor)

(defface file-editor-comment-indicator-face
  '((t :foreground "#ff79c6" :weight bold))
  "Face for comment indicator."
  :group 'file-editor)

(defface file-editor-header-face
  '((t :foreground "#bd93f9" :weight bold :height 1.1))
  "Face for header."
  :group 'file-editor)

(defface file-editor-stat-added-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for added line count."
  :group 'file-editor)

(defface file-editor-stat-removed-face
  '((t :foreground "#ff5555" :weight bold))
  "Face for removed line count."
  :group 'file-editor)

(defface file-editor-button-face
  '((t :foreground "#50fa7b" :box (:line-width 2 :style released-button) :weight bold))
  "Face for approve button."
  :group 'file-editor)

(defface file-editor-button-reject-face
  '((t :foreground "#ff5555" :box (:line-width 2 :style released-button) :weight bold))
  "Face for reject button."
  :group 'file-editor)

(defface file-editor-comment-face
  '((t :background "#44475a" :foreground "#f8f8f2" :extend t))
  "Face for inline comments."
  :group 'file-editor)

;;; ============================================================
;;; Data Structures
;;; ============================================================

(cl-defstruct file-editor-session
  "Structure representing a review session."
  id
  file-path
  full-content          ; Full file content (string)
  start-line            ; Start line of edit (1-indexed)
  end-line              ; End line of edit (1-indexed)
  original-content      ; Original content of edited region
  new-content           ; Proposed new content
  ai-comment
  line-comments         ; Alist of (line-number . comment-text)
  summary-comment
  decision
  callback
  buffer
  first-diff-pos)       ; Position of first diff line

(defvar file-editor--current-session nil
  "Current review session.")

(defvar file-editor--sessions (make-hash-table :test 'equal)
  "Hash table of active sessions by ID.")

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun file-editor--generate-uuid ()
  "Generate a simple UUID."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (logand (random 65536) 4095) 16384)
          (logior (logand (random 65536) 16383) 32768)
          (random 65536) (random 65536) (random 65536)))

(defun file-editor--split-lines (str)
  "Split STR into lines, preserving empty lines."
  (if (or (null str) (string-empty-p str))
      '("")
    (split-string str "\n" nil)))

(defun file-editor--get-comment-for-line (session line-num)
  "Get comment for LINE-NUM in SESSION."
  (cdr (assoc line-num (file-editor-session-line-comments session))))

(defun file-editor--set-comment-for-line (session line-num comment)
  "Set COMMENT for LINE-NUM in SESSION."
  (let ((comments (file-editor-session-line-comments session)))
    (if (assoc line-num comments)
        (setcdr (assoc line-num comments) comment)
      (setf (file-editor-session-line-comments session)
            (cons (cons line-num comment) comments)))))

(defun file-editor--remove-comment-for-line (session line-num)
  "Remove comment for LINE-NUM in SESSION."
  (setf (file-editor-session-line-comments session)
        (assq-delete-all line-num (file-editor-session-line-comments session))))

(defun file-editor--read-file-content (file-path)
  "Read content of FILE-PATH, return nil if not exists."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string))))

(defun file-editor--get-mode-for-file (file-path)
  "Get major mode for FILE-PATH based on extension."
  (let ((mode (assoc-default file-path auto-mode-alist 'string-match)))
    (if (and mode (symbolp mode) (fboundp mode))
        mode
      'fundamental-mode)))

(defun file-editor--fontify-string (str file-path)
  "Return STR with syntax highlighting based on FILE-PATH extension.
Returns original STR if fontification fails."
  (if (string-empty-p str)
      str
    (condition-case nil
        (let ((mode (file-editor--get-mode-for-file file-path)))
          (with-temp-buffer
            (insert str)
            (delay-mode-hooks (funcall mode))
            (font-lock-ensure)
            (buffer-string)))
      (error str))))

;;; ============================================================
;;; Buffer Rendering
;;; ============================================================

(defun file-editor--render-header (session)
  "Render the header section for SESSION."
  (let* ((file-path (file-editor-session-file-path session))
         (start-line (file-editor-session-start-line session))
         (end-line (file-editor-session-end-line session))
         (orig-lines (file-editor--split-lines (file-editor-session-original-content session)))
         (new-lines (file-editor--split-lines (file-editor-session-new-content session)))
         (removed-count (length orig-lines))
         (added-count (length new-lines)))
    (insert (propertize (format " File Review: %s\n" file-path)
                        'face 'file-editor-header-face))
    (insert (propertize (format " Lines %d-%d  |  " start-line end-line)
                        'face 'file-editor-line-number-face))
    (insert (propertize (format "-%d" removed-count)
                        'face 'file-editor-stat-removed-face))
    (insert " ")
    (insert (propertize (format "+%d" added-count)
                        'face 'file-editor-stat-added-face))
    (insert "\n\n")))

(defun file-editor--render-ai-comment (session)
  "Render AI's comment for SESSION if present."
  (when-let ((comment (file-editor-session-ai-comment session)))
    (insert (propertize " AI Comment:\n" 'face 'file-editor-header-face))
    (insert (propertize (format " %s\n\n" comment) 'face 'font-lock-comment-face))))

(defun file-editor--lcs-matrix (a b)
  "Build LCS length matrix for sequences A and B."
  (let* ((m (length a))
         (n (length b))
         (dp (make-vector (1+ m) nil)))
    (dotimes (i (1+ m))
      (aset dp i (make-vector (1+ n) 0)))
    (dotimes (i m)
      (dotimes (j n)
        (if (string= (nth i a) (nth j b))
            (aset (aref dp (1+ i)) (1+ j)
                  (1+ (aref (aref dp i) j)))
          (aset (aref dp (1+ i)) (1+ j)
                (max (aref (aref dp (1+ i)) j)
                     (aref (aref dp i) (1+ j)))))))
    dp))

(defun file-editor--lcs-backtrack (dp a b)
  "Backtrack through DP matrix to find LCS pairs of matching indices."
  (let ((pairs '())
        (i (length a))
        (j (length b)))
    (while (and (> i 0) (> j 0))
      (cond
       ((string= (nth (1- i) a) (nth (1- j) b))
        (push (cons (1- i) (1- j)) pairs)
        (setq i (1- i) j (1- j)))
       ((> (aref (aref dp (1- i)) j)
           (aref (aref dp i) (1- j)))
        (setq i (1- i)))
       (t
        (setq j (1- j)))))
    pairs))

(defun file-editor--compute-line-diff (orig-lines new-lines)
  "Compute line-by-line diff using LCS algorithm.
Returns list of (type content orig-idx new-idx)."
  (let* ((dp (file-editor--lcs-matrix orig-lines new-lines))
         (lcs-pairs (file-editor--lcs-backtrack dp orig-lines new-lines))
         (result '())
         (oi 0)
         (ni 0))
    (dolist (pair lcs-pairs)
      (let ((lcs-oi (car pair))
            (lcs-ni (cdr pair)))
        (while (< oi lcs-oi)
          (push (list 'remove (nth oi orig-lines) oi nil) result)
          (setq oi (1+ oi)))
        (while (< ni lcs-ni)
          (push (list 'add (nth ni new-lines) nil ni) result)
          (setq ni (1+ ni)))
        (push (list 'same (nth oi orig-lines) oi ni) result)
        (setq oi (1+ oi) ni (1+ ni))))
    (while (< oi (length orig-lines))
      (push (list 'remove (nth oi orig-lines) oi nil) result)
      (setq oi (1+ oi)))
    (while (< ni (length new-lines))
      (push (list 'add (nth ni new-lines) nil ni) result)
      (setq ni (1+ ni)))
    (nreverse result)))

(defun file-editor--render-full-file (session)
  "Render full file with unified diff highlighting for SESSION."
  (let* ((file-path (file-editor-session-file-path session))
         (full-content (file-editor-session-full-content session))
         (start-line (file-editor-session-start-line session))
         (end-line (file-editor-session-end-line session))
         (orig-lines (file-editor--split-lines (file-editor-session-original-content session)))
         (new-lines (file-editor--split-lines (file-editor-session-new-content session)))
         (all-lines (if full-content (file-editor--split-lines full-content) '()))
         ;; Pre-fontify all lines for syntax highlighting
         (fontified-lines (mapcar (lambda (line)
                                    (file-editor--fontify-string line file-path))
                                  all-lines))
         (fontified-new (mapcar (lambda (line)
                                  (file-editor--fontify-string line file-path))
                                new-lines))
         (diff-result (file-editor--compute-line-diff orig-lines new-lines))
         (line-num 1)
         (first-diff-pos nil))

    (insert (propertize (make-string 60 ?─) 'face 'file-editor-line-number-face))
    (insert "\n")

    ;; Lines before diff region
    (while (< line-num start-line)
      (when (< (1- line-num) (length fontified-lines))
        (let ((line (nth (1- line-num) fontified-lines)))
          (insert (propertize (format "%4d │ " line-num) 'face 'file-editor-line-number-face))
          (insert (format "%s\n" line))))
      (setq line-num (1+ line-num)))

    ;; Diff region with unified diff style
    (setq first-diff-pos (point))
    (let ((display-line-num start-line))
      (dolist (diff-entry diff-result)
        (let ((type (nth 0 diff-entry))
              (content (nth 1 diff-entry))
              (orig-idx (nth 2 diff-entry))
              (diff-new-idx (nth 3 diff-entry)))
          (pcase type
            ('same
             ;; Unchanged line in diff region - use fontified version
             (let ((fontified-content (file-editor--fontify-string content file-path)))
               (insert (propertize (format "%4d │ " display-line-num) 'face 'file-editor-line-number-face))
               (insert (format "%s\n" fontified-content)))
             (setq display-line-num (1+ display-line-num)))

            ('remove
             ;; Removed line - fontify and apply background
             (let* ((actual-line-num (+ start-line (or orig-idx 0)))
                    (has-comment (file-editor--get-comment-for-line session actual-line-num))
                    (comment-indicator (if has-comment
                                           (propertize " ●" 'face 'file-editor-comment-indicator-face)
                                         ""))
                    (fontified-content (file-editor--fontify-string content file-path))
                    (line-start (point)))
               (insert (propertize (format "%4d " display-line-num) 'face 'file-editor-diff-line-number-face))
               (insert (propertize "- │ " 'face 'file-editor-stat-removed-face))
               ;; Insert fontified content and add background
               (let ((content-start (point)))
                 (insert fontified-content)
                 (add-face-text-property content-start (point) 'file-editor-removed-face t))
               (insert comment-indicator)
               ;; Set text property on entire line for M-n/M-p navigation
               (put-text-property line-start (point) 'file-editor-line-num actual-line-num)
               (put-text-property line-start (point) 'file-editor-line-type 'remove)
               (insert "\n")
               (when has-comment
                 (insert (propertize (format "      │ 💬 %s\n" has-comment)
                                     'face 'file-editor-comment-face)))
               (setq display-line-num (1+ display-line-num))))

            ('add
             ;; Added line - fontify and apply background
             (let* ((effective-num (+ 10000 (1+ (or diff-new-idx 0))))
                    (has-comment (file-editor--get-comment-for-line session effective-num))
                    (comment-indicator (if has-comment
                                           (propertize " ●" 'face 'file-editor-comment-indicator-face)
                                         ""))
                    (fontified-content (when diff-new-idx (nth diff-new-idx fontified-new)))
                    (line-start (point)))
               (insert (propertize "   + " 'face 'file-editor-stat-added-face))
               (insert (propertize "+ │ " 'face 'file-editor-stat-added-face))
               ;; Insert fontified content and add background
               (let ((content-start (point)))
                 (insert (or fontified-content content))
                 (add-face-text-property content-start (point) 'file-editor-added-face t))
               (insert comment-indicator)
               ;; Set text property on entire line for M-n/M-p navigation
               (put-text-property line-start (point) 'file-editor-line-num effective-num)
               (put-text-property line-start (point) 'file-editor-line-type 'add)
               (insert "\n")
               (when has-comment
                 (insert (propertize (format "      │ 💬 %s\n" has-comment)
                                     'face 'file-editor-comment-face)))))))))

    ;; Lines after diff region
    (setq line-num (1+ end-line))
    (while (<= line-num (length fontified-lines))
      (let ((line (nth (1- line-num) fontified-lines)))
        (insert (propertize (format "%4d │ " line-num) 'face 'file-editor-line-number-face))
        (insert (format "%s\n" line)))
      (setq line-num (1+ line-num)))

    (insert (propertize (make-string 60 ?─) 'face 'file-editor-line-number-face))
    (insert "\n")

    ;; Store first diff position
    (setf (file-editor-session-first-diff-pos session) first-diff-pos)))

(defun file-editor--render-comments-summary (session)
  "Render comments summary section for SESSION."
  (let ((comments (file-editor-session-line-comments session)))
    (when comments
      (insert "\n")
      (insert (propertize (format " Comments (%d)\n" (length comments))
                          'face 'file-editor-header-face))
      (dolist (comment (reverse comments))
        (let ((line-num (car comment))
              (text (cdr comment)))
          (insert (propertize (format "  Line %s: "
                                      (if (> line-num 10000)
                                          (format "+%d" (- line-num 10000))
                                        (format "%d" line-num)))
                              'face 'file-editor-line-number-face))
          (insert (format "%s\n" text)))))))

(defun file-editor--render-summary-input (session)
  "Render summary comment input for SESSION."
  (insert "\n")
  (insert (propertize " Summary (press C to edit):\n" 'face 'file-editor-header-face))
  (let ((summary (file-editor-session-summary-comment session)))
    (if (and summary (not (string-empty-p summary)))
        (insert (format " %s\n" summary))
      (insert (propertize " (none)\n" 'face 'font-lock-comment-face)))))

(defun file-editor--render-buttons ()
  "Render action buttons."
  (insert "\n")
  (insert "  ")
  (insert-text-button " Approve (RET) "
                      'face 'file-editor-button-face
                      'action (lambda (_) (file-editor-approve))
                      'follow-link t)
  (insert "    ")
  (insert-text-button " Request Changes (r) "
                      'face 'file-editor-button-reject-face
                      'action (lambda (_) (file-editor-request-changes))
                      'follow-link t)
  (insert "\n\n"))

(defun file-editor--render-help ()
  "Render help text."
  (insert (propertize " Keys: " 'face 'file-editor-header-face))
  (insert "c=comment  C=summary  M-n/M-p=next/prev diff  RET=approve  r=reject  q=quit\n"))

(defun file-editor--render-buffer (session &optional preserve-pos)
  "Render the review buffer for SESSION.
If PRESERVE-POS is non-nil, try to keep cursor position."
  (let ((buffer (file-editor-session-buffer session))
        (saved-pos (when preserve-pos (point)))
        (saved-line-num (when preserve-pos (file-editor--current-line-num))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (file-editor--render-header session)
        (file-editor--render-ai-comment session)
        (file-editor--render-full-file session)
        (file-editor--render-comments-summary session)
        (file-editor--render-summary-input session)
        (file-editor--render-buttons)
        (file-editor--render-help)
        ;; Restore position or go to first diff
        (if (and preserve-pos saved-line-num)
            (file-editor--goto-line-num saved-line-num)
          (goto-char (or (file-editor-session-first-diff-pos session) (point-min))))))))

(defun file-editor--goto-line-num (target-line-num)
  "Go to line with TARGET-LINE-NUM property."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (equal (get-text-property (point) 'file-editor-line-num) target-line-num)
          (setq found t)
        (forward-line 1)))
    found))

;;; ============================================================
;;; Major Mode
;;; ============================================================

(defvar file-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'file-editor-add-comment)
    (define-key map (kbd "C") #'file-editor-edit-summary)
    (define-key map (kbd "e") #'file-editor-edit-comment)
    (define-key map (kbd "d") #'file-editor-delete-comment)
    (define-key map (kbd "M-n") #'file-editor-next-diff)
    (define-key map (kbd "M-p") #'file-editor-prev-diff)
    (define-key map (kbd "n") #'file-editor-next-diff)
    (define-key map (kbd "p") #'file-editor-prev-diff)
    (define-key map (kbd "RET") #'file-editor-approve)
    (define-key map (kbd "r") #'file-editor-request-changes)
    (define-key map (kbd "q") #'file-editor-quit)
    (define-key map (kbd "?") #'file-editor-help)
    (define-key map (kbd "g") #'file-editor-refresh)
    map)
  "Keymap for `file-editor-mode'.")

(define-derived-mode file-editor-mode special-mode "FileReview"
  "Major mode for reviewing file edits.

\\{file-editor-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local line-spacing 0.1))

;;; ============================================================
;;; Interactive Commands
;;; ============================================================

(defun file-editor--current-line-num ()
  "Get the line number at point from text property."
  (get-text-property (point) 'file-editor-line-num))

(defun file-editor--on-diff-line-p ()
  "Return t if point is on a diff line."
  (not (null (file-editor--current-line-num))))

(defun file-editor-add-comment ()
  "Add a comment to the current line."
  (interactive)
  (if-let ((line-num (file-editor--current-line-num)))
      (let* ((existing (file-editor--get-comment-for-line file-editor--current-session line-num))
             (display-num (if (> line-num 10000)
                              (format "+%d" (- line-num 10000))
                            (format "%d" line-num)))
             (comment (read-string (format "Comment for line %s: " display-num) existing)))
        (unless (string-empty-p comment)
          (file-editor--set-comment-for-line file-editor--current-session line-num comment)
          (file-editor--render-buffer file-editor--current-session t)))
    (message "Not on a diff line. Move to a +/- line first.")))

(defun file-editor-edit-comment ()
  "Edit the comment on the current line."
  (interactive)
  (file-editor-add-comment))

(defun file-editor-delete-comment ()
  "Delete the comment on the current line."
  (interactive)
  (when-let ((line-num (file-editor--current-line-num)))
    (when (file-editor--get-comment-for-line file-editor--current-session line-num)
      (when (y-or-n-p "Delete this comment? ")
        (file-editor--remove-comment-for-line file-editor--current-session line-num)
        (file-editor--render-buffer file-editor--current-session t)))))

(defun file-editor-edit-summary ()
  "Edit the summary comment."
  (interactive)
  (let* ((existing (file-editor-session-summary-comment file-editor--current-session))
         (comment (read-string "Summary comment: " existing)))
    (setf (file-editor-session-summary-comment file-editor--current-session) comment)
    (file-editor--render-buffer file-editor--current-session t)))

(defun file-editor-next-diff ()
  "Move to the next diff line."
  (interactive)
  (let ((found nil)
        (start-pos (point)))
    (forward-line 1)
    (while (and (not found) (not (eobp)))
      (when (file-editor--on-diff-line-p)
        (setq found t))
      (unless found
        (forward-line 1)))
    (unless found
      (goto-char start-pos)
      (message "No more diff lines"))))

(defun file-editor-prev-diff ()
  "Move to the previous diff line."
  (interactive)
  (let ((found nil)
        (start-pos (point)))
    (forward-line -1)
    (while (and (not found) (not (bobp)))
      (when (file-editor--on-diff-line-p)
        (setq found t))
      (unless found
        (forward-line -1)))
    (unless found
      (goto-char start-pos)
      (message "No more diff lines"))))

(defun file-editor-refresh ()
  "Refresh the review buffer."
  (interactive)
  (file-editor--render-buffer file-editor--current-session t))

(defun file-editor-help ()
  "Show help for file-editor-mode."
  (interactive)
  (message "c=comment  C=summary  M-n/M-p=nav  RET=approve  r=reject  q=quit"))

;;; ============================================================
;;; Decision Actions
;;; ============================================================

(defun file-editor--build-result (session decision)
  "Build result alist for SESSION with DECISION."
  `((session_id . ,(file-editor-session-id session))
    (file . ,(file-editor-session-file-path session))
    (start_line . ,(file-editor-session-start-line session))
    (end_line . ,(file-editor-session-end-line session))
    (decision . ,(symbol-name decision))
    (summary . ,(or (file-editor-session-summary-comment session) ""))
    (line_comments . ,(mapcar (lambda (c)
                                `((line . ,(car c))
                                  (comment . ,(cdr c))))
                              (file-editor-session-line-comments session)))
    (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))

(defun file-editor--finalize (session decision)
  "Finalize SESSION with DECISION, write result and cleanup."
  (setf (file-editor-session-decision session) decision)

  (let ((result (file-editor--build-result session decision)))
    ;; Write result to file
    (with-temp-file file-editor-result-file
      (insert (json-encode result)))

    ;; Call callback if provided
    (when-let ((callback (file-editor-session-callback session)))
      (funcall callback result))

    ;; Cleanup
    (remhash (file-editor-session-id session) file-editor--sessions)
    (let ((buf (file-editor-session-buffer session)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))

    (message "Review %s: %s"
             (if (eq decision 'approve) "approved" "requested changes")
             (file-editor-session-file-path session))))

(defun file-editor-approve ()
  "Approve the current edit."
  (interactive)
  (when (y-or-n-p "Approve this edit? ")
    (file-editor--finalize file-editor--current-session 'approve)))

(defun file-editor-request-changes ()
  "Request changes for the current edit."
  (interactive)
  (when (y-or-n-p "Request changes? ")
    (file-editor--finalize file-editor--current-session 'request-changes)))

(defun file-editor-quit ()
  "Quit the review (treated as request-changes)."
  (interactive)
  (when (y-or-n-p "Quit review? (Treated as Request Changes) ")
    (file-editor--finalize file-editor--current-session 'request-changes)))

;;; ============================================================
;;; Public API
;;; ============================================================

;;;###autoload
(cl-defun file-editor-open (&key file start-line end-line original new
                                 (ai-comment nil) (callback nil))
  "Open file editor review UI.

Arguments:
  FILE       - Path to the file being edited
  START-LINE - Start line number (1-indexed)
  END-LINE   - End line number (1-indexed)
  ORIGINAL   - Original content string
  NEW        - Proposed new content string
  AI-COMMENT - Optional comment from AI
  CALLBACK   - Optional function to call with result

Returns the session ID."
  ;; Validate required arguments
  (unless file (error "file-editor-open: file is required"))
  (unless (and start-line (integerp start-line))
    (error "file-editor-open: start-line must be an integer, got %S" start-line))
  (unless (and end-line (integerp end-line))
    (error "file-editor-open: end-line must be an integer, got %S" end-line))
  (let* ((id (file-editor--generate-uuid))
         (buf-name (format "*file-review: %s*" (file-name-nondirectory file)))
         (buffer (get-buffer-create buf-name))
         (full-content (file-editor--read-file-content file))
         (session (make-file-editor-session
                   :id id
                   :file-path file
                   :full-content full-content
                   :start-line start-line
                   :end-line end-line
                   :original-content original
                   :new-content new
                   :ai-comment ai-comment
                   :line-comments nil
                   :summary-comment nil
                   :decision nil
                   :callback callback
                   :buffer buffer
                   :first-diff-pos nil)))

    ;; Store session
    (puthash id session file-editor--sessions)
    (setq file-editor--current-session session)

    ;; Setup buffer
    (with-current-buffer buffer
      (file-editor-mode)
      (setq-local file-editor--current-session session))

    ;; Render (will go to first diff)
    (file-editor--render-buffer session nil)

    ;; Display
    (pop-to-buffer buffer
                   '((display-buffer-reuse-window display-buffer-below-selected)
                     (window-height . 0.7)))

    id))

;;;###autoload
(defun file-editor-open-from-json (json-string)
  "Open file editor from JSON-STRING request."
  (let* ((data (json-read-from-string json-string))
         (file-path (alist-get 'file_path data))
         (start-line (alist-get 'start_line data))
         (end-line (alist-get 'end_line data))
         (original (alist-get 'original_content data))
         (new-content (alist-get 'new_content data))
         (comment (alist-get 'comment data)))
    (file-editor-open
     :file file-path
     :start-line start-line
     :end-line end-line
     :original original
     :new new-content
     :ai-comment comment)))

;;; ============================================================
;;; Test Interface
;;; ============================================================

;;;###autoload
(defun file-editor-test ()
  "Test the file editor UI with sample data (multiple diff regions)."
  (interactive)
  (file-editor--create-test-file)
  (file-editor-open
   :file "/tmp/test-sample.py"
   :start-line 3
   :end-line 14
   :original "import os

def calculate_total(items):
    total = 0
    for item in items:
        total += item.price
    return total

def format_price(price):
    return \"$\" + str(price)

def main():"
   :new "import os
from decimal import Decimal

def calculate_total(items):
    \"\"\"Calculate total with Decimal precision.\"\"\"
    total = Decimal('0')
    for item in items:
        total += Decimal(str(item.price))
    return total

def format_price(price):
    return f\"${price:.2f}\"

def main():"
   :ai-comment "1) Added Decimal import  2) Changed total calculation to use Decimal  3) Updated format_price to use f-string with 2 decimal places"
   :callback (lambda (result)
               (message "Review completed: %S" result))))

(defun file-editor--create-test-file ()
  "Create test file for demo."
  (with-temp-file "/tmp/test-sample.py"
    (insert "#!/usr/bin/env python3
\"\"\"Sample module for testing file-editor.\"\"\"
import os

def calculate_total(items):
    total = 0
    for item in items:
        total += item.price
    return total

def format_price(price):
    return \"$\" + str(price)

def main():
    items = get_items()
    total = calculate_total(items)
    print(format_price(total))

if __name__ == \"__main__\":
    main()
")))

(provide 'file-editor)

;;; file-editor.el ends here
