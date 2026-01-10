;;; file-editor.el --- Overlay-based file edit review UI -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Keywords: tools, mcp, review, diff

;;; Commentary:
;;
;; Overlay-based interface for reviewing AI file edits.
;; Features:
;; - Shows diff directly on actual file buffer (not separate buffer)
;; - Removed lines shown with red background
;; - Added lines shown as phantom text with green background
;; - Approve / Request Changes workflow
;; - Integrates with Activity Panel for Autopilot mode
;;

;;; Code:

(require 'cl-lib)
(require 'json)

(declare-function activity-panel-start-edit-review "activity-panel")
(declare-function activity-panel-end-edit-review "activity-panel")
(declare-function activity-panel-show-file "activity-panel")
(declare-function activity-panel-active-p "activity-panel")
(declare-function activity-panel-get-file-window "activity-panel")

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup file-editor nil
  "Overlay-based file edit review."
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

(defface file-editor-removed-marker-face
  '((t :foreground "#ff5555" :weight bold))
  "Face for removed line marker."
  :group 'file-editor)

(defface file-editor-added-marker-face
  '((t :foreground "#50fa7b" :weight bold))
  "Face for added line marker."
  :group 'file-editor)

(defface file-editor-header-face
  '((t :foreground "#bd93f9" :weight bold :height 1.1))
  "Face for review header."
  :group 'file-editor)

(defface file-editor-status-face
  '((t :foreground "#f1fa8c" :weight bold))
  "Face for review status indicator."
  :group 'file-editor)

;;; ============================================================
;;; Data Structures
;;; ============================================================

(cl-defstruct file-editor-session
  "Structure representing a review session."
  id
  file-path
  start-line            ; Start line of edit (1-indexed)
  end-line              ; End line of edit (1-indexed)
  original-content      ; Original content of edited region
  new-content           ; Proposed new content
  ai-comment
  callback
  ;; Overlay-based fields
  file-buffer           ; The actual file buffer
  removed-overlays      ; List of overlays for removed lines
  added-overlays        ; List of overlays for added lines (phantom)
  header-overlay        ; Overlay for review header
  comment-overlays      ; List of overlays for line comments
  line-comments         ; Alist of (line-number . comment-text)
  decision)             ; 'approve or 'request-changes

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

;;; ============================================================
;;; LCS Diff Algorithm
;;; ============================================================

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

;;; ============================================================
;;; Overlay Creation
;;; ============================================================

(defun file-editor--create-removed-overlay (buffer line-num)
  "Create overlay for removed line at LINE-NUM in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line-num))
      (let* ((start (line-beginning-position))
             (end (1+ (line-end-position))) ; Include newline
             (ov (make-overlay start end buffer)))
        (overlay-put ov 'face 'file-editor-removed-face)
        (overlay-put ov 'file-editor-type 'removed)
        (overlay-put ov 'priority 100)
        (overlay-put ov 'before-string
                     (propertize "- " 'face 'file-editor-removed-marker-face))
        ov))))

(defun file-editor--create-added-overlay (buffer after-line-num content)
  "Create phantom overlay for added line after AFTER-LINE-NUM in BUFFER.
CONTENT is the added line content."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- after-line-num))
      (let* ((end (line-end-position))
             (ov (make-overlay end end buffer))
             (display-str (propertize (concat "\n" (propertize "+ " 'face 'file-editor-added-marker-face) content)
                                      'face 'file-editor-added-face)))
        (overlay-put ov 'after-string display-str)
        (overlay-put ov 'file-editor-type 'added)
        (overlay-put ov 'file-editor-content content)
        (overlay-put ov 'priority 100)
        ov))))

(defun file-editor--create-header-overlay (buffer start-line ai-comment)
  "Create header overlay at START-LINE in BUFFER with AI-COMMENT."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let* ((pos (line-beginning-position))
             (ov (make-overlay pos pos buffer))
             (header-text (concat
                           (propertize " >>> REVIEW PENDING <<< "
                                       'face '(:background "#ff5555" :foreground "#f8f8f2" :weight bold))
                           "\n"
                           (when ai-comment
                             (concat (propertize (format " AI: %s " ai-comment)
                                                'face '(:background "#bd93f9" :foreground "#f8f8f2" :weight bold))
                                     "\n"))
                           (propertize " C-c C-c: Approve | C-c C-k: Reject | M-n/M-p: Navigate "
                                       'face '(:background "#44475a" :foreground "#f8f8f2"))
                           "\n"
                           (propertize (make-string 60 ?─) 'face '(:foreground "#6272a4"))
                           "\n")))
        (overlay-put ov 'before-string header-text)
        (overlay-put ov 'file-editor-type 'header)
        (overlay-put ov 'priority 200)
        ov))))

(defun file-editor--create-diff-overlays (session)
  "Create all diff overlays for SESSION."
  (let* ((buffer (file-editor-session-file-buffer session))
         (start-line (file-editor-session-start-line session))
         (orig-lines (file-editor--split-lines
                      (file-editor-session-original-content session)))
         (new-lines (file-editor--split-lines
                     (file-editor-session-new-content session)))
         (diff-result (file-editor--compute-line-diff orig-lines new-lines))
         (line-num start-line)
         (removed-ovs '())
         (added-ovs '())
         (pending-adds '()))
    ;; Create header overlay
    (let ((header-ov (file-editor--create-header-overlay
                      buffer start-line
                      (file-editor-session-ai-comment session))))
      (setf (file-editor-session-header-overlay session) header-ov))
    ;; Process diff entries
    (dolist (entry diff-result)
      (pcase (car entry)
        ('remove
         ;; First, flush any pending adds before this line
         (when pending-adds
           (dolist (add-content (nreverse pending-adds))
             (push (file-editor--create-added-overlay buffer (1- line-num) add-content)
                   added-ovs))
           (setq pending-adds nil))
         ;; Create removed overlay
         (push (file-editor--create-removed-overlay buffer line-num) removed-ovs)
         (setq line-num (1+ line-num)))
        ('add
         ;; Queue added lines to be shown after current position
         (push (nth 1 entry) pending-adds))
        ('same
         ;; Flush pending adds
         (when pending-adds
           (dolist (add-content (nreverse pending-adds))
             (push (file-editor--create-added-overlay buffer (1- line-num) add-content)
                   added-ovs))
           (setq pending-adds nil))
         (setq line-num (1+ line-num)))))
    ;; Flush any remaining adds at the end
    (when pending-adds
      (dolist (add-content (nreverse pending-adds))
        (push (file-editor--create-added-overlay buffer (1- line-num) add-content)
              added-ovs)))
    ;; Store overlays in session
    (setf (file-editor-session-removed-overlays session) (nreverse removed-ovs))
    (setf (file-editor-session-added-overlays session) (nreverse added-ovs))))

(defun file-editor--clear-overlays (session)
  "Remove all diff overlays for SESSION."
  (dolist (ov (file-editor-session-removed-overlays session))
    (when (overlayp ov) (delete-overlay ov)))
  (dolist (ov (file-editor-session-added-overlays session))
    (when (overlayp ov) (delete-overlay ov)))
  (dolist (ov (file-editor-session-comment-overlays session))
    (when (overlayp ov) (delete-overlay ov)))
  (when-let ((header (file-editor-session-header-overlay session)))
    (when (overlayp header) (delete-overlay header)))
  (setf (file-editor-session-removed-overlays session) nil)
  (setf (file-editor-session-added-overlays session) nil)
  (setf (file-editor-session-comment-overlays session) nil)
  (setf (file-editor-session-header-overlay session) nil))

;;; ============================================================
;;; Line Comments
;;; ============================================================

(defun file-editor--create-comment-overlay (buffer line-num comment-text)
  "Create comment overlay at LINE-NUM in BUFFER with COMMENT-TEXT."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line-num))
      (let* ((end (line-end-position))
             (ov (make-overlay end end buffer))
             (display-str (propertize
                           (concat "\n"
                                   (propertize "  💬 " 'face '(:foreground "#ff79c6"))
                                   (propertize comment-text
                                              'face '(:background "#ffb86c" :foreground "#282a36" :slant italic)))
                           'face '(:extend t))))
        (overlay-put ov 'after-string display-str)
        (overlay-put ov 'file-editor-type 'comment)
        (overlay-put ov 'file-editor-line line-num)
        (overlay-put ov 'priority 150)
        ov))))

(defun file-editor-add-comment ()
  "Add a comment on the current line."
  (interactive)
  (unless file-editor--current-session
    (user-error "No active review session"))
  (let* ((session file-editor--current-session)
         (line-num (line-number-at-pos))
         (comment-text (read-string (format "Comment for line %d: " line-num))))
    (when (and comment-text (not (string-empty-p comment-text)))
      ;; Add to line-comments alist
      (push (cons line-num comment-text)
            (file-editor-session-line-comments session))
      ;; Create overlay
      (let ((ov (file-editor--create-comment-overlay
                 (file-editor-session-file-buffer session)
                 line-num comment-text)))
        (push ov (file-editor-session-comment-overlays session)))
      (message "Comment added on line %d" line-num))))

;;; ============================================================
;;; Minor Mode for Review
;;; ============================================================

(defvar file-editor-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'file-editor-approve)
    (define-key map (kbd "C-c C-k") #'file-editor-request-changes)
    (define-key map (kbd "M-n") #'file-editor-next-diff)
    (define-key map (kbd "M-p") #'file-editor-prev-diff)
    (define-key map (kbd "c") #'file-editor-add-comment)
    map)
  "Keymap for `file-editor-review-mode'.")

(define-minor-mode file-editor-review-mode
  "Minor mode for reviewing file edits with overlays.

\\{file-editor-review-mode-map}"
  :lighter " Review"
  :keymap file-editor-review-mode-map
  (if file-editor-review-mode
      (message "Review mode: C-c C-c=approve, C-c C-k=reject, c=comment")
    (message "Review mode disabled")))

;;; ============================================================
;;; Navigation
;;; ============================================================

(defun file-editor-next-diff ()
  "Move to the next diff overlay."
  (interactive)
  (when file-editor--current-session
    (let* ((session file-editor--current-session)
           (all-ovs (append (file-editor-session-removed-overlays session)
                           (file-editor-session-added-overlays session)))
           (pos (point))
           (next-pos nil))
      (dolist (ov all-ovs)
        (when (and (overlayp ov)
                   (> (overlay-start ov) pos)
                   (or (null next-pos) (< (overlay-start ov) next-pos)))
          (setq next-pos (overlay-start ov))))
      (if next-pos
          (goto-char next-pos)
        (message "No more diff hunks")))))

(defun file-editor-prev-diff ()
  "Move to the previous diff overlay."
  (interactive)
  (when file-editor--current-session
    (let* ((session file-editor--current-session)
           (all-ovs (append (file-editor-session-removed-overlays session)
                           (file-editor-session-added-overlays session)))
           (pos (point))
           (prev-pos nil))
      (dolist (ov all-ovs)
        (when (and (overlayp ov)
                   (< (overlay-start ov) pos)
                   (or (null prev-pos) (> (overlay-start ov) prev-pos)))
          (setq prev-pos (overlay-start ov))))
      (if prev-pos
          (goto-char prev-pos)
        (message "No more diff hunks")))))

;;; ============================================================
;;; Decision Actions
;;; ============================================================

(defun file-editor--build-result (session decision feedback line-comments)
  "Build result alist for SESSION with DECISION, FEEDBACK, and LINE-COMMENTS."
  `((session_id . ,(file-editor-session-id session))
    (file . ,(file-editor-session-file-path session))
    (start_line . ,(file-editor-session-start-line session))
    (end_line . ,(file-editor-session-end-line session))
    (decision . ,(symbol-name decision))
    (feedback . ,(or feedback ""))
    (line_comments . ,(mapcar (lambda (c)
                                `((line . ,(car c))
                                  (comment . ,(cdr c))))
                              line-comments))
    (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))

(defun file-editor--finalize (session decision &optional feedback)
  "Finalize SESSION with DECISION and optional FEEDBACK."
  (setf (file-editor-session-decision session) decision)
  ;; Clear overlays
  (file-editor--clear-overlays session)
  ;; Disable review mode
  (when-let ((buf (file-editor-session-file-buffer session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (file-editor-review-mode -1))))
  ;; Clear pending review flag
  (when (boundp 'mp--review-pending)
    (setq mp--review-pending nil)
    (when (fboundp 'mp--update-feat-tab-bar)
      (mp--update-feat-tab-bar)))
  ;; Build result and callback
  (let* ((line-comments (file-editor-session-line-comments session))
         (result (file-editor--build-result session decision feedback line-comments)))
    (with-temp-file file-editor-result-file
      (insert (json-encode result)))
    (when-let ((callback (file-editor-session-callback session)))
      (funcall callback result))
    (remhash (file-editor-session-id session) file-editor--sessions)
    (setq file-editor--current-session nil)
    (message "Review %s: %s"
             (if (eq decision 'approve) "approved" "rejected")
             (file-editor-session-file-path session))))

(defun file-editor-approve ()
  "Approve the current edit."
  (interactive)
  (unless file-editor--current-session
    (user-error "No active review session"))
  (let ((feedback (read-string "Feedback (optional): ")))
    (file-editor--finalize file-editor--current-session 'approve feedback)))

(defun file-editor-request-changes ()
  "Request changes for the current edit."
  (interactive)
  (unless file-editor--current-session
    (user-error "No active review session"))
  (let ((feedback (read-string "Reason for rejection: ")))
    (file-editor--finalize file-editor--current-session 'request-changes feedback)))

;;; ============================================================
;;; Public API
;;; ============================================================

;;;###autoload
(cl-defun file-editor-open (&key file start-line end-line original new
                                 (ai-comment nil) (callback nil))
  "Open overlay-based file review UI.

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
    (error "file-editor-open: start-line must be an integer"))
  (unless (and end-line (integerp end-line))
    (error "file-editor-open: end-line must be an integer"))
  (let* ((id (file-editor--generate-uuid))
         (existing-buf (get-file-buffer file))
         (buffer (if existing-buf
                     (progn
                       ;; Silently revert if file changed on disk
                       (with-current-buffer existing-buf
                         (when (and (buffer-file-name)
                                    (file-exists-p (buffer-file-name))
                                    (not (verify-visited-file-modtime existing-buf)))
                           (revert-buffer t t t)))
                       existing-buf)
                   (find-file-noselect file t)))
         (session (make-file-editor-session
                   :id id
                   :file-path file
                   :start-line start-line
                   :end-line end-line
                   :original-content original
                   :new-content new
                   :ai-comment ai-comment
                   :callback callback
                   :file-buffer buffer
                   :removed-overlays nil
                   :added-overlays nil
                   :header-overlay nil
                   :comment-overlays nil
                   :line-comments nil
                   :decision nil)))
    ;; Store session
    (puthash id session file-editor--sessions)
    (setq file-editor--current-session session)
    ;; Create diff overlays on the actual file buffer
    (file-editor--create-diff-overlays session)
    ;; Enable review minor mode and auto-revert
    (with-current-buffer buffer
      (auto-revert-mode 1)
      (file-editor-review-mode 1))
    ;; Display in Autopilot's file window if available
    (let ((autopilot-win (and (boundp 'mp--autopilot-file-window)
                              mp--autopilot-file-window
                              (window-live-p mp--autopilot-file-window)
                              mp--autopilot-file-window)))
      (if autopilot-win
          (progn
            (set-window-buffer autopilot-win buffer)
            ;; Position without stealing focus
            (with-selected-window autopilot-win
              (goto-char (point-min))
              (forward-line (1- start-line))
              (recenter 3))
            ;; Clear pending flag since we're displaying
            (when (boundp 'mp--review-pending)
              (setq mp--review-pending nil)))
        ;; Not on Autopilot tab - set pending indicator
        (when (boundp 'mp--review-pending)
          (setq mp--review-pending session)
          ;; Update tab bar to show indicator
          (when (fboundp 'mp--update-feat-tab-bar)
            (mp--update-feat-tab-bar)))))
    id))

;;;###autoload
(defun file-editor-test ()
  "Test the overlay-based file editor UI."
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
   :ai-comment "Added Decimal import, changed total calculation to use Decimal, updated format_price to use f-string"
   :callback (lambda (result)
               (message "Test review completed: %S" result))))

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
