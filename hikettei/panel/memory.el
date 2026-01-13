;;; panel-memory.el --- Memory Panel -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 1.0.0
;; Keywords: ai, memory, panel

;;; Commentary:
;;
;; Memory Panel for viewing and managing AI agent memories.
;; Displays memories in a tree view with preview pane.
;; Keybinding: C-x j y
;;

;;; Code:

(require 'multi-panel)

;; Delay loading memory module until actually needed
(declare-function mcp-memory--search "memory")
(declare-function mcp-memory--get-by-id "memory")
(declare-function mcp-memory--get-content "memory")
(declare-function mcp-memory--list "memory")
(declare-function mcp-memory--delete-entry "memory")
(declare-function mcp-memory--memory-dir "memory")

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup mp-memory nil
  "Memory panel settings."
  :group 'multi-panel
  :prefix "mp-memory-")

(defface mp-memory-type-note
  '((t :foreground "#f1fa8c"))
  "Face for note type memories."
  :group 'mp-memory)

(defface mp-memory-type-webpage
  '((t :foreground "#8be9fd"))
  "Face for webpage type memories."
  :group 'mp-memory)

(defface mp-memory-type-github
  '((t :foreground "#50fa7b"))
  "Face for github type memories."
  :group 'mp-memory)

(defface mp-memory-type-arxiv
  '((t :foreground "#ffb86c"))
  "Face for arxiv type memories."
  :group 'mp-memory)

(defface mp-memory-type-pdf
  '((t :foreground "#ff79c6"))
  "Face for pdf type memories."
  :group 'mp-memory)

;;; ============================================================
;;; State
;;; ============================================================

(defvar mp-memory--filter-type nil
  "Current type filter (nil = all).")

(defvar mp-memory--selected-id nil
  "Currently selected memory ID.")

(defvar mp-memory--tree-buffer nil
  "Buffer for memory tree view.")

(defvar mp-memory--preview-buffer nil
  "Buffer for memory preview.")

(defvar mp-memory--tree-window nil
  "Window for tree view.")

(defvar mp-memory--preview-window nil
  "Window for preview.")

;;; ============================================================
;;; Utilities
;;; ============================================================

(defun mp-memory--type-icon (type)
  "Get icon for memory TYPE."
  (pcase type
    ("note" "📝")
    ("webpage" "🌐")
    ("github" "📦")
    ("arxiv" "📄")
    ("pdf" "📕")
    ("screenshot" "📷")
    (_ "📎")))

(defun mp-memory--type-face (type)
  "Get face for memory TYPE."
  (pcase type
    ("note" 'mp-memory-type-note)
    ("webpage" 'mp-memory-type-webpage)
    ("github" 'mp-memory-type-github)
    ("arxiv" 'mp-memory-type-arxiv)
    ("pdf" 'mp-memory-type-pdf)
    ("screenshot" 'mp-memory-type-webpage)  ; Use cyan color
    (_ 'default)))


(defun mp-memory--format-date (iso-date)
  "Format ISO-DATE for display."
  (if (and iso-date (stringp iso-date))
      (if (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" iso-date)
          (format "%s/%s/%s"
                  (match-string 1 iso-date)
                  (match-string 2 iso-date)
                  (match-string 3 iso-date))
        iso-date)
    ""))

;;; ============================================================
;;; Tree View
;;; ============================================================

(defvar mp-memory-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'mp-memory-open)
    (define-key map (kbd "n") #'mp-memory-next)
    (define-key map (kbd "p") #'mp-memory-prev)
    (define-key map (kbd "j") #'mp-memory-next)
    (define-key map (kbd "k") #'mp-memory-prev)
    (define-key map (kbd "<down>") #'mp-memory-next)
    (define-key map (kbd "<up>") #'mp-memory-prev)
    (define-key map [mouse-1] #'mp-memory-mouse-select)
    (define-key map [double-mouse-1] #'mp-memory-mouse-select)
    (define-key map (kbd "f") #'mp-memory-cycle-filter)
    (define-key map (kbd "d") #'mp-memory-delete-at-point)
    (define-key map (kbd "g") #'mp-memory-refresh)
    (define-key map (kbd "q") #'mp-memory-quit)
    (define-key map (kbd "m") #'mp-memory-markdown-preview)
    (define-key map (kbd "TAB") #'mp-memory-next)
    (define-key map (kbd "<backtab>") #'mp-memory-prev)
    map)
  "Keymap for memory tree mode.")

(defun mp-memory-markdown-preview ()
  "Open markdown preview for the selected memory's content file."
  (interactive)
  (when-let ((id (get-text-property (point) 'memory-id)))
    (require 'memory)
    (let ((mem (mcp-memory--get-by-id id)))
      (when mem
        (let* ((type (cdr (assoc 'type mem)))
               (file (cdr (assoc 'file mem))))
          (cond
           ((and (string= type "note") file (file-exists-p file))
            (mp-memory--open-markdown-preview file))
           ((and file (file-exists-p file) (string-suffix-p ".md" file t))
            (mp-memory--open-markdown-preview file))
           (t
            (message "No markdown file available for this memory"))))))))

(defun mp-memory--open-markdown-preview (file)
  "Open FILE and trigger markdown preview in the preview window."
  (let ((target-win (cond
                     ((and mp-memory--preview-window
                           (window-live-p mp-memory--preview-window))
                      mp-memory--preview-window)
                     ((and (boundp 'mp--workarea-window)
                           mp--workarea-window
                           (window-live-p mp--workarea-window))
                      mp--workarea-window)
                     (t nil))))
    (if target-win
        (with-selected-window target-win
          (find-file file)
          (cond
           ((fboundp 'markdown-live-preview-mode)
            (markdown-live-preview-mode 1))
           ((fboundp 'markdown-preview)
            (markdown-preview))
           (t
            (markdown-mode)
            (message "Opened %s in markdown-mode" (file-name-nondirectory file)))))
      (find-file-other-window file))))


(defun mp-memory--post-command-hook ()
  "Hook to auto-preview on cursor movement."
  (when (eq major-mode 'mp-memory-tree-mode)
    (mp-memory--auto-preview)))

(define-derived-mode mp-memory-tree-mode special-mode "MemTree"
  "Mode for memory tree view.
\\{mp-memory-tree-mode-map}"
  :interactive nil
  (setq-local mode-line-format nil)
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (use-local-map mp-memory-tree-mode-map)
  (hl-line-mode 1)
  (face-remap-add-relative 'hl-line :background "#44475a" :extend t)
  (add-hook 'post-command-hook #'mp-memory--post-command-hook nil t))

(defun mp-memory--render-tree ()
  "Render the memory tree buffer."
  (require 'memory)
  (let ((buf (get-buffer-create "*Memory Tree*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        ;; Header
        (insert (propertize " 🧠 Memory\n" 'face '(:foreground "#bd93f9" :weight bold :height 1.2)))
        (insert (propertize (make-string 30 ?─) 'face '(:foreground "#6272a4")))
        (insert "\n")
        ;; Filter bar
        (insert " ")
        (dolist (type '("all" "note" "webpage" "github" "arxiv" "pdf" "screenshot"))
          (let ((active (string= type (or mp-memory--filter-type "all"))))
            (insert (propertize (format "[%s]" type)
                                'face (if active
                                          '(:foreground "#50fa7b" :weight bold)
                                        '(:foreground "#6272a4"))
                                'mouse-face 'highlight
                                'filter-type type)
                    " ")))
        (insert "\n\n")
        ;; Memory list
        (let ((memories (mcp-memory--list mp-memory--filter-type 100 0)))
          (if (null memories)
              (insert (propertize "  No memories yet.\n  Use AI to store content!\n\n"
                                  'face '(:foreground "#6272a4" :slant italic))
                      (propertize "  j/k or ↑/↓ - navigate\n"
                                  'face '(:foreground "#6272a4"))
                      (propertize "  RET - open file/URL\n"
                                  'face '(:foreground "#6272a4"))
                      (propertize "  m - markdown preview\n"
                                  'face '(:foreground "#6272a4"))
                      (propertize "  f - cycle filter\n"
                                  'face '(:foreground "#6272a4"))
                      (propertize "  d - delete  g - refresh\n"
                                  'face '(:foreground "#6272a4")))

            ;; Render items
            (dolist (mem memories)
              (let* ((id (cdr (assoc 'id mem)))
                     (type (cdr (assoc 'type mem)))
                     (title (cdr (assoc 'title mem)))
                     (icon (mp-memory--type-icon type))
                     (face (mp-memory--type-face type)))
                (insert (propertize (format " %s %s\n"
                                            icon
                                            (if (> (length title) 28)
                                                (concat (substring title 0 25) "...")
                                              title))
                                    'face face
                                    'memory-id id
                                    'mouse-face 'highlight))))
            ;; Footer with keybindings
            (insert "\n")
            (insert (propertize (make-string 30 ?─) 'face '(:foreground "#44475a")))
            (insert "\n")
            (insert (propertize " j/k" 'face '(:foreground "#6272a4")))
            (insert (propertize " nav " 'face '(:foreground "#44475a")))
            (insert (propertize "f" 'face '(:foreground "#6272a4")))
            (insert (propertize " filter " 'face '(:foreground "#44475a")))
            (insert (propertize "m" 'face '(:foreground "#ff79c6")))
            (insert (propertize " md " 'face '(:foreground "#44475a")))
            (insert (propertize "d" 'face '(:foreground "#6272a4")))
            (insert (propertize " del\n" 'face '(:foreground "#44475a")))))

        (mp-memory-tree-mode)
        (goto-char (min pos (point-max)))))
    buf))


(defvar mp-memory--first-item-line 5
  "Line number where first memory item starts.")

(defun mp-memory--goto-item-line (line)
  "Go to LINE and auto-preview."
  (goto-char (point-min))
  (forward-line (1- line))
  (mp-memory--auto-preview))

(defun mp-memory--auto-preview ()
  "Preview the memory at current point if any."
  (when-let ((id (get-text-property (point) 'memory-id)))
    (unless (string= id mp-memory--selected-id)
      (setq mp-memory--selected-id id)
      (mp-memory--show-preview id))))

(defun mp-memory-next ()
  "Move to next memory item and preview."
  (interactive)
  (let ((start (point)))
    ;; Move forward until we find a new memory-id or end of buffer
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (point) 'memory-id)))
      (forward-line 1))
    (if (eobp)
        (goto-char start)  ; Stay at current if no more items
      (mp-memory--auto-preview))))

(defun mp-memory-prev ()
  "Move to previous memory item and preview."
  (interactive)
  (let ((start (point)))
    ;; Move backward until we find a memory-id
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (point) 'memory-id)))
      (forward-line -1))
    (if (not (get-text-property (point) 'memory-id))
        (goto-char start)  ; Stay at current if no previous items
      (mp-memory--auto-preview))))

(defun mp-memory-mouse-select (event)
  "Handle mouse click EVENT on memory item - opens content buffer."
  (interactive "e")
  (let* ((pos (posn-point (event-end event)))
         (win (posn-window (event-end event))))
    (when (and pos win)
      ;; Select the window that was clicked
      (select-window win)
      (goto-char pos)
      ;; Check if clicked on filter
      (if-let ((filter-type (get-text-property pos 'filter-type)))
          (progn
            (setq mp-memory--filter-type (unless (string= filter-type "all") filter-type))
            (setq mp-memory--selected-id nil)
            (mp-memory--render-tree)
            (mp-memory--goto-first-item))
        ;; Otherwise, find memory-id on this line and open it
        (beginning-of-line)
        (let ((line-end (line-end-position))
              (found nil))
          ;; Search for memory-id property on this line
          (while (and (< (point) line-end) (not found))
            (if (get-text-property (point) 'memory-id)
                (setq found t)
              (forward-char 1)))
          (when found
            (mp-memory-open)))))))


(defun mp-memory-close-detail ()
  "Close the memory detail buffer and return to tree view."
  (interactive)
  (let ((buf (current-buffer)))
    ;; Kill the detail buffer
    (when (buffer-live-p buf)
      (kill-buffer buf))
    ;; Return focus to tree view if available, otherwise switch to memory panel
    (cond
     ((and mp-memory--tree-window (window-live-p mp-memory--tree-window))
      (select-window mp-memory--tree-window))
     ((and mp--workarea-window (window-live-p mp--workarea-window))
      ;; Re-setup memory panel
      (mp--setup-memory (and (boundp 'ai-session--current) ai-session--current)))
     (t
      ;; Fallback: switch to memory panel
      (mp-switch-to 'memory)))))

(defun mp-memory-open ()

  "Open the currently selected memory in a new buffer."
  (interactive)
  (when-let ((id (get-text-property (point) 'memory-id)))
    (require 'memory)
    ;; For screenshots, get from scan (not index)
    (let ((mem (or (mcp-memory--get-by-id id)
                   (cl-find-if (lambda (m) (string= (cdr (assoc 'id m)) id))
                               (mcp-memory--scan-screenshots)))))
      (when mem
        (let* ((title (cdr (assoc 'title mem)))
               (type (cdr (assoc 'type mem)))
               (file (cdr (assoc 'file mem))))
          ;; Handle screenshot type - open image directly
          (if (and (string= type "screenshot") file (file-exists-p file))
              (progn
                (if (and mp--workarea-window (window-live-p mp--workarea-window))
                    (progn
                      (select-window mp--workarea-window)
                      (find-file file))
                  (find-file file))
                (message "Opened: %s" title))
            ;; Regular memory - show in formatted buffer
            (let* ((buf-name (format "*Memory: %s*" (if (> (length title) 30)
                                                        (concat (substring title 0 27) "...")
                                                      title)))
                   (buf (get-buffer-create buf-name)))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  ;; Title
                  (insert (propertize (format "%s\n" title)
                                      'face '(:foreground "#f8f8f2" :weight bold :height 1.5)))
                  (insert (propertize (make-string 50 ?─) 'face '(:foreground "#6272a4")))
                  (insert "\n\n")
                  ;; Metadata
                  (insert (propertize (format "Type: %s  |  ID: %s\n" type id)
                                      'face '(:foreground "#6272a4")))
                  (when-let ((url (cdr (assoc 'url mem))))
                    (insert (propertize (format "URL: %s\n" url)
                                        'face '(:foreground "#8be9fd"))))
                  (when-let ((tags (cdr (assoc 'tags mem))))
                    (when (> (length tags) 0)
                      (insert (propertize (format "Tags: %s\n" (mapconcat #'identity tags ", "))
                                          'face '(:foreground "#ffb86c")))))
                  (insert (propertize (format "Created: %s\n"
                                              (mp-memory--format-date (cdr (assoc 'created_at mem))))
                                      'face '(:foreground "#6272a4")))
                  (insert "\n")
                  (insert (propertize (make-string 50 ?─) 'face '(:foreground "#6272a4")))
                  (insert "\n\n")
                  ;; Content
                  (let ((content (mcp-memory--get-content mem)))
                    (if content
                        (insert content)
                      (insert (propertize "(No content available)"
                                          'face '(:foreground "#6272a4" :slant italic)))))
                  (goto-char (point-min)))
                (special-mode)
                (local-set-key (kbd "q") #'mp-memory-close-detail))
              ;; Display buffer
              (if (and mp--workarea-window (window-live-p mp--workarea-window))
                  (set-window-buffer mp--workarea-window buf)
                (switch-to-buffer buf))
              (message "Opened: %s" title))))))))



(defun mp-memory--goto-first-item ()
  "Go to the first memory item in the tree."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (get-text-property (point) 'memory-id)))
    (forward-line 1))
  (mp-memory--auto-preview))

(defun mp-memory-tree-select ()
  "Select memory at point and show in preview (legacy, use mp-memory-open)."
  (interactive)
  (mp-memory--auto-preview))


(defun mp-memory-cycle-filter ()
  "Cycle through memory type filters."
  (interactive)
  (let* ((types '(nil "note" "webpage" "github" "arxiv" "pdf" "screenshot"))
         (current (cl-position mp-memory--filter-type types :test #'equal))
         (next-idx (mod (1+ (or current 0)) (length types))))
    (setq mp-memory--filter-type (nth next-idx types))
    (setq mp-memory--selected-id nil)
    (mp-memory--render-tree)
    (message "Filter: %s" (or mp-memory--filter-type "all"))))

(defun mp-memory-delete-at-point ()
  "Delete memory at point."
  (interactive)
  (when-let ((id (get-text-property (point) 'memory-id)))
    (when (yes-or-no-p (format "Delete memory %s? " id))
      (require 'memory)
      (mcp-memory--delete-entry id)
      (when (string= id mp-memory--selected-id)
        (setq mp-memory--selected-id nil)
        (mp-memory--clear-preview))
      (mp-memory--render-tree)
      (message "Deleted: %s" id))))

(defun mp-memory-refresh ()
  "Refresh memory tree."
  (interactive)
  (mp-memory--render-tree)
  (when mp-memory--selected-id
    (mp-memory--show-preview mp-memory--selected-id))
  (message "Refreshed"))

(defun mp-memory-quit ()
  "Quit memory panel."
  (interactive)
  (mp-switch-to 'autopilot))

;;; ============================================================
;;; Preview Pane
;;; ============================================================

(defvar mp-memory-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'mp-memory-quit)
    map)
  "Keymap for memory preview mode.")

(define-derived-mode mp-memory-preview-mode special-mode "MemPreview"
  "Mode for memory preview."
  (setq-local mode-line-format nil))

(defun mp-memory--clear-preview ()
  "Clear the preview buffer."
  (when-let ((buf (get-buffer "*Memory Preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "\n  Select a memory to preview\n"
                            'face '(:foreground "#6272a4" :slant italic)))
        (mp-memory-preview-mode)))))

(defun mp-memory--ensure-preview-window ()
  "Ensure preview window exists and return it."
  (cond
   ;; Preview window is alive, use it
   ((and mp-memory--preview-window (window-live-p mp-memory--preview-window))
    mp-memory--preview-window)
   ;; Find window displaying preview buffer
   ((get-buffer-window "*Memory Preview*")
    (setq mp-memory--preview-window (get-buffer-window "*Memory Preview*"))
    mp-memory--preview-window)
   ;; Create new split from tree window
   ((and mp-memory--tree-window (window-live-p mp-memory--tree-window))
    (let ((preview-win (split-window mp-memory--tree-window nil 'right)))
      (setq mp-memory--preview-window preview-win)
      preview-win))
   ;; Fallback: just display in other window
   (t nil)))

(defun mp-memory--show-preview (id)
  "Show preview for memory with ID."
  (require 'memory)
  (let ((mem (mcp-memory--get-by-id id))
        (buf (get-buffer-create "*Memory Preview*")))
    (when mem
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; Title
          (insert (propertize (format " %s\n\n"
                                      (cdr (assoc 'title mem)))
                              'face '(:foreground "#f8f8f2" :weight bold :height 1.3)))
          ;; Metadata
          (insert (propertize (format "Type: %s\n" (cdr (assoc 'type mem)))
                              'face '(:foreground "#6272a4")))
          (insert (propertize (format "ID: %s\n" id)
                              'face '(:foreground "#6272a4")))
          (when-let ((url (cdr (assoc 'url mem))))
            (insert (propertize (format "URL: %s\n" url)
                                'face '(:foreground "#8be9fd"))))
          (when-let ((tags (cdr (assoc 'tags mem))))
            (when (> (length tags) 0)
              (insert (propertize (format "Tags: %s\n"
                                          (mapconcat #'identity tags ", "))
                                  'face '(:foreground "#ffb86c")))))
          (insert (propertize (format "Created: %s\n"
                                      (mp-memory--format-date
                                       (cdr (assoc 'created_at mem))))
                              'face '(:foreground "#6272a4")))
          ;; Separator
          (insert "\n")
          (insert (propertize (make-string 40 ?─) 'face '(:foreground "#6272a4")))
          (insert "\n\n")
          ;; Content
          (let ((content (mcp-memory--get-content mem)))
            (if content
                (insert content)
              (insert (propertize "(No content available)"
                                  'face '(:foreground "#6272a4" :slant italic)))))
          (goto-char (point-min))
          (mp-memory-preview-mode)))
      ;; Ensure preview is displayed
      (when-let ((win (mp-memory--ensure-preview-window)))
        (set-window-buffer win buf)))))

;;; ============================================================
;;; Panel Setup
;;; ============================================================

(defun mp--setup-memory (_session)
  "Setup Memory panel.
Layout: Tree view on top, Preview pane below (vertical split)."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    ;; Reset state
    (setq mp-memory--filter-type nil)
    (setq mp-memory--selected-id nil)
    ;; Split: tree on top (40%), preview below (60%)
    (let* ((tree-height (floor (* (window-height) 0.40)))
           (tree-win (selected-window))
           (preview-win (split-window-below tree-height)))
      (setq mp-memory--tree-window tree-win)
      (setq mp-memory--preview-window preview-win)
      ;; Tree buffer
      (setq mp-memory--tree-buffer (mp-memory--render-tree))
      (set-window-buffer tree-win mp-memory--tree-buffer)
      ;; Preview buffer
      (setq mp-memory--preview-buffer (get-buffer-create "*Memory Preview*"))
      (mp-memory--clear-preview)
      (set-window-buffer preview-win mp-memory--preview-buffer)
      ;; Focus tree and auto-select first item
      (select-window tree-win)
      (with-current-buffer mp-memory--tree-buffer
        (mp-memory--goto-first-item)))))


(defun mp--teardown-memory (_session)
  "Teardown Memory panel."
  ;; Delete preview window FIRST (before killing buffers)
  ;; This ensures mp--workarea-window regains full width
  (when (and mp-memory--preview-window (window-live-p mp-memory--preview-window))
    (delete-window mp-memory--preview-window))
  ;; Kill buffers
  (when (buffer-live-p mp-memory--tree-buffer)
    (kill-buffer mp-memory--tree-buffer))
  (when (buffer-live-p mp-memory--preview-buffer)
    (kill-buffer mp-memory--preview-buffer))
  ;; Reset all state
  (setq mp-memory--tree-window nil)
  (setq mp-memory--preview-window nil)
  (setq mp-memory--tree-buffer nil)
  (setq mp-memory--preview-buffer nil)
  ;; Clear saved workarea state - Memory panel always needs fresh setup
  ;; because its buffers are killed on teardown
  (when-let ((tab (gethash 'memory mp--feat-tabs)))
    (setf (mp-feat-tab-workarea-state tab) nil)))

;;; ============================================================
;;; Registration
;;; ============================================================

(mp-define-feat-tab memory
  :name "Memory"
  :key "y"
  :icon "🧠"
  :setup #'mp--setup-memory
  :teardown #'mp--teardown-memory)

(provide 'panel-memory)
;;; panel-memory.el ends here
