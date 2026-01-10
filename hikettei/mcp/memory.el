;;; memory.el --- Memory System for AI Agents -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 1.0.0
;; Keywords: ai, mcp, memory

;;; Commentary:
;;
;; Persistent memory system for AI agents.
;; Stores WebPages, GitHub repos, arXiv papers, PDFs, and markdown notes.
;; All data stored in {workspace}/.hikettei/memory/
;;
;; Memory Types:
;;   - note: Agent memos in markdown with YAML front matter
;;   - webpage: Web content converted to markdown
;;   - github: Repository metadata + README
;;   - arxiv: Paper metadata + PDF
;;   - pdf: PDF files with extracted metadata
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(declare-function mcp-server--project-root "mcp-server")

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup mcp-memory nil
  "Memory system for AI agents."
  :group 'tools
  :prefix "mcp-memory-")

(defcustom mcp-memory-directory "memory"
  "Subdirectory within .hikettei for memory storage."
  :type 'string
  :group 'mcp-memory)

;;; ============================================================
;;; State
;;; ============================================================

(defvar mcp-memory--index nil
  "In-memory cache of the memory index.")

(defvar mcp-memory--index-dirty nil
  "Non-nil if index has unsaved changes.")

;;; ============================================================
;;; Path Utilities
;;; ============================================================

(defun mcp-memory--workspace ()
  "Get current workspace root."
  (or (and (boundp 'mcp-server--project-root) mcp-server--project-root)
      default-directory))

(defun mcp-memory--memory-dir ()
  "Get the memory directory for current workspace."
  (expand-file-name mcp-memory-directory
                    (expand-file-name ".hikettei" (mcp-memory--workspace))))

(defun mcp-memory--index-file ()
  "Get path to index.json."
  (expand-file-name "index.json" (mcp-memory--memory-dir)))

(defun mcp-memory--ensure-dirs ()
  "Ensure all memory subdirectories exist."
  (let ((base (mcp-memory--memory-dir)))
    (dolist (subdir '("notes" "web" "github" "arxiv" "pdf"))
      (make-directory (expand-file-name subdir base) t))))

;;; ============================================================
;;; Index Management
;;; ============================================================

(defun mcp-memory--load-index ()
  "Load memory index from disk."
  (let ((file (mcp-memory--index-file)))
    (if (file-exists-p file)
        (condition-case err
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'symbol))
              (setq mcp-memory--index (json-read-file file)))
          (error
           (message "Error loading memory index: %s" (error-message-string err))
           (setq mcp-memory--index '((version . "1.0") (memories . nil)))))
      (setq mcp-memory--index '((version . "1.0") (memories . nil)))))
  mcp-memory--index)

(defun mcp-memory--save-index ()
  "Save memory index to disk."
  (mcp-memory--ensure-dirs)
  (let* ((file (mcp-memory--index-file))
         (json-encoding-pretty-print t)
         (json-encoding-default-indentation "  "))
    (with-temp-file file
      (insert (json-encode mcp-memory--index)))
    (setq mcp-memory--index-dirty nil)))

(defun mcp-memory--get-memories ()
  "Get list of memories from index."
  (unless mcp-memory--index (mcp-memory--load-index))
  (cdr (assoc 'memories mcp-memory--index)))

(defun mcp-memory--set-memories (memories)
  "Set MEMORIES list in index."
  (unless mcp-memory--index (mcp-memory--load-index))
  (setcdr (assoc 'memories mcp-memory--index) memories)
  (setq mcp-memory--index-dirty t))

(defun mcp-memory--add-entry (entry)
  "Add ENTRY to memory index and save."
  (let ((memories (mcp-memory--get-memories)))
    (mcp-memory--set-memories (cons entry memories))
    (mcp-memory--save-index)))

(defun mcp-memory--update-entry (id updates)
  "Update entry with ID using UPDATES alist."
  (let ((memories (mcp-memory--get-memories))
        (found nil))
    (setq memories
          (mapcar (lambda (mem)
                    (if (string= id (cdr (assoc 'id mem)))
                        (progn
                          (setq found t)
                          (dolist (update updates)
                            (let ((key (car update))
                                  (val (cdr update)))
                              (if (assoc key mem)
                                  (setcdr (assoc key mem) val)
                                (push update mem))))
                          mem)
                      mem))
                  memories))
    (when found
      (mcp-memory--set-memories memories)
      (mcp-memory--save-index))
    found))

(defun mcp-memory--delete-entry (id)
  "Delete entry with ID from index."
  (let ((memories (mcp-memory--get-memories)))
    (mcp-memory--set-memories
     (cl-remove-if (lambda (mem) (string= id (cdr (assoc 'id mem))))
                   memories))
    (mcp-memory--save-index)))

;;; ============================================================
;;; ID Generation
;;; ============================================================

(defun mcp-memory--generate-id ()
  "Generate unique memory ID."
  (format "mem-%s-%s"
          (format-time-string "%Y%m%d-%H%M%S")
          (substring (md5 (format "%s%s" (random) (current-time))) 0 6)))

(defun mcp-memory--slugify (str)
  "Convert STR to URL-safe slug."
  (let ((slug (downcase str)))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
    (if (> (length slug) 50)
        (substring slug 0 50)
      slug)))

(defun mcp-memory--timestamp ()
  "Get current ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

;;; ============================================================
;;; Note Operations
;;; ============================================================

(defun mcp-memory--store-note (title content &optional tags)
  "Store a note with TITLE, CONTENT, and optional TAGS.
Returns the memory ID."
  (mcp-memory--ensure-dirs)
  (let* ((id (mcp-memory--generate-id))
         (slug (mcp-memory--slugify title))
         (file-path (format "notes/%s.md" slug))
         (full-path (expand-file-name file-path (mcp-memory--memory-dir)))
         (timestamp (mcp-memory--timestamp))
         (tags-list (if (listp tags) tags (list tags))))
    ;; Handle duplicate slug by appending ID suffix
    (when (file-exists-p full-path)
      (setq slug (format "%s-%s" slug (substring id -6)))
      (setq file-path (format "notes/%s.md" slug))
      (setq full-path (expand-file-name file-path (mcp-memory--memory-dir))))
    ;; Write markdown with YAML front matter
    (with-temp-file full-path
      (insert "---\n")
      (insert (format "id: %s\n" id))
      (insert (format "title: \"%s\"\n" (replace-regexp-in-string "\"" "\\\\\"" title)))
      (when tags-list
        (insert "tags:\n")
        (dolist (tag tags-list)
          (insert (format "  - %s\n" tag))))
      (insert (format "created_at: %s\n" timestamp))
      (insert (format "updated_at: %s\n" timestamp))
      (insert "---\n\n")
      (insert content))
    ;; Add to index
    (mcp-memory--add-entry
     `((id . ,id)
       (type . "note")
       (title . ,title)
       (tags . ,tags-list)
       (created_at . ,timestamp)
       (updated_at . ,timestamp)
       (file_path . ,file-path)))
    id))

(defun mcp-memory--update-note (id content &optional title tags)
  "Update note with ID. Set new CONTENT, optionally TITLE and TAGS."
  (let* ((entry (mcp-memory--get-by-id id))
         (file-path (cdr (assoc 'file_path entry)))
         (full-path (expand-file-name file-path (mcp-memory--memory-dir)))
         (timestamp (mcp-memory--timestamp))
         (new-title (or title (cdr (assoc 'title entry))))
         (new-tags (or tags (cdr (assoc 'tags entry)))))
    (when (and entry (file-exists-p full-path))
      ;; Rewrite file
      (with-temp-file full-path
        (insert "---\n")
        (insert (format "id: %s\n" id))
        (insert (format "title: \"%s\"\n" (replace-regexp-in-string "\"" "\\\\\"" new-title)))
        (when new-tags
          (insert "tags:\n")
          (dolist (tag new-tags)
            (insert (format "  - %s\n" tag))))
        (insert (format "created_at: %s\n" (cdr (assoc 'created_at entry))))
        (insert (format "updated_at: %s\n" timestamp))
        (insert "---\n\n")
        (insert content))
      ;; Update index
      (mcp-memory--update-entry id
                                `((title . ,new-title)
                                  (tags . ,new-tags)
                                  (updated_at . ,timestamp)))
      t)))

;;; ============================================================
;;; WebPage Operations
;;; ============================================================

(defun mcp-memory--fetch-url-content (url)
  "Fetch content from URL and return as string."
  (let ((buffer (url-retrieve-synchronously url t t 30)))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            ;; Skip HTTP headers
            (when (re-search-forward "\n\n" nil t)
              (buffer-substring-no-properties (point) (point-max))))
        (kill-buffer buffer)))))

(defun mcp-memory--html-to-text (html)
  "Convert HTML to plain text using shr."
  (with-temp-buffer
    (insert html)
    (let ((shr-inhibit-images t)
          (shr-use-fonts nil))
      (condition-case nil
          (progn
            (shr-render-region (point-min) (point-max))
            (buffer-string))
        (error html)))))

(defun mcp-memory--store-webpage (url &optional title tags)
  "Store webpage from URL with optional TITLE and TAGS.
Returns the memory ID."
  (mcp-memory--ensure-dirs)
  (let* ((html (mcp-memory--fetch-url-content url))
         (text (if html (mcp-memory--html-to-text html) ""))
         (id (mcp-memory--generate-id))
         (hash (substring (md5 url) 0 8))
         (file-path (format "web/%s.md" hash))
         (full-path (expand-file-name file-path (mcp-memory--memory-dir)))
         (timestamp (mcp-memory--timestamp))
         (actual-title (or title (format "Web: %s" (url-host (url-generic-parse-url url)))))
         (tags-list (if (listp tags) tags (list tags))))
    ;; Write content
    (with-temp-file full-path
      (insert (format "<!-- URL: %s -->\n" url))
      (insert (format "<!-- Fetched: %s -->\n\n" timestamp))
      (insert text))
    ;; Add to index
    (mcp-memory--add-entry
     `((id . ,id)
       (type . "webpage")
       (title . ,actual-title)
       (url . ,url)
       (tags . ,tags-list)
       (created_at . ,timestamp)
       (updated_at . ,timestamp)
       (file_path . ,file-path)))
    id))

;;; ============================================================
;;; GitHub Operations
;;; ============================================================

(defun mcp-memory--parse-github-url (url)
  "Parse GitHub URL and return (owner . repo) or nil."
  (when (string-match "github\\.com/\\([^/]+\\)/\\([^/]+\\)" url)
    (cons (match-string 1 url)
          (replace-regexp-in-string "\\.git$" "" (match-string 2 url)))))

(defun mcp-memory--fetch-github-api (endpoint)
  "Fetch from GitHub API ENDPOINT and return parsed JSON."
  (let* ((url (format "https://api.github.com%s" endpoint))
         (url-request-extra-headers '(("Accept" . "application/vnd.github.v3+json")
                                      ("User-Agent" . "Emacs-MCP-Memory")))
         (content (mcp-memory--fetch-url-content url)))
    (when content
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (condition-case nil
            (json-read-from-string content)
          (error nil))))))

(defun mcp-memory--store-github (url &optional tags)
  "Store GitHub repository info from URL with optional TAGS.
Returns the memory ID."
  (mcp-memory--ensure-dirs)
  (let* ((parsed (mcp-memory--parse-github-url url))
         (owner (car parsed))
         (repo (cdr parsed)))
    (unless parsed
      (error "Invalid GitHub URL: %s" url))
    (let* ((api-data (mcp-memory--fetch-github-api (format "/repos/%s/%s" owner repo)))
           (readme-data (mcp-memory--fetch-github-api (format "/repos/%s/%s/readme" owner repo)))
           (readme-content (when readme-data
                            (condition-case nil
                                (base64-decode-string (cdr (assoc 'content readme-data)))
                              (error ""))))
           (id (mcp-memory--generate-id))
           (file-path (format "github/%s_%s.json" owner repo))
           (full-path (expand-file-name file-path (mcp-memory--memory-dir)))
           (timestamp (mcp-memory--timestamp))
           (title (format "%s/%s" owner repo))
           (tags-list (if (listp tags) tags (list tags)))
           (data `((owner . ,owner)
                   (repo . ,repo)
                   (full_name . ,title)
                   (description . ,(cdr (assoc 'description api-data)))
                   (url . ,url)
                   (stars . ,(cdr (assoc 'stargazers_count api-data)))
                   (forks . ,(cdr (assoc 'forks_count api-data)))
                   (language . ,(cdr (assoc 'language api-data)))
                   (topics . ,(cdr (assoc 'topics api-data)))
                   (readme . ,readme-content)
                   (fetched_at . ,timestamp))))
      ;; Write JSON data
      (with-temp-file full-path
        (insert (let ((json-encoding-pretty-print t))
                  (json-encode data))))
      ;; Add to index
      (mcp-memory--add-entry
       `((id . ,id)
         (type . "github")
         (title . ,title)
         (url . ,url)
         (tags . ,(append tags-list (cdr (assoc 'topics api-data))))
         (created_at . ,timestamp)
         (updated_at . ,timestamp)
         (file_path . ,file-path)
         (summary . ,(cdr (assoc 'description api-data)))))
      id)))

;;; ============================================================
;;; arXiv Operations
;;; ============================================================

(defun mcp-memory--parse-arxiv-url (url)
  "Parse arXiv URL and return arxiv ID or nil."
  (when (string-match "arxiv\\.org/\\(?:abs\\|pdf\\)/\\([0-9.]+\\)" url)
    (match-string 1 url)))

(defun mcp-memory--fetch-arxiv-metadata (arxiv-id)
  "Fetch arXiv metadata for ARXIV-ID from API."
  (let* ((api-url (format "http://export.arxiv.org/api/query?id_list=%s" arxiv-id))
         (content (mcp-memory--fetch-url-content api-url)))
    (when content
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (let ((title "")
              (abstract "")
              (authors '())
              (categories '())
              (published ""))
          ;; Parse XML (simple regex-based parsing)
          (when (re-search-forward "<title>\\([^<]+\\)</title>" nil t)
            (setq title (string-trim (match-string 1))))
          (when (re-search-forward "<summary>\\([^<]+\\)</summary>" nil t)
            (setq abstract (string-trim (match-string 1))))
          (goto-char (point-min))
          (while (re-search-forward "<author>.*?<name>\\([^<]+\\)</name>" nil t)
            (push (match-string 1) authors))
          (goto-char (point-min))
          (while (re-search-forward "<category[^>]*term=\"\\([^\"]+\\)\"" nil t)
            (push (match-string 1) categories))
          (goto-char (point-min))
          (when (re-search-forward "<published>\\([^<]+\\)</published>" nil t)
            (setq published (match-string 1)))
          `((title . ,title)
            (abstract . ,abstract)
            (authors . ,(nreverse authors))
            (categories . ,(nreverse categories))
            (published . ,published)))))))

(defun mcp-memory--download-file (url dest)
  "Download file from URL to DEST path."
  (let ((buffer (url-retrieve-synchronously url t t 60)))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let ((content (buffer-substring-no-properties (point) (point-max))))
                (with-temp-file dest
                  (set-buffer-multibyte nil)
                  (insert content))
                t)))
        (kill-buffer buffer)))))

(defun mcp-memory--store-arxiv (url &optional tags)
  "Store arXiv paper from URL with optional TAGS.
Downloads PDF and metadata. Returns the memory ID."
  (mcp-memory--ensure-dirs)
  (let ((arxiv-id (mcp-memory--parse-arxiv-url url)))
    (unless arxiv-id
      (error "Invalid arXiv URL: %s" url))
    (let* ((metadata (mcp-memory--fetch-arxiv-metadata arxiv-id))
           (id (mcp-memory--generate-id))
           (json-path (format "arxiv/%s.json" arxiv-id))
           (pdf-path (format "arxiv/%s.pdf" arxiv-id))
           (json-full (expand-file-name json-path (mcp-memory--memory-dir)))
           (pdf-full (expand-file-name pdf-path (mcp-memory--memory-dir)))
           (pdf-url (format "https://arxiv.org/pdf/%s.pdf" arxiv-id))
           (timestamp (mcp-memory--timestamp))
           (title (or (cdr (assoc 'title metadata)) (format "arXiv:%s" arxiv-id)))
           (tags-list (if (listp tags) tags (list tags)))
           (data `((arxiv_id . ,arxiv-id)
                   (title . ,title)
                   (authors . ,(cdr (assoc 'authors metadata)))
                   (abstract . ,(cdr (assoc 'abstract metadata)))
                   (categories . ,(cdr (assoc 'categories metadata)))
                   (published . ,(cdr (assoc 'published metadata)))
                   (pdf_url . ,pdf-url)
                   (fetched_at . ,timestamp))))
      ;; Write JSON metadata
      (with-temp-file json-full
        (insert (let ((json-encoding-pretty-print t))
                  (json-encode data))))
      ;; Download PDF
      (mcp-memory--download-file pdf-url pdf-full)
      ;; Add to index
      (mcp-memory--add-entry
       `((id . ,id)
         (type . "arxiv")
         (title . ,title)
         (url . ,url)
         (tags . ,(append tags-list (cdr (assoc 'categories metadata))))
         (created_at . ,timestamp)
         (updated_at . ,timestamp)
         (file_path . ,json-path)
         (pdf_path . ,pdf-path)
         (summary . ,(cdr (assoc 'abstract metadata)))))
      id)))

;;; ============================================================
;;; PDF Operations
;;; ============================================================

(defun mcp-memory--store-pdf (file-path &optional title tags)
  "Store local PDF from FILE-PATH with optional TITLE and TAGS.
Returns the memory ID."
  (mcp-memory--ensure-dirs)
  (unless (file-exists-p file-path)
    (error "PDF file not found: %s" file-path))
  (let* ((id (mcp-memory--generate-id))
         (hash (substring (md5 file-path) 0 8))
         (dest-pdf (format "pdf/%s.pdf" hash))
         (dest-json (format "pdf/%s.json" hash))
         (pdf-full (expand-file-name dest-pdf (mcp-memory--memory-dir)))
         (json-full (expand-file-name dest-json (mcp-memory--memory-dir)))
         (timestamp (mcp-memory--timestamp))
         (actual-title (or title (file-name-nondirectory file-path)))
         (tags-list (if (listp tags) tags (list tags)))
         (data `((original_path . ,file-path)
                 (title . ,actual-title)
                 (stored_at . ,timestamp))))
    ;; Copy PDF
    (copy-file file-path pdf-full t)
    ;; Write metadata JSON
    (with-temp-file json-full
      (insert (let ((json-encoding-pretty-print t))
                (json-encode data))))
    ;; Add to index
    (mcp-memory--add-entry
     `((id . ,id)
       (type . "pdf")
       (title . ,actual-title)
       (tags . ,tags-list)
       (created_at . ,timestamp)
       (updated_at . ,timestamp)
       (file_path . ,dest-json)
       (pdf_path . ,dest-pdf)))
    id))

;;; ============================================================
;;; Search & Retrieval
;;; ============================================================

(defun mcp-memory--get-by-id (id)
  "Get memory entry by ID."
  (cl-find-if (lambda (mem) (string= id (cdr (assoc 'id mem))))
              (mcp-memory--get-memories)))

(defun mcp-memory--search (query &optional type tags limit)
  "Search memories matching QUERY, TYPE filter, TAGS filter with LIMIT results."
  (let* ((memories (mcp-memory--get-memories))
         (limit (or limit 10))
         (type-filter (and type (not (string= type "all")) type))
         (results '()))
    (dolist (mem memories)
      (when (and
             ;; Type filter
             (or (null type-filter)
                 (string= type-filter (cdr (assoc 'type mem))))
             ;; Tags filter (all tags must match)
             (or (null tags) (= 0 (length tags))
                 (let ((mem-tags (cdr (assoc 'tags mem))))
                   (cl-every (lambda (tag)
                               (cl-find tag mem-tags :test #'string=))
                             tags)))
             ;; Query filter
             (or (null query) (string-empty-p query)
                 (mcp-memory--matches-query mem query)))
        (push mem results)))
    (seq-take (nreverse results) limit)))

(defun mcp-memory--matches-query (mem query)
  "Check if MEM matches QUERY in title, tags, or summary."
  (let ((title (or (cdr (assoc 'title mem)) ""))
        (tags (or (cdr (assoc 'tags mem)) '()))
        (summary (or (cdr (assoc 'summary mem)) ""))
        (query-lower (downcase query)))
    (or (string-match-p (regexp-quote query-lower) (downcase title))
        (string-match-p (regexp-quote query-lower) (downcase summary))
        (cl-some (lambda (tag)
                   (string-match-p (regexp-quote query-lower) (downcase tag)))
                 tags))))

(defun mcp-memory--get-content (mem)
  "Get full content for memory MEM."
  (let* ((file-path (cdr (assoc 'file_path mem)))
         (full-path (expand-file-name file-path (mcp-memory--memory-dir))))
    (when (file-exists-p full-path)
      (with-temp-buffer
        (insert-file-contents full-path)
        (buffer-string)))))

;;; ============================================================
;;; Screenshot Support
;;; ============================================================

(defun mcp-memory--screenshots-dir ()
  "Get screenshots directory path."
  (expand-file-name "screen_shots"
                    (expand-file-name ".hikettei" (mcp-memory--workspace))))

(defun mcp-memory--scan-screenshots ()
  "Scan screenshots directory and return list of screenshot entries."
  (let ((dir (mcp-memory--screenshots-dir))
        (screenshots '()))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.png$"))
        (let* ((filename (file-name-nondirectory file))
               (id (concat "screenshot-" (file-name-sans-extension filename)))
               (title (if (string-match "screenshot-\\([0-9]+-[0-9]+\\)" filename)
                          (format "Screenshot %s" (match-string 1 filename))
                        filename))
               (mtime (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                          (file-attribute-modification-time
                                           (file-attributes file)))))
          (push `((id . ,id)
                  (type . "screenshot")
                  (title . ,title)
                  (file . ,file)
                  (created_at . ,mtime))
                screenshots))))
    (nreverse screenshots)))

(defun mcp-memory--list (&optional type limit offset)
  "List memories with optional TYPE filter, LIMIT and OFFSET."
  (let* ((memories (mcp-memory--get-memories))
         ;; Add screenshots if type is nil (all) or "screenshot"
         (all-memories (if (or (null type) (string= type "all") (string= type "screenshot"))
                           (append memories (mcp-memory--scan-screenshots))
                         memories))
         (type-filter (and type (not (string= type "all")) type))
         (limit (or limit 50))
         (offset (or offset 0))
         (filtered (if type-filter
                       (cl-remove-if-not
                        (lambda (mem) (string= type-filter (cdr (assoc 'type mem))))
                        all-memories)
                     all-memories))
         ;; Sort by created_at descending
         (sorted (sort filtered
                       (lambda (a b)
                         (string> (or (cdr (assoc 'created_at a)) "")
                                  (or (cdr (assoc 'created_at b)) ""))))))
    (seq-take (seq-drop sorted offset) limit)))


;;; ============================================================
;;; MCP Tool Handlers
;;; ============================================================

(defun mcp-memory--tool-note (args)
  "Handle memory_note tool. Create or update markdown note.
ARGS: title, content, tags (optional), id (optional for update)."
  (condition-case err
      (let ((title (cdr (assoc 'title args)))
            (content (cdr (assoc 'content args)))
            (tags (cdr (assoc 'tags args)))
            (update-id (cdr (assoc 'id args))))
        (unless title (error "Missing required parameter: title"))
        (unless content (error "Missing required parameter: content"))
        (if update-id
            ;; Update existing note
            (if (mcp-memory--update-note update-id content title tags)
                (format "Updated note: %s" update-id)
              (error "Note not found: %s" update-id))
          ;; Create new note
          (let ((id (mcp-memory--store-note title content tags)))
            (format "Created note: %s\nTitle: %s\nTags: %s"
                    id title (if tags (string-join tags ", ") "none")))))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-memory--tool-store (args)
  "Handle memory_store tool. Store webpage/github/arxiv/pdf.
ARGS: type, url (or path for pdf), title (optional), tags (optional)."
  (condition-case err
      (let ((type (cdr (assoc 'type args)))
            (url (cdr (assoc 'url args)))
            (title (cdr (assoc 'title args)))
            (tags (cdr (assoc 'tags args))))
        (unless type (error "Missing required parameter: type"))
        (unless url (error "Missing required parameter: url"))
        (let ((id (pcase type
                    ("webpage" (mcp-memory--store-webpage url title tags))
                    ("github" (mcp-memory--store-github url tags))
                    ("arxiv" (mcp-memory--store-arxiv url tags))
                    ("pdf" (mcp-memory--store-pdf url title tags))
                    (_ (error "Unknown type: %s" type)))))
          (format "Stored %s: %s\nID: %s" type url id)))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-memory--tool-search (args)
  "Handle memory_search tool.
ARGS: query (optional), type (optional), tags (optional), limit (optional)."
  (condition-case err
      (let* ((query (cdr (assoc 'query args)))
             (type (cdr (assoc 'type args)))
             (tags (cdr (assoc 'tags args)))
             (limit (or (cdr (assoc 'limit args)) 10))
             (results (mcp-memory--search query type tags limit)))
        (if results
            (let ((output ""))
              (dolist (mem results)
                (setq output
                      (concat output
                              (format "- [%s] %s (ID: %s)\n"
                                      (cdr (assoc 'type mem))
                                      (cdr (assoc 'title mem))
                                      (cdr (assoc 'id mem)))
                              (when-let ((summary (cdr (assoc 'summary mem))))
                                (format "  Summary: %s\n"
                                        (if (> (length summary) 100)
                                            (concat (substring summary 0 100) "...")
                                          summary)))
                              (when-let ((tags (cdr (assoc 'tags mem))))
                                (format "  Tags: %s\n" (string-join tags ", "))))))
              (format "Found %d memories:\n\n%s" (length results) output))
          "No memories found matching your criteria."))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-memory--tool-get (args)
  "Handle memory_get tool. Retrieve full content by ID.
ARGS: id, include_content (optional, default true)."
  (condition-case err
      (let* ((id (cdr (assoc 'id args)))
             (include-content (not (eq (cdr (assoc 'include_content args)) :json-false)))
             (mem (mcp-memory--get-by-id id)))
        (unless id (error "Missing required parameter: id"))
        (unless mem (error "Memory not found: %s" id))
        (let ((output (format "ID: %s\nType: %s\nTitle: %s\nCreated: %s\n"
                              (cdr (assoc 'id mem))
                              (cdr (assoc 'type mem))
                              (cdr (assoc 'title mem))
                              (cdr (assoc 'created_at mem)))))
          (when-let ((url (cdr (assoc 'url mem))))
            (setq output (concat output (format "URL: %s\n" url))))
          (when-let ((tags (cdr (assoc 'tags mem))))
            (setq output (concat output (format "Tags: %s\n" (string-join tags ", ")))))
          (when include-content
            (let ((content (mcp-memory--get-content mem)))
              (when content
                (setq output (concat output "\n--- Content ---\n\n" content)))))
          output))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-memory--tool-list (args)
  "Handle memory_list tool.
ARGS: type (optional), limit (optional), offset (optional)."
  (condition-case err
      (let* ((type (cdr (assoc 'type args)))
             (limit (or (cdr (assoc 'limit args)) 50))
             (offset (or (cdr (assoc 'offset args)) 0))
             (results (mcp-memory--list type limit offset)))
        (if results
            (let ((output ""))
              (dolist (mem results)
                (setq output
                      (concat output
                              (format "- [%s] %s\n  ID: %s | Created: %s\n"
                                      (cdr (assoc 'type mem))
                                      (cdr (assoc 'title mem))
                                      (cdr (assoc 'id mem))
                                      (cdr (assoc 'created_at mem))))))
              (format "Memories (showing %d-%d):\n\n%s"
                      (1+ offset)
                      (+ offset (length results))
                      output))
          "No memories found."))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-memory--tool-delete (args)
  "Handle memory_delete tool. Delete memory by ID.
ARGS: id."
  (condition-case err
      (let* ((id (cdr (assoc 'id args)))
             (mem (mcp-memory--get-by-id id)))
        (unless id (error "Missing required parameter: id"))
        (unless mem (error "Memory not found: %s" id))
        (let ((file-path (cdr (assoc 'file_path mem)))
              (pdf-path (cdr (assoc 'pdf_path mem))))
          ;; Delete files
          (when file-path
            (let ((full (expand-file-name file-path (mcp-memory--memory-dir))))
              (when (file-exists-p full) (delete-file full))))
          (when pdf-path
            (let ((full (expand-file-name pdf-path (mcp-memory--memory-dir))))
              (when (file-exists-p full) (delete-file full))))
          ;; Remove from index
          (mcp-memory--delete-entry id)
          (format "Deleted memory: %s (%s)" id (cdr (assoc 'title mem)))))
    (error (format "Error: %s" (error-message-string err)))))

;;; ============================================================
;;; Tool Definitions (for mcp-server.el integration)
;;; ============================================================

(defconst mcp-memory--tools
  '(((name . "memory_note")
     (description . "Create or update a markdown note. Use for memos, plans, observations, research summaries.")
     (inputSchema . ((type . "object")
                     (properties . ((title . ((type . "string")
                                              (description . "Note title")))
                                    (content . ((type . "string")
                                                (description . "Markdown content")))
                                    (tags . ((type . "array")
                                             (items . ((type . "string")))
                                             (description . "Tags for categorization")))
                                    (id . ((type . "string")
                                           (description . "Optional: ID of existing note to update")))))
                     (required . ("title" "content")))))

    ((name . "memory_store")
     (description . "Store external content: webpage, github repo, arxiv paper, or local PDF.")
     (inputSchema . ((type . "object")
                     (properties . ((type . ((type . "string")
                                             (enum . ("webpage" "github" "arxiv" "pdf"))
                                             (description . "Type of content to store")))
                                    (url . ((type . "string")
                                            (description . "URL (or file path for PDF)")))
                                    (title . ((type . "string")
                                              (description . "Optional title override")))
                                    (tags . ((type . "array")
                                             (items . ((type . "string")))
                                             (description . "Tags for categorization")))))
                     (required . ("type" "url")))))

    ((name . "memory_search")
     (description . "Search memories by keyword, type, or tags.")
     (inputSchema . ((type . "object")
                     (properties . ((query . ((type . "string")
                                              (description . "Search query (searches title, tags, summary)")))
                                    (type . ((type . "string")
                                             (enum . ("all" "note" "webpage" "github" "arxiv" "pdf"))
                                             (description . "Filter by memory type")))
                                    (tags . ((type . "array")
                                             (items . ((type . "string")))
                                             (description . "Filter by tags (AND logic)")))
                                    (limit . ((type . "integer")
                                              (default . 10)
                                              (description . "Max results to return")))))
                     (required . []))))

    ((name . "memory_get")
     (description . "Retrieve full content of a memory by ID.")
     (inputSchema . ((type . "object")
                     (properties . ((id . ((type . "string")
                                           (description . "Memory ID")))
                                    (include_content . ((type . "boolean")
                                                        (default . t)
                                                        (description . "Include full content or just metadata")))))
                     (required . ("id")))))

    ((name . "memory_list")
     (description . "List all memories, optionally filtered by type.")
     (inputSchema . ((type . "object")
                     (properties . ((type . ((type . "string")
                                             (enum . ("all" "note" "webpage" "github" "arxiv" "pdf"))))
                                    (limit . ((type . "integer")
                                              (default . 50)))
                                    (offset . ((type . "integer")
                                               (default . 0)))))
                     (required . []))))

    ((name . "memory_delete")
     (description . "Delete a memory by ID.")
     (inputSchema . ((type . "object")
                     (properties . ((id . ((type . "string")
                                           (description . "Memory ID to delete")))))
                     (required . ("id"))))))
  "Memory system MCP tool definitions.")

(provide 'memory)
;;; memory.el ends here
