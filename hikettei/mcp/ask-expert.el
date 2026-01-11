;;; ask-expert.el --- Ask External Expert MCP Tool -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 1.0.0
;; Keywords: ai, mcp, expert

;;; Commentary:
;;
;; MCP tool for consulting external AI experts (GPT-5.2 Pro, etc.)
;; via a manual, asynchronous workflow that respects API usage policies.
;;
;; Workflow:
;; 1. Agent calls ask_expert with a detailed question
;; 2. Question is saved to .hikettei/DISCUSSION.md
;; 3. Discussion panel opens showing the Q&A board
;; 4. User manually queries the external AI (via web interface)
;; 5. User pastes the answer into the Discussion panel
;; 6. User resumes the agent session
;;
;; This asynchronous approach avoids automated API calls that might
;; violate terms of service while still leveraging expert AI knowledge.

;;; Code:

(require 'cl-lib)
(require 'json)

;;; ============================================================
;;; State
;;; ============================================================

(defvar ask-expert--current-question nil
  "Current pending question awaiting answer.")

(defvar ask-expert--discussion-file nil
  "Path to the current DISCUSSION.md file.")

;;; ============================================================
;;; Discussion File Management
;;; ============================================================

(defun ask-expert--get-discussion-path ()
  "Get path to DISCUSSION.md in current workspace."
  (let ((workspace (or (bound-and-true-p mcp-server--project-root)
                       default-directory)))
    (expand-file-name ".hikettei/DISCUSSION.md" workspace)))

(defun ask-expert--ensure-discussion-file ()
  "Ensure DISCUSSION.md exists with proper structure."
  (let ((path (ask-expert--get-discussion-path)))
    (make-directory (file-name-directory path) t)
    (unless (file-exists-p path)
      (with-temp-file path
        (insert "# Expert Discussion Board\n\n"
                "This file tracks Q&A sessions with external AI experts.\n\n"
                "---\n\n")))
    (setq ask-expert--discussion-file path)
    path))

(defun ask-expert--append-question (title question context)
  "Append a new QUESTION with TITLE and CONTEXT to DISCUSSION.md."
  (let ((path (ask-expert--ensure-discussion-file))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
        (id (format "Q-%s" (format-time-string "%Y%m%d%H%M%S"))))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-max))
      (insert (format "\n## %s\n\n" id))
      (insert (format "**Title:** %s\n\n" (or title "Untitled")))
      (insert (format "**Timestamp:** %s\n\n" timestamp))
      (insert "### Question\n\n")
      (insert "```\n")
      (insert question)
      (insert "\n```\n\n")
      (when (and context (not (string-empty-p context)))
        (insert "### Context\n\n")
        (insert context)
        (insert "\n\n"))
      (insert "### Answer\n\n")
      (insert "_Awaiting response from external expert..._\n\n")
      (insert "---\n")
      (write-region (point-min) (point-max) path nil 'silent))
    (setq ask-expert--current-question id)
    id))

(defun ask-expert--get-latest-question ()
  "Get the latest question from DISCUSSION.md."
  (let ((path (ask-expert--get-discussion-path)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-max))
        ;; Find the last Question section
        (when (re-search-backward "^### Question\n\n```\n" nil t)
          (forward-line 2)
          (let ((start (point)))
            (when (re-search-forward "^```$" nil t)
              (buffer-substring-no-properties start (match-beginning 0)))))))))

(defun ask-expert--get-latest-answer ()
  "Get the latest answer from DISCUSSION.md (if filled in)."
  (let ((path (ask-expert--get-discussion-path)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-max))
        (when (re-search-backward "^### Answer\n\n" nil t)
          (forward-line 2)
          (let ((start (point))
                (end (or (save-excursion
                           (when (re-search-forward "^---$" nil t)
                             (match-beginning 0)))
                         (point-max))))
            (let ((content (string-trim (buffer-substring-no-properties start end))))
              (unless (string-match-p "^_Awaiting response" content)
                content))))))))

(defun ask-expert--get-entry-by-title (title)
  "Get Q&A entry by TITLE. Returns alist with id, title, question, answer."
  (let ((path (ask-expert--get-discussion-path)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        ;; Search for the title
        (when (re-search-forward (format "^\\*\\*Title:\\*\\* %s$" (regexp-quote title)) nil t)
          ;; Go back to find the ID
          (let (id question answer)
            (save-excursion
              (when (re-search-backward "^## \\(Q-[0-9]+\\)" nil t)
                (setq id (match-string 1))))
            ;; Get question
            (when (re-search-forward "^### Question\n\n```\n" nil t)
              (let ((q-start (point)))
                (when (re-search-forward "^```$" nil t)
                  (setq question (buffer-substring-no-properties q-start (match-beginning 0))))))
            ;; Get answer
            (when (re-search-forward "^### Answer\n\n" nil t)
              (let ((a-start (point))
                    (a-end (or (save-excursion
                                 (when (re-search-forward "^---$" nil t)
                                   (match-beginning 0)))
                               (point-max))))
                (setq answer (string-trim (buffer-substring-no-properties a-start a-end)))))
            ;; Return entry if found
            (when id
              `((id . ,id)
                (title . ,title)
                (question . ,question)
                (answer . ,(if (string-match-p "^_Awaiting response" (or answer ""))
                               nil
                             answer))))))))))

(defun ask-expert--list-entries ()
  "List all Q&A entries. Returns list of (id . title) pairs."
  (let ((path (ask-expert--get-discussion-path))
        (entries '()))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (re-search-forward "^## \\(Q-[0-9]+\\)" nil t)
          (let ((id (match-string 1))
                (title nil))
            (save-excursion
              (forward-line 1)
              (when (looking-at "^\\*\\*Title:\\*\\* \\(.+\\)$")
                (setq title (match-string 1))))
            (push (cons id (or title "Untitled")) entries)))))
    (nreverse entries)))

;;; ============================================================
;;; MCP Tool Definition
;;; ============================================================

(defvar ask-expert--tools
  `(;; Tool 1: ask_expert
    ((name . "ask_expert")
     (description . "Request consultation from an external AI expert.

**CRITICAL PROTOCOL**: This is an ASYNCHRONOUS workflow.

After calling this tool, you MUST:
1. STOP all autonomous actions
2. Wait for user to resume (may take 10 min to hours)
3. DO NOT continue working or call other tools

The user will manually:
- Copy your question from Discussion panel
- Query external AI (GPT-5.2 Pro, etc.)
- Paste answer back
- Resume conversation

Use for: prior art research, debugging stuck errors, architecture decisions.")
     (inputSchema
      . ((type . "object")
         (properties
          . ((title . ((type . "string")
                       (description . "Short title for lookup")))
             (question . ((type . "string")
                          (description . "Detailed question with full context")))
             (context . ((type . "string")
                         (description . "Additional context")))
             (urgency . ((type . "string")
                         (enum . ["low" "medium" "high"])))))
         (required . ["title" "question"]))))
    
    ;; Tool 2: get_expert_answer
    ((name . "get_expert_answer")
     (description . "Retrieve Q&A pair by title from Discussion Board.")
     (inputSchema
      . ((type . "object")
         (properties
          . ((title . ((type . "string")
                       (description . "Exact title to retrieve")))))
         (required . ["title"]))))
    
    ;; Tool 3: check_expert_answer
    ((name . "check_expert_answer")
     (description . "Check if answer available for most recent question.")
     (inputSchema
      . ((type . "object")
         (properties . ,(make-hash-table :test 'equal))
         (required . []))))))

;;; ============================================================
;;; Tool Handlers
;;; ============================================================

(declare-function mp-switch-to "multi-panel")
(declare-function discussion-board-refresh "panel-discussion")

(defun ask-expert--tool-ask (args)
  "Handle ask_expert tool call."
  (let* ((title (alist-get "title" args nil nil #'string=))
         (question (alist-get "question" args nil nil #'string=))
         (context (or (alist-get "context" args nil nil #'string=) ""))
         (urgency (or (alist-get "urgency" args nil nil #'string=) "medium")))
    (unless title
      (error "Missing required parameter: title"))
    (unless question
      (error "Missing required parameter: question"))
    
    ;; Save question to DISCUSSION.md
    (let ((question-id (ask-expert--append-question title question context)))
      
      ;; Switch to Discussion panel
      (when (fboundp 'mp-switch-to)
        (mp-switch-to 'discussion))
      
      ;; Refresh and expand the new entry
      (when (fboundp 'discussion-board-refresh)
        (puthash question-id t discussion-board--expanded)
        (discussion-board-refresh))
      
      ;; Return the protocol message
      (format "Question registered: %s
Title: %s
Urgency: %s

=== AGENT PROTOCOL ===
Your question has been saved to the Discussion Board.

**YOU MUST NOW STOP AND WAIT.**

The user will:
1. Copy your question from the Discussion panel (click [Copy])
2. Consult an external AI expert manually (GPT-5.2 Pro, etc.)
3. Paste the answer into the Discussion panel
4. Resume this conversation

**DO NOT continue with any other actions until the user resumes with the expert's answer.**

Use `get_expert_answer(\"%s\")` to retrieve the answer when resuming.
======================"
              question-id title urgency title))))

(defun ask-expert--tool-check-answer (_args)
  "Handle check_expert_answer tool call."
  (let ((answer (ask-expert--get-latest-answer))
        (question (ask-expert--get-latest-question)))
    (if answer
        (format "Expert answer received:

=== ORIGINAL QUESTION ===
%s
========================

=== EXPERT'S ANSWER ===
%s
======================="
                (or question "(question not found)")
                answer)
      "No answer has been provided yet. Please wait for the user to consult the external expert and provide their response.")))

(defun ask-expert--tool-get-answer (args)
  "Handle get_expert_answer tool call."
  (let* ((title (alist-get "title" args nil nil #'string=)))
    (unless title
      (error "Missing required parameter: title"))
    (let ((entry (ask-expert--get-entry-by-title title)))
      (if entry
          (let ((question (alist-get 'question entry))
                (answer (alist-get 'answer entry)))
            (if answer
                (format "Expert answer for \"%s\":

=== QUESTION ===
%s
================

=== ANSWER ===
%s
==============="
                        title
                        (or question "(question not found)")
                        answer)
              (format "Question \"%s\" found but no answer yet.

=== QUESTION ===
%s
================

The user has not yet provided the expert's answer. Please wait."
                      title (or question "(question not found)"))))
        (format "No question found with title: \"%s\"

Available questions:
%s"
                title
                (let ((entries (ask-expert--list-entries)))
                  (if entries
                      (mapconcat (lambda (e) (format "  - %s: %s" (car e) (cdr e)))
                                 entries "\n")
                    "  (none)")))))))

(provide 'ask-expert)

;;; ask-expert.el ends here
