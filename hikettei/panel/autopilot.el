;;; autopilot.el --- Autopilot Panel with Activity Display -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 0.3.0

;;; Commentary:
;;
;; Autopilot mode with Activity Panel for real-time AI operation visualization.
;; Shows file content with read highlights and edit overlays.
;; Supports 4 review modes: Manual, Hybrid, AutoReview, NoReview.
;; Layout: Single window for file display with review mode indicator.

;;; Code:

(require 'multi-panel)

;; Ensure mcp directory is in load-path for activity-panel
(let* ((this-file (or load-file-name buffer-file-name))
       (panel-dir (and this-file (file-name-directory this-file)))
       (hikettei-dir (and panel-dir (file-name-directory (directory-file-name panel-dir))))
       (mcp-dir (and hikettei-dir (expand-file-name "mcp" hikettei-dir))))
  (when (and mcp-dir (file-directory-p mcp-dir))
    (add-to-list 'load-path mcp-dir)))

;; Now require activity-panel
(require 'activity-panel)
;;; ============================================================
;;; Customization
;;; ============================================================

(defcustom mp-autopilot-panel-ratio 0.6
  "Ratio of Activity Panel width to total WorkArea width.
Value should be between 0.1 and 0.9."
  :type 'number
  :group 'multi-panel)
(defcustom mp-autopilot-default-review-mode 'manual
  "Default review mode for Autopilot."
  :type '(choice (const :tag "Manual - Human reviews all edits" manual)
                 (const :tag "Hybrid - AI reviews, human confirms" hybrid)
                 (const :tag "AutoReview - AI auto-decides" auto-review)
                 (const :tag "NoReview - Auto-approve all" no-review))
  :group 'multi-panel)

;;; ============================================================
;;; State
;;; ============================================================

(defvar mp-autopilot-review-mode 'manual
  "Current review mode: manual, hybrid, auto-review, no-review.")

(defvar mp--autopilot-file-window nil
  "Window for file display in Autopilot mode.")

(defvar mp--autopilot-status-buffer nil
  "Buffer for Autopilot status display.")

(defvar mp--autopilot-saved-revert-settings nil
  "Saved auto-revert settings to restore on teardown.")

(defvar mp-autopilot--review-process nil
  "Background process for AI review.")
;;; ============================================================
;;; Status Buffer
;;; ============================================================

(defun mp--create-autopilot-status-buffer ()
  "Create or get the Autopilot status buffer."
  (let ((buf (get-buffer-create "*Autopilot Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (special-mode)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil))
    (setq mp--autopilot-status-buffer buf)
    buf))

;;; ============================================================
;;; Setup / Teardown
;;; ============================================================

(defun mp--setup-autopilot (session)
  "Setup Autopilot mode - single window for AI file display.
SESSION is the current mcp-session."
  ;; Ensure autorevert is loaded
  (require 'autorevert nil t)
  ;; Save current auto-revert settings and enable silent auto-revert
  (setq mp--autopilot-saved-revert-settings
        (list (cons 'global-auto-revert-mode (and (boundp 'global-auto-revert-mode) global-auto-revert-mode))
              (cons 'auto-revert-verbose (and (boundp 'auto-revert-verbose) auto-revert-verbose))
              (cons 'auto-revert-check-vc-info (and (boundp 'auto-revert-check-vc-info) auto-revert-check-vc-info))))
  (setq auto-revert-verbose nil)
  (setq auto-revert-check-vc-info nil)
  (global-auto-revert-mode 1)
  
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    ;; Single window for file display
    (setq mp--autopilot-file-window (selected-window))
    
    ;; Check for pending review
    (if (and (boundp 'mp--review-pending) mp--review-pending)
        ;; Display pending review
        (let* ((pending-session mp--review-pending)
               (buffer (file-editor-session-file-buffer pending-session))
               (start-line (file-editor-session-start-line pending-session)))
          ;; Clear pending flag first
          (setq mp--review-pending nil)
          (mp--update-feat-tab-bar)
          ;; Display if buffer is live
          (when (buffer-live-p buffer)
            (set-window-buffer mp--autopilot-file-window buffer)
            (with-selected-window mp--autopilot-file-window
              (goto-char (point-min))
              (forward-line (1- start-line))
              (recenter 3))))
      ;; No pending review - show default buffer
      (switch-to-buffer (get-buffer-create "*Autopilot*"))
      (with-current-buffer "*Autopilot*"
        (let ((inhibit-read-only t)
              (mode-name (mp-autopilot--mode-display-name mp-autopilot-review-mode))
              (mode-face (pcase mp-autopilot-review-mode
                           ('manual '(:foreground "#6272a4"))
                           ('hybrid '(:foreground "#f1fa8c"))
                           ('auto-review '(:foreground "#ffb86c"))
                           ('no-review '(:foreground "#ff5555"))
                           (_ '(:foreground "#6272a4")))))
          (erase-buffer)
          (insert (propertize "\n  Autopilot Mode\n\n" 'face '(:foreground "#50fa7b" :weight bold)))
          (insert "  Waiting for AI file access...\n\n")
          (insert "  Files read or edited by AI will appear here.\n\n")
          (insert (propertize "  ─────────────────────────────────────\n" 'face '(:foreground "#44475a")))
          (insert "\n  Review Mode: ")
          (insert (propertize (format "%s" mode-name) 'face (append mode-face '(:weight bold))))
          (insert "\n\n")
          (insert (propertize "  C-x j m" 'face '(:foreground "#bd93f9" :weight bold)))
          (insert (propertize " - Cycle review mode\n" 'face '(:foreground "#6272a4"))))
        (special-mode)))))

(defun mp--teardown-autopilot (session)
  "Teardown Autopilot mode, cleaning up Activity Panel.
SESSION is the current mcp-session."
  ;; Restore auto-revert settings
  (when mp--autopilot-saved-revert-settings
    (let ((saved-global (alist-get 'global-auto-revert-mode mp--autopilot-saved-revert-settings))
          (saved-verbose (alist-get 'auto-revert-verbose mp--autopilot-saved-revert-settings))
          (saved-vc (alist-get 'auto-revert-check-vc-info mp--autopilot-saved-revert-settings)))
      (setq auto-revert-verbose saved-verbose)
      (setq auto-revert-check-vc-info saved-vc)
      (unless saved-global
        (global-auto-revert-mode -1)))
    (setq mp--autopilot-saved-revert-settings nil))
  
  (when (fboundp 'activity-panel-cleanup)
    (activity-panel-cleanup))
  (setq mp--autopilot-file-window nil))

;;; ============================================================
;;; Review Mode Functions
;;; ============================================================

(defun mp-autopilot-cycle-review-mode ()
  "Cycle through review modes."
  (interactive)
  (setq mp-autopilot-review-mode
        (pcase mp-autopilot-review-mode
          ('manual 'hybrid)
          ('hybrid 'auto-review)
          ('auto-review 'no-review)
          ('no-review 'manual)))
  (mp--update-feat-tab-bar)
  ;; Refresh *Autopilot* buffer if it's showing the default view
  (when-let ((buf (get-buffer "*Autopilot*")))
    (when (and (eq mp--current-feat-tab 'autopilot)
               (buffer-live-p buf))
      (mp--setup-autopilot nil)))
  (message "Review mode: %s" (mp-autopilot--mode-display-name mp-autopilot-review-mode)))

(defun mp-autopilot--mode-display-name (mode)
  "Get display name for review MODE."
  (pcase mode
    ('manual "Manual")
    ('hybrid "Hybrid (AI + Human)")
    ('auto-review "AutoReview")
    ('no-review "NoReview")
    (_ "Unknown")))

(defun mp-autopilot--mode-indicator ()
  "Get single-letter indicator for current review mode."
  (pcase mp-autopilot-review-mode
    ('manual "M")
    ('hybrid "H")
    ('auto-review "A")
    ('no-review "N")
    (_ "?")))

;;; ============================================================
;;; Background AI Reviewer
;;; ============================================================

(defun mp-autopilot--get-current-agent-config ()
  "Get config for currently active AI agent.
Returns plist with :executable, :args, etc."
  (when (and (boundp 'ai-session--current) ai-session--current
             (fboundp 'ai-session-agent-config))
    (ai-session-agent-config ai-session--current)))

(defun mp-autopilot--build-review-prompt (file-path start-line end-line original new ai-comment)
  "Build review prompt for AI.
Arguments are FILE-PATH, START-LINE, END-LINE, ORIGINAL content, NEW content, and AI-COMMENT."
  (format "You are a code reviewer. Review this change and respond with ONLY a JSON object.

REQUIRED FORMAT (no other text):
{\"decision\": \"approve\" or \"reject\", \"reason\": \"YOUR EXPLANATION HERE (REQUIRED)\", \"comments\": []}

File: %s (lines %d-%d)

ORIGINAL:
```
%s
```

NEW:
```
%s
```

Author's comment: %s

IMPORTANT:
- The \"reason\" field is REQUIRED - explain WHY you approve or reject
- Output ONLY the JSON object, no markdown, no extra text"
          file-path start-line end-line
          (or original "")
          (or new "")
          (or ai-comment "None")))

(defun mp-autopilot--extract-json (str)
  "Extract JSON object from STR, handling markdown code blocks and extra text."
  (let ((cleaned str))
    ;; Remove markdown code blocks: ```json ... ``` or ``` ... ```
    (when (string-match "```\\(?:json\\)?\\s-*\n?\\([^`]*\\)```" cleaned)
      (setq cleaned (match-string 1 cleaned)))
    ;; Find JSON object by matching braces
    (when (string-match "\\({[^{}]*\\(?:{[^{}]*}[^{}]*\\)*}\\)" cleaned)
      (match-string 1 cleaned))))

(defun mp-autopilot--parse-review-response (output)
  "Parse JSON review response from OUTPUT string.
Returns (decision reason comments) or nil on parse error."
  (when (and output (not (string-empty-p output)))
    ;; Log raw output for debugging (visible in *Messages*)
    (message "AutoReview raw output: %s" (substring output 0 (min 200 (length output))))
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-str (mp-autopilot--extract-json output))
               (parsed (when json-str (json-read-from-string json-str)))
               (decision (alist-get 'decision parsed))
               (reason (alist-get 'reason parsed))
               (comments (alist-get 'comments parsed)))
          (when decision
            (list (if (or (string= decision "approve")
                          (string= decision "approved"))
                      'approve 'reject)
                  (or reason "")
                  (or comments '()))))
      (error
       (message "AutoReview parse error: %s" err)
       nil))))

(defun mp-autopilot--spawn-review-agent (prompt callback)
  "Spawn background review agent with PROMPT, call CALLBACK with result.
CALLBACK receives (decision reason comments) or nil on error."
  (let* ((agent-config (mp-autopilot--get-current-agent-config))
         (executable (and agent-config (plist-get agent-config :executable))))
    (unless executable
      (funcall callback nil)
      (cl-return-from mp-autopilot--spawn-review-agent))
    (let ((output-buffer (generate-new-buffer " *review-output*")))
      ;; Pass prompt directly via -p argument
      (setq mp-autopilot--review-process
            (make-process
             :name "ai-reviewer"
             :buffer output-buffer
             :command (list executable "--print" "-p" prompt)
             :sentinel (lambda (proc event)
                        (when (string-match-p "finished\\|exited" event)
                          (let ((output (with-current-buffer (process-buffer proc)
                                         (buffer-string))))
                            (funcall callback (mp-autopilot--parse-review-response output)))
                          (ignore-errors (kill-buffer output-buffer)))))))))

;;; ============================================================
;;; Review Mode Handlers
;;; ============================================================

(defun mp-autopilot--handle-no-review (session)
  "Handle NoReview mode - auto-approve immediately.
SESSION is the file-editor session."
  (require 'file-editor)
  ;; Delay slightly to allow recursive-edit to start in MCP server
  (run-at-time 0.1 nil
               (lambda (s)
                 (when (fboundp 'file-editor--finalize)
                   (file-editor--finalize s 'approve "Auto-approved (NoReview mode)"))
                 (message "NoReview: Auto-approved"))
               session))

(defun mp-autopilot--handle-auto-review (session)
  "Handle AutoReview mode - spawn AI to auto-decide.
SESSION is the file-editor session."
  (let* ((file-path (file-editor-session-file-path session))
         (start-line (file-editor-session-start-line session))
         (end-line (file-editor-session-end-line session))
         (original (file-editor-session-original-content session))
         (new (file-editor-session-new-content session))
         (ai-comment (file-editor-session-ai-comment session))
         (prompt (mp-autopilot--build-review-prompt
                  file-path start-line end-line original new ai-comment)))
    (message "AutoReview: AI is reviewing...")
    (mp-autopilot--spawn-review-agent
     prompt
     (lambda (result)
       (if result
           (let* ((decision (car result))
                  (raw-reason (cadr result))
                  (reason (if (or (null raw-reason) (string-empty-p raw-reason))
                              (format "AI %s (no detailed reason provided)"
                                      (if (eq decision 'approve) "approved" "rejected"))
                            raw-reason)))
             (if (eq decision 'approve)
                 (file-editor--finalize session 'approve reason)
               (file-editor--finalize session 'request-changes reason))
             (message "AutoReview: %s - %s" decision reason))
         ;; Fallback to manual on parse error
         (message "AutoReview: AI response parse failed, falling back to manual")
         (mp-autopilot--display-for-manual-review session))))))

(defun mp-autopilot--handle-hybrid-review (session)
  "Handle Hybrid mode - AI reviews first, then human confirms.
SESSION is the file-editor session."
  (let* ((file-path (file-editor-session-file-path session))
         (start-line (file-editor-session-start-line session))
         (end-line (file-editor-session-end-line session))
         (original (file-editor-session-original-content session))
         (new (file-editor-session-new-content session))
         (ai-comment (file-editor-session-ai-comment session))
         (prompt (mp-autopilot--build-review-prompt
                  file-path start-line end-line original new ai-comment)))
    (message "Hybrid: AI is pre-reviewing...")
    (mp-autopilot--spawn-review-agent
     prompt
     (lambda (result)
       (if result
           (let ((decision (car result))
                 (reason (cadr result))
                 (comments (caddr result)))
             ;; Add AI comments to session
             (dolist (comment comments)
               (let ((line (alist-get 'line comment))
                     (text (alist-get 'text comment)))
                 (when (and line text)
                   (push (cons line text)
                         (file-editor-session-line-comments session)))))
             ;; Update AI comment with recommendation
             (setf (file-editor-session-ai-comment session)
                   (format "[AI recommends: %s] %s\n%s"
                           (if (eq decision 'approve) "APPROVE" "REJECT")
                           reason
                           (or ai-comment "")))
             ;; Now display for human review
             (mp-autopilot--display-for-manual-review session)
             (message "Hybrid: AI recommends %s - your turn to confirm" decision))
         ;; Fallback to manual on parse error
         (message "Hybrid: AI response parse failed, proceeding with manual review")
         (mp-autopilot--display-for-manual-review session))))))

(defun mp-autopilot--display-for-manual-review (session)
  "Display SESSION for manual review in Autopilot window."
  (require 'file-editor)
  (let ((buffer (file-editor-session-file-buffer session))
        (start-line (file-editor-session-start-line session)))
    ;; Set as current session
    (setq file-editor--current-session session)
    ;; Create overlays
    (when (fboundp 'file-editor--create-diff-overlays)
      (file-editor--create-diff-overlays session))
    ;; Enable review mode in buffer
    (with-current-buffer buffer
      (auto-revert-mode 1)
      (file-editor-review-mode 1))
    ;; Display in Autopilot window
    (let ((autopilot-win (and (boundp 'mp--autopilot-file-window)
                              mp--autopilot-file-window
                              (window-live-p mp--autopilot-file-window)
                              mp--autopilot-file-window)))
      (if autopilot-win
          (progn
            (set-window-buffer autopilot-win buffer)
            (with-selected-window autopilot-win
              (goto-char (point-min))
              (forward-line (1- start-line))
              (recenter 3))
            (when (boundp 'mp--review-pending)
              (setq mp--review-pending nil)))
        ;; Not on Autopilot tab - set pending indicator
        (when (boundp 'mp--review-pending)
          (setq mp--review-pending session)
          (when (fboundp 'mp--update-feat-tab-bar)
            (mp--update-feat-tab-bar)))))))

;;; ============================================================
;;; Feat Tab Registration
;;; ============================================================

(mp-define-feat-tab autopilot
  :name "Autopilot"
  :key "a"
  :icon ""
  :setup #'mp--setup-autopilot
  :teardown #'mp--teardown-autopilot)

;;; ============================================================
;;; Keybindings
;;; ============================================================

;; C-x j m - cycle review mode
(when (boundp 'mp-prefix-map)
  (define-key mp-prefix-map (kbd "m") #'mp-autopilot-cycle-review-mode))

(provide 'panel-autopilot)

;;; autopilot.el ends here