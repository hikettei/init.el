;;; voicebox.el --- VOICEVOX Text-to-Speech MCP Tools -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 1.0.0
;; Keywords: ai, mcp, voice, tts

;;; Commentary:
;;
;; VOICEVOX text-to-speech integration for AI agents.
;; Provides voice feedback during coding sessions.
;;
;; Modes:
;;   - disabled: No voice (default)
;;   - minimum: Voice notification on task completion
;;   - maximum: Frequent human-like voice updates
;;
;; Usage:
;;   C-x j v - Cycle through voicebox modes
;;
;; Cache: {workspace}/.hikettei/sound_cache/*.wav

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

(declare-function mcp-server--project-root "mcp-server")

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup mcp-voicebox nil
  "VOICEVOX text-to-speech for AI agents."
  :group 'tools
  :prefix "mcp-voicebox-")

(defcustom mcp-voicebox-api-url "http://127.0.0.1:50021"
  "VOICEVOX API base URL."
  :type 'string
  :group 'mcp-voicebox)

(defcustom mcp-voicebox-app-path "/Applications/VOICEVOX.app"
  "Path to VOICEVOX application."
  :type 'string
  :group 'mcp-voicebox)

(defcustom mcp-voicebox-speaker-id nil
  "VOICEVOX speaker style ID. Auto-detected if nil."
  :type '(choice (const nil) integer)
  :group 'mcp-voicebox)

(defcustom mcp-voicebox-speaker-name "冥鳴ひまり"
  "Preferred speaker name to search for."
  :type 'string
  :group 'mcp-voicebox)

(defcustom mcp-voicebox-cache-max-files 20
  "Maximum number of cached audio files to keep."
  :type 'integer
  :group 'mcp-voicebox)

;;; ============================================================
;;; State
;;; ============================================================

(defvar mcp-voicebox-mode 'disabled
  "Current voicebox mode: disabled, minimum, maximum.")

(defvar mcp-voicebox--server-checked nil
  "Non-nil if server availability has been checked this session.")

(defvar mcp-voicebox--server-available nil
  "Non-nil if VOICEVOX server is available.")

(defvar mcp-voicebox--current-process nil
  "Current ffplay process for audio playback.")

(defvar mcp-voicebox--cached-speaker-id nil
  "Cached speaker ID after first lookup.")

;;; ============================================================
;;; Path Utilities
;;; ============================================================

(defun mcp-voicebox--workspace ()
  "Get current workspace root."
  (or (and (boundp 'mcp-server--project-root) mcp-server--project-root)
      default-directory))

(defun mcp-voicebox--cache-dir ()
  "Get the sound cache directory for current workspace."
  (expand-file-name "sound_cache"
                    (expand-file-name ".hikettei" (mcp-voicebox--workspace))))

(defun mcp-voicebox--ensure-cache-dir ()
  "Ensure sound cache directory exists."
  (make-directory (mcp-voicebox--cache-dir) t))

;;; ============================================================
;;; Cache Management
;;; ============================================================

(defun mcp-voicebox--cleanup-cache ()
  "Delete old audio files, keeping only `mcp-voicebox-cache-max-files' most recent."
  (let ((dir (mcp-voicebox--cache-dir)))
    (when (file-directory-p dir)
      (let* ((files (directory-files dir t "\\.wav$"))
             (sorted (sort files
                           (lambda (a b)
                             (time-less-p (nth 5 (file-attributes b))
                                          (nth 5 (file-attributes a))))))
             (to-delete (nthcdr mcp-voicebox-cache-max-files sorted)))
        (dolist (f to-delete)
          (ignore-errors (delete-file f)))))))

(defun mcp-voicebox--cache-path (text)
  "Get cache file path for TEXT (using hash)."
  (let ((hash (md5 text)))
    (expand-file-name (format "voice-%s.wav" hash)
                      (mcp-voicebox--cache-dir))))

;;; ============================================================
;;; VOICEVOX Server Interaction
;;; ============================================================

(defun mcp-voicebox--server-running-p ()
  "Check if VOICEVOX server is running."
  (condition-case nil
      (let ((url-request-method "GET")
            (url-show-status nil)
            (url (format "%s/version" mcp-voicebox-api-url)))
        (with-current-buffer (url-retrieve-synchronously url t t 2)
          (prog1 t (kill-buffer))))
    (error nil)))

(defun mcp-voicebox--start-server ()
  "Attempt to start VOICEVOX application."
  (when (and mcp-voicebox-app-path
             (file-exists-p mcp-voicebox-app-path))
    (message "Starting VOICEVOX...")
    (call-process "open" nil 0 nil "-a" mcp-voicebox-app-path)
    ;; Wait for server to start (max 15 seconds)
    (let ((attempts 0)
          (max-attempts 30))
      (while (and (< attempts max-attempts)
                  (not (mcp-voicebox--server-running-p)))
        (sleep-for 0.5)
        (setq attempts (1+ attempts)))
      (mcp-voicebox--server-running-p))))

(defun mcp-voicebox--ensure-server ()
  "Ensure VOICEVOX server is available."
  (unless mcp-voicebox--server-checked
    (setq mcp-voicebox--server-available (mcp-voicebox--server-running-p))
    (unless mcp-voicebox--server-available
      (setq mcp-voicebox--server-available (mcp-voicebox--start-server)))
    (setq mcp-voicebox--server-checked t))
  mcp-voicebox--server-available)

(defun mcp-voicebox--fetch-speakers ()
  "Fetch available speakers from VOICEVOX API."
  (condition-case err
      (let* ((url-request-method "GET")
             (url-show-status nil)
             (url (format "%s/speakers" mcp-voicebox-api-url))
             (buffer (url-retrieve-synchronously url t t 10)))
        (when buffer
          (unwind-protect
              (with-current-buffer buffer
                (goto-char (point-min))
                (when (re-search-forward "\n\n" nil t)
                  (let ((json-object-type 'alist)
                        (json-array-type 'list))
                    (json-read))))
            (kill-buffer buffer))))
    (error
     (message "Failed to fetch speakers: %s" (error-message-string err))
     nil)))

(defun mcp-voicebox--find-speaker-id (speaker-name)
  "Find style_id for SPEAKER-NAME from available speakers."
  (let ((speakers (mcp-voicebox--fetch-speakers)))
    (catch 'found
      (dolist (speaker speakers)
        (when (string= (cdr (assoc 'name speaker)) speaker-name)
          (let ((styles (cdr (assoc 'styles speaker))))
            (when styles
              ;; Return first style's id
              (throw 'found (cdr (assoc 'id (car styles))))))))
      nil)))

(defun mcp-voicebox--get-speaker-id ()
  "Get speaker ID, using cache or looking up."
  (or mcp-voicebox-speaker-id
      mcp-voicebox--cached-speaker-id
      (when (mcp-voicebox--ensure-server)
        (let ((id (mcp-voicebox--find-speaker-id mcp-voicebox-speaker-name)))
          (if id
              (setq mcp-voicebox--cached-speaker-id id)
            (message "Warning: Speaker '%s' not found, using default (id=14)"
                     mcp-voicebox-speaker-name)
            (setq mcp-voicebox--cached-speaker-id 14))
          mcp-voicebox--cached-speaker-id))))

;;; ============================================================
;;; Audio Synthesis & Playback
;;; ============================================================

(defun mcp-voicebox--audio-query (text speaker-id)
  "Create audio query for TEXT with SPEAKER-ID."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-show-status nil)
         (url (format "%s/audio_query?text=%s&speaker=%d"
                      mcp-voicebox-api-url
                      (url-hexify-string text)
                      speaker-id))
         (buffer (url-retrieve-synchronously url t t 30)))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (buffer-substring-no-properties (point) (point-max))))
        (kill-buffer buffer)))))

(defun mcp-voicebox--synthesis (audio-query speaker-id output-path)
  "Synthesize audio from AUDIO-QUERY with SPEAKER-ID to OUTPUT-PATH."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string audio-query 'utf-8))
         (url-show-status nil)
         (url (format "%s/synthesis?speaker=%d"
                      mcp-voicebox-api-url
                      speaker-id))
         (buffer (url-retrieve-synchronously url t t 60)))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let ((coding-system-for-write 'binary))
                (write-region (point) (point-max) output-path nil 'silent))
              t))
        (kill-buffer buffer)))))

(defun mcp-voicebox--play-audio (wav-path)
  "Play WAV file at WAV-PATH using ffplay."
  ;; Kill any existing playback
  (when (and mcp-voicebox--current-process
             (process-live-p mcp-voicebox--current-process))
    (kill-process mcp-voicebox--current-process))
  ;; Start new playback (ffplay -nodisp -autoexit for headless playback)
  (setq mcp-voicebox--current-process
        (start-process "voicebox-play" nil
                       "ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet"
                       wav-path)))

(defun mcp-voicebox--speak (text)
  "Synthesize and play TEXT as speech. Returns success message or error."
  (condition-case err
      (progn
        (unless (mcp-voicebox--ensure-server)
          (error "VOICEVOX server not available"))
        (let ((speaker-id (mcp-voicebox--get-speaker-id)))
          (unless speaker-id
            (error "Failed to get speaker ID"))
          (mcp-voicebox--ensure-cache-dir)
          ;; Check cache first
          (let ((cache-path (mcp-voicebox--cache-path text)))
            (unless (file-exists-p cache-path)
              ;; Generate audio
              (let ((query (mcp-voicebox--audio-query text speaker-id)))
                (unless query
                  (error "Failed to create audio query"))
                (unless (mcp-voicebox--synthesis query speaker-id cache-path)
                  (error "Failed to synthesize audio"))))
            ;; Play audio
            (mcp-voicebox--play-audio cache-path)
            ;; Cleanup old cache files
            (mcp-voicebox--cleanup-cache)
            (format "Speaking: %s" (truncate-string-to-width text 50)))))
    (error (format "Error: %s" (error-message-string err)))))

;;; ============================================================
;;; Mode Management
;;; ============================================================

(defun mcp-voicebox-cycle-mode ()
  "Cycle through voicebox modes: disabled -> minimum -> maximum -> disabled."
  (interactive)
  (setq mcp-voicebox-mode
        (pcase mcp-voicebox-mode
          ('disabled 'minimum)
          ('minimum 'maximum)
          ('maximum 'disabled)))
  ;; Reset server check when enabling
  (when (not (eq mcp-voicebox-mode 'disabled))
    (setq mcp-voicebox--server-checked nil))
  ;; Update tab bar to show new mode
  (when (fboundp 'mp--update-feat-tab-bar)
    (mp--update-feat-tab-bar))
  ;; Refresh Autopilot panel if it's active
  (when (and (boundp 'mp--current-feat-tab)
             (eq mp--current-feat-tab 'autopilot)
             (fboundp 'mp--setup-autopilot))
    (mp--setup-autopilot nil))
  (message "Voicebox mode: %s" (mcp-voicebox--mode-display-name mcp-voicebox-mode)))

(defun mcp-voicebox--mode-display-name (mode)
  "Get display name for MODE."
  (pcase mode
    ('disabled "Disabled (Off)")
    ('minimum "Minimum (Task completion)")
    ('maximum "Maximum (Verbose)")
    (_ "Unknown")))

(defun mcp-voicebox--mode-indicator ()
  "Get indicator for current mode (for tab bar)."
  (pcase mcp-voicebox-mode
    ('disabled "🔇")
    ('minimum "🔈")
    ('maximum "🔊")
    (_ "?")))

(defun mcp-voicebox-enabled-p ()
  "Return t if voicebox is enabled (not disabled)."
  (not (eq mcp-voicebox-mode 'disabled)))

;;; ============================================================
;;; MCP Tool Handler
;;; ============================================================

(defun mcp-voicebox--tool-speak (args)
  "Handle voicebox_speak tool. ARGS: text (required)."
  (condition-case err
      (let ((text (cdr (assoc 'text args))))
        (unless text
          (error "Missing required parameter: text"))
        (if (mcp-voicebox-enabled-p)
            (mcp-voicebox--speak text)
          "Voicebox is disabled. User can enable with C-x j v."))
    (error (format "Error: %s" (error-message-string err)))))

;;; ============================================================
;;; Tool Definition
;;; ============================================================

(defconst mcp-voicebox--tools
  `(((name . "voicebox_speak")
     (description . "Speak text using VOICEVOX text-to-speech. Only works when voicebox mode is enabled (minimum or maximum). Use this to provide voice feedback to the user in Japanese.

Usage Guidelines based on mode:
- In 'minimum' mode: Use ONLY for task completion notifications. Example: 'タスクが完了しました。' or '実装が終わりました。確認をお願いします。'
- In 'maximum' mode: Use for progress updates, explanations, and human-like interaction. Example: 'ファイルを読み込んでいます。', 'このコードを修正しますね。', 'ちょっと苦戦しています...'

Important:
- Speak in natural Japanese
- Keep messages concise (under 100 characters)
- The tool returns status - if it says 'disabled', stop trying to use it")
     (inputSchema . ((type . "object")
                     (properties . ((text . ((type . "string")
                                             (description . "Text to speak in Japanese")))))
                     (required . ["text"])))))
  "Voicebox MCP tool definitions.")

(provide 'voicebox)
;;; voicebox.el ends here
