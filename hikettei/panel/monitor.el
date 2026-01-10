;;; monitor.el --- Monitor Panel -*- lexical-binding: t; -*-

;;; Commentary:
;; MCP Serverのログを表示

;;; Code:

(require 'multi-panel)

(defvar mp--monitor-timer nil
  "Timer for auto-refreshing monitor buffer.")

(defun mp--setup-monitor (session)
  "Setup Monitor mode - MCP Server logs."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    (let ((log-buf (get-buffer-create "*MCP Server*")))
      (switch-to-buffer log-buf)
      (with-current-buffer log-buf
        (goto-char (point-max))
        ;; Add status header if empty
        (when (= (point-min) (point-max))
          (insert "═══════════════════════════════════════\n")
          (insert "  MCP Server Log\n")
          (insert "═══════════════════════════════════════\n\n"))
        ;; Show current status
        (mp--monitor-show-status))))
  ;; Start auto-scroll timer
  (when mp--monitor-timer
    (cancel-timer mp--monitor-timer))
  (setq mp--monitor-timer
        (run-with-timer 1 1 #'mp--monitor-auto-scroll)))

(defun mp--monitor-show-status ()
  "Show current MCP server status in log buffer."
  (let ((buf (get-buffer "*MCP Server*")))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (let ((session (and (boundp 'ai-session--current) ai-session--current)))
          (insert (format "[%s] === Session Status ===\n"
                          (format-time-string "%H:%M:%S")))
          (if session
              (progn
                (insert (format "  Session: %s\n" (ai-session-title session)))
                (insert (format "  Agent: %s\n" (ai-session-agent session)))
                (insert (format "  Workspace: %s\n" (ai-session-workspace session)))
                (let ((mcp-server (ai-session-mcp-server session)))
                  (if (and mcp-server (consp mcp-server))
                      (insert (format "  MCP Server: Running on port %d\n" (cdr mcp-server)))
                    (insert "  MCP Server: NOT RUNNING\n")))
                (let ((config-file (ai-session-mcp-config-file session)))
                  (if config-file
                      (insert (format "  MCP Config: %s\n" config-file))
                    (insert "  MCP Config: NOT GENERATED\n"))))
            (insert "  No active session\n"))
          (insert "\n"))))))

(defun mp--monitor-auto-scroll ()
  "Auto-scroll MCP Server buffer to bottom."
  (when-let ((buf (get-buffer "*MCP Server*")))
    (dolist (win (get-buffer-window-list buf nil t))
      (with-selected-window win
        (goto-char (point-max))
        (recenter -1)))))

(mp-define-feat-tab monitor
  :name "Monitor"
  :key "d"
  :icon ""
  :setup #'mp--setup-monitor)

(provide 'panel-monitor)

;;; monitor.el ends here
