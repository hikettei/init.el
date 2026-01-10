;;; screenshot.el --- Screenshot utilities for MCP -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 1.0.0
;; Keywords: screenshot, mcp, tools

;;; Commentary:
;;
;; Provides screenshot functionality for AI agents via MCP.
;; Uses Emacs built-in `x-export-frames` (Emacs 27+) for capturing
;; the entire Emacs frame without user interaction.
;;
;; Inspired by screenshot.el by rubikitch:
;; https://www.emacswiki.org/emacs/download/screenshot.el
;;

;;; Code:

(require 'cl-lib)

;;; ============================================================
;;; Screenshot Capture
;;; ============================================================

(defun mcp-screenshot--ensure-directory (dir)
  "Ensure DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun mcp-screenshot--generate-filename ()
  "Generate a timestamped filename for screenshot."
  (format-time-string "screenshot-%Y%m%d-%H%M%S.png"))

(defun mcp-screenshot-take (output-path &optional frame)
  "Take a screenshot of FRAME and save to OUTPUT-PATH.
FRAME defaults to the selected frame.
Returns the absolute path of the saved screenshot."
  (let* ((frame (or frame (selected-frame)))
         (dir (file-name-directory output-path)))
    ;; Ensure output directory exists
    (mcp-screenshot--ensure-directory dir)
    ;; Use x-export-frames for PNG output (requires Emacs 27+)
    (if (fboundp 'x-export-frames)
        (progn
          ;; x-export-frames returns image data, write to file
          (let ((data (x-export-frames nil (intern "png"))))
            (with-temp-file output-path
              (set-buffer-multibyte nil)
              (insert data)))
          (expand-file-name output-path))
      ;; Fallback: use ImageMagick import command
      (mcp-screenshot--take-with-import output-path frame))))

(defun mcp-screenshot--take-with-import (output-path frame)
  "Take screenshot using ImageMagick import command.
OUTPUT-PATH is the destination file.
FRAME is used to get the window ID."
  (let* ((window-id (frame-parameter frame 'outer-window-id))
         (cmd (if window-id
                  (format "import -window %s %s" window-id
                          (shell-quote-argument output-path))
                ;; If no window-id, capture root window (entire screen)
                (format "import -window root %s"
                        (shell-quote-argument output-path)))))
    (if (zerop (call-process-shell-command cmd))
        (expand-file-name output-path)
      (error "Failed to capture screenshot with import command"))))

;;; ============================================================
;;; Public API for MCP Server
;;; ============================================================

(defun mcp-screenshot-capture (workspace-dir &optional filename)
  "Capture Emacs frame screenshot for AI agents.
WORKSPACE-DIR is the project root directory.
FILENAME is optional; if nil, generates timestamped name.
Saves to WORKSPACE-DIR/.hikettei/screen_shots/
Returns the relative path from workspace root."
  (let* ((filename (or filename (mcp-screenshot--generate-filename)))
         (screenshot-dir (expand-file-name ".hikettei/screen_shots/" workspace-dir))
         (output-path (expand-file-name filename screenshot-dir))
         (saved-path (mcp-screenshot-take output-path)))
    ;; Return relative path from workspace
    (file-relative-name saved-path workspace-dir)))

(provide 'mcp-screenshot)

;;; screenshot.el ends here
