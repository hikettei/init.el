;;; browser.el --- WebKit Browser MCP Tools -*- lexical-binding: t; -*-

;; Author: hikettei
;; Version: 1.0.0
;; Keywords: ai, mcp, browser, webkit

;;; Commentary:
;;
;; MCP tools for controlling xwidget-webkit browser from AI agents.
;; Provides navigation, content extraction, and interaction capabilities.
;;
;; Tools:
;;   - browser_navigate: Open URL
;;   - browser_back/forward/reload: Navigation
;;   - browser_get_state: Get current URL and title
;;   - browser_get_content: Get page content (text/HTML)
;;   - browser_get_links: Get all links on page
;;   - browser_click: Click element by selector
;;   - browser_type: Type into input field
;;   - browser_scroll: Scroll page
;;   - browser_execute_js: Execute arbitrary JavaScript
;;   - browser_screenshot: Take screenshot
;;   - browser_wait: Wait for element to appear

;;; Code:

(require 'xwidget)

(declare-function mcp-server--project-root "mcp-server")

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun mcp-browser--workspace ()
  "Get current workspace root."
  (or (and (boundp 'mcp-server--project-root) mcp-server--project-root)
      default-directory))

(defun mcp-browser--get-webkit ()
  "Get active xwidget-webkit session or error."
  (or (xwidget-webkit-current-session)
      (error "No active WebKit browser. Open Explore panel first (C-x j r).")))

(defvar mcp-browser--js-result nil
  "Temporary storage for JavaScript execution result.")
(defvar mcp-browser--js-result nil
  "Temporary storage for JavaScript execution result.")

(defun mcp-browser--execute-js-sync (xw script &optional timeout)
  "Execute SCRIPT in xwidget XW synchronously and return result.
TIMEOUT is max wait time in seconds (default 5)."
  (setq mcp-browser--js-result nil)
  (let ((timeout (or timeout 5))
        (start (float-time)))
    (xwidget-webkit-execute-script xw script
      (lambda (result) (setq mcp-browser--js-result result)))
    ;; Wait for result with timeout - use sit-for to process events
    (while (and (null mcp-browser--js-result)
                (< (- (float-time) start) timeout))
      (sit-for 0.05))
    (or mcp-browser--js-result "undefined")))

(defun mcp-browser--escape-js-string (str)
  "Escape STR for use in JavaScript string literal."
  (replace-regexp-in-string
   "'" "\\\\'"
   (replace-regexp-in-string
    "\n" "\\\\n"
    (replace-regexp-in-string
     "\r" "\\\\r"
     (replace-regexp-in-string
      "\\\\" "\\\\\\\\" str)))))
;;; ============================================================
;;; Tool Handlers
;;; ============================================================

(defun mcp-browser--tool-open (args)
  "Open browser by switching to Explore panel."
  (condition-case err
      (let ((url (cdr (assoc 'url args))))
        ;; Switch to Explore panel
        (when (fboundp 'mp-switch-to)
          (mp-switch-to 'explore))
        ;; Wait a bit for webkit to initialize
        (sit-for 0.5)
        ;; Navigate to URL if provided
        (if url
            (progn
              (xwidget-webkit-goto-url url)
              (format "Browser opened and navigated to: %s" url))
          "Browser opened (Explore panel)"))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-navigate (args)
  "Navigate to URL using existing webkit session."
  (condition-case err
      (let ((url (cdr (assoc 'url args))))
        (unless url (error "Missing required parameter: url"))
        (mcp-browser--get-webkit) ;; Ensure webkit exists
        (xwidget-webkit-goto-url url)
        (format "Navigated to: %s" url))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-back (_args)
  "Go back in history."
  (condition-case err
      (progn
        (xwidget-webkit-back (mcp-browser--get-webkit))
        "Navigated back")
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-forward (_args)
  "Go forward in history."
  (condition-case err
      (progn
        (xwidget-webkit-forward (mcp-browser--get-webkit))
        "Navigated forward")
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-reload (_args)
  "Reload current page."
  (condition-case err
      (progn
        (xwidget-webkit-reload (mcp-browser--get-webkit))
        "Page reloaded")
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-get-state (_args)
  "Get browser state."
  (condition-case err
      (let ((xw (mcp-browser--get-webkit)))
        (format "URL: %s\nTitle: %s"
                (xwidget-webkit-uri xw)
                (xwidget-webkit-title xw)))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-get-content (args)
  "Get page content via JS."
  (condition-case err
      (let* ((xw (mcp-browser--get-webkit))
             (selector (or (cdr (assoc 'selector args)) "body"))
             (format-type (or (cdr (assoc 'format args)) "text"))
             (prop (if (string= format-type "html") "innerHTML" "innerText"))
             (script (format "document.querySelector('%s')?.%s || ''"
                             (mcp-browser--escape-js-string selector) prop))
             (result (mcp-browser--execute-js-sync xw script)))
        (or result ""))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-get-links (_args)
  "Get all links on page."
  (condition-case err
      (let* ((xw (mcp-browser--get-webkit))
             (script "JSON.stringify(Array.from(document.querySelectorAll('a[href]')).map(a => ({text: a.innerText.trim().substring(0, 100), href: a.href})).filter(l => l.text && l.href).slice(0, 100))")
             (result (mcp-browser--execute-js-sync xw script)))
        (or result "[]"))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-click (args)
  "Click element by selector."
  (condition-case err
      (let* ((xw (mcp-browser--get-webkit))
             (selector (cdr (assoc 'selector args))))
        (unless selector (error "Missing required parameter: selector"))
        (let ((script (format "document.querySelector('%s')?.click()"
                              (mcp-browser--escape-js-string selector))))
          (xwidget-webkit-execute-script xw script)
          (format "Clicked: %s" selector)))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-type (args)
  "Type text into input."
  (condition-case err
      (let* ((xw (mcp-browser--get-webkit))
             (selector (cdr (assoc 'selector args)))
             (text (cdr (assoc 'text args))))
        (unless selector (error "Missing required parameter: selector"))
        (unless text (error "Missing required parameter: text"))
        (let ((script (format "const el = document.querySelector('%s'); if(el){el.value = '%s'; el.dispatchEvent(new Event('input', {bubbles:true})); el.dispatchEvent(new Event('change', {bubbles:true}));}"
                              (mcp-browser--escape-js-string selector)
                              (mcp-browser--escape-js-string text))))
          (xwidget-webkit-execute-script xw script)
          (format "Typed into %s: %s" selector text)))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-scroll (args)
  "Scroll page."
  (condition-case err
      (let* ((xw (mcp-browser--get-webkit))
             (direction (cdr (assoc 'direction args)))
             (amount (or (cdr (assoc 'amount args)) 300)))
        (unless direction (error "Missing required parameter: direction"))
        (let* ((delta (if (string= direction "up") (- amount) amount))
               (script (format "window.scrollBy(0, %d)" delta)))
          (xwidget-webkit-execute-script xw script)
          (format "Scrolled %s by %d pixels" direction amount)))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-execute-js (args)
  "Execute arbitrary JavaScript."
  (condition-case err
      (let* ((xw (mcp-browser--get-webkit))
             (script (cdr (assoc 'script args))))
        (unless script (error "Missing required parameter: script"))
        (let ((result (mcp-browser--execute-js-sync xw script)))
          (or result "undefined")))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-screenshot (_args)
  "Take browser screenshot via Emacs frame."
  (condition-case err
      (let* ((filename (format "browser-%s.png" (format-time-string "%Y%m%d-%H%M%S")))
             (dir (expand-file-name "screen_shots"
                    (expand-file-name ".hikettei" (mcp-browser--workspace))))
             (path (expand-file-name filename dir)))
        (make-directory dir t)
        (call-process "screencapture" nil nil nil "-x" path)
        (format "Screenshot saved: %s\nUse Read tool to view the image." path))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-browser--tool-wait (args)
  "Wait for element to appear."
  (condition-case err
      (let* ((xw (mcp-browser--get-webkit))
             (selector (cdr (assoc 'selector args)))
             (timeout (or (cdr (assoc 'timeout args)) 10)))
        (unless selector (error "Missing required parameter: selector"))
        (let ((start (float-time))
              (found nil))
          (while (and (not found) (< (- (float-time) start) timeout))
            (let ((result (mcp-browser--execute-js-sync xw
                            (format "document.querySelector('%s') !== null"
                                    (mcp-browser--escape-js-string selector)) 1)))
              (if (string= result "true")
                  (setq found t)
                (sleep-for 0.5))))
          (if found
              (format "Element found: %s" selector)
            (format "Timeout after %ds waiting for: %s" timeout selector))))
    (error (format "Error: %s" (error-message-string err)))))

;;; ============================================================
;;; Tool Definitions
;;; ============================================================

(defconst mcp-browser--tools
  '(((name . "browser_open")
     (description . "Open the browser by switching to the Explore panel. Call this first if browser is not active. Optionally navigate to a URL.")
     (inputSchema . ((type . "object")
                     (properties . ((url . ((type . "string")
                                            (description . "Optional URL to navigate to after opening"))))))))

    ((name . "browser_navigate")
     (description . "Navigate the WebKit browser to a URL. Requires browser to be open first.")
     (inputSchema . ((type . "object")
                     (properties . ((url . ((type . "string")
                                            (description . "URL to navigate to")))))
                     (required . ("url")))))

    ((name . "browser_back")
     (description . "Go back to previous page in browser history")
     (inputSchema . ((type . "object")
                     (properties . ()))))

    ((name . "browser_forward")
     (description . "Go forward to next page in browser history")
     (inputSchema . ((type . "object")
                     (properties . ()))))

    ((name . "browser_reload")
     (description . "Reload current page")
     (inputSchema . ((type . "object")
                     (properties . ()))))

    ((name . "browser_get_state")
     (description . "Get current browser state including URL and page title")
     (inputSchema . ((type . "object")
                     (properties . ()))))

    ((name . "browser_get_content")
     (description . "Get page content as text or HTML. Use selector to target specific elements.")
     (inputSchema . ((type . "object")
                     (properties . ((selector . ((type . "string")
                                                 (description . "CSS selector (default: body)")))
                                    (format . ((type . "string")
                                               (enum . ("text" "html"))
                                               (description . "Output format: text or html (default: text)"))))))))

    ((name . "browser_get_links")
     (description . "Get all links on the page as JSON array with text and href properties")
     (inputSchema . ((type . "object")
                     (properties . ()))))

    ((name . "browser_click")
     (description . "Click an element by CSS selector")
     (inputSchema . ((type . "object")
                     (properties . ((selector . ((type . "string")
                                                 (description . "CSS selector for element to click")))))
                     (required . ("selector")))))

    ((name . "browser_type")
     (description . "Type text into an input element. Triggers input and change events.")
     (inputSchema . ((type . "object")
                     (properties . ((selector . ((type . "string")
                                                 (description . "CSS selector for input element")))
                                    (text . ((type . "string")
                                             (description . "Text to type")))))
                     (required . ("selector" "text")))))

    ((name . "browser_scroll")
     (description . "Scroll the page up or down by specified pixels")
     (inputSchema . ((type . "object")
                     (properties . ((direction . ((type . "string")
                                                  (enum . ("up" "down"))
                                                  (description . "Scroll direction")))
                                    (amount . ((type . "integer")
                                               (description . "Pixels to scroll (default: 300)")))))
                     (required . ("direction")))))

    ((name . "browser_execute_js")
     (description . "Execute arbitrary JavaScript code in the browser and return the result")
     (inputSchema . ((type . "object")
                     (properties . ((script . ((type . "string")
                                               (description . "JavaScript code to execute")))))
                     (required . ("script")))))

    ((name . "browser_screenshot")
     (description . "Take a screenshot of the current Emacs frame including the browser. Returns the file path.")
     (inputSchema . ((type . "object")
                     (properties . ()))))

    ((name . "browser_wait")
     (description . "Wait for an element to appear on the page. Polls every 0.5 seconds until found or timeout.")
     (inputSchema . ((type . "object")
                     (properties . ((selector . ((type . "string")
                                                 (description . "CSS selector to wait for")))
                                    (timeout . ((type . "integer")
                                                (description . "Timeout in seconds (default: 10)")))))
                     (required . ("selector"))))))
  "Browser MCP tool definitions.")

(provide 'browser)
;;; browser.el ends here
