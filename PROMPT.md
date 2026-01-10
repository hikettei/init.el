# emacs_eval Snippets

A quick reference for frequently used `emacs_eval` operations in this Emacs configuration.
Keep this file updated when you discover useful patterns.

---

## Review Modes

### Switch to Manual Mode (Default)

```elisp
(progn
  (setq mp-autopilot-review-mode 'manual)
  (mp--update-feat-tab-bar)
  "Switched to Manual mode")
```

### Switch to Hybrid Mode

```elisp
(progn
  (setq mp-autopilot-review-mode 'hybrid)
  (mp--update-feat-tab-bar)
  "Switched to Hybrid mode")
```

### Switch to AutoReview Mode

```elisp
(progn
  (setq mp-autopilot-review-mode 'auto-review)
  (mp--update-feat-tab-bar)
  "Switched to AutoReview mode")
```

### Switch to NoReview Mode

```elisp
(progn
  (setq mp-autopilot-review-mode 'no-review)
  (mp--update-feat-tab-bar)
  "Switched to NoReview mode")
```

### Check Current Review Mode

```elisp
(format "Current review mode: %s" mp-autopilot-review-mode)
```

---

## File Reloading

### Reload Single File

```elisp
(load-file (expand-file-name "hikettei/panel/autopilot.el" user-emacs-directory))
```

### Reload Multiple Files

```elisp
(progn
  (load-file (expand-file-name "hikettei/multi-panel.el" user-emacs-directory))
  (load-file (expand-file-name "hikettei/mcp/file-editor.el" user-emacs-directory))
  (load-file (expand-file-name "hikettei/mcp/mcp-server.el" user-emacs-directory))
  (load-file (expand-file-name "hikettei/panel/autopilot.el" user-emacs-directory))
  "Reloaded all panel files")
```

### Reload and Refresh Autopilot

```elisp
(progn
  (load-file (expand-file-name "hikettei/panel/autopilot.el" user-emacs-directory))
  (when (eq mp--current-feat-tab 'autopilot)
    (mp--setup-autopilot nil))
  "Reloaded and refreshed Autopilot")
```

---

## UI Updates

### Update Tab Bar

```elisp
(mp--update-feat-tab-bar)
```

### Switch to Autopilot Panel

```elisp
(mp-switch-to 'autopilot)
```

### Switch to Freeform Panel

```elisp
(mp-switch-to 'freeform)
```

### Toggle NeoTree

```elisp
(mp-toggle-neotree)
```

### Toggle AI Chat

```elisp
(mp-toggle-ai-chat)
```

---

## Debugging State

### Check Current Session

```elisp
(when (boundp 'ai-session--current)
  (list 'workspace (ai-session-workspace ai-session--current)
        'agent (ai-session-agent ai-session--current)
        'title (ai-session-title ai-session--current)))
```

### Check Agent Config

```elisp
(when (boundp 'ai-session--current)
  (ai-session-agent-config ai-session--current))
```

### Check MCP Server Status

```elisp
(if mcp-server--server
    (format "MCP Server running on port %d for %s"
            mcp-server--port mcp-server--project-root)
  "MCP Server not running")
```

### Check Current Feat Tab

```elisp
(format "Current tab: %s" mp--current-feat-tab)
```

### Check Window References

```elisp
(list 'workarea (window-live-p mp--workarea-window)
      'neotree (window-live-p mp--neotree-window)
      'ai-chat (window-live-p mp--ai-chat-window)
      'tab-bar (window-live-p mp--feat-tab-bar-window))
```

### Check Pending Review

```elisp
(if mp--review-pending
    "Review pending"
  "No pending review")
```

---

## File Editor

### Test File Editor UI

```elisp
(file-editor-test)
```

### Check Current Review Session

```elisp
(when file-editor--current-session
  (list 'file (file-editor-session-file-path file-editor--current-session)
        'lines (cons (file-editor-session-start-line file-editor--current-session)
                     (file-editor-session-end-line file-editor--current-session))))
```

### Clear Current Review Session

```elisp
(progn
  (when file-editor--current-session
    (file-editor--clear-overlays file-editor--current-session))
  (setq file-editor--current-session nil)
  "Cleared review session")
```

---

## Screenshots

### Take Screenshot

```elisp
(mcp-server--tool-screenshot nil)
```

---

## Misc

### Show Message

```elisp
(message "Hello from emacs_eval!")
```

### Get Emacs Version

```elisp
(emacs-version)
```

### List Loaded Features

```elisp
(length features)
```
