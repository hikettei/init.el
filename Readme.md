# hikettei/init.el

A modern Emacs configuration for AI-assisted development with multi-agent support (Claude, Gemini, Codex).

```
    +------------------------------------------------------------------+
    |  [Autopilot] [Terminal] [Explore] [Git]           [N] [A]        |
    +--------+----------------------------------+----------------------+
    |        |                                  |                      |
    |        |                                  |                      |
    |  Neo   |           WorkArea               |      AI Chat         |
    |  Tree  |                                  |      (vterm)         |
    |        |     +--------------------+       |                      |
    |        |     | File Edit Review   |       |   $ claude           |
    |        |     | (MCP Integration)  |       |   > Hello!           |
    |        |     +--------------------+       |                      |
    |        |                                  |                      |
    +--------+----------------------------------+----------------------+
```

## Installing Emacs

### macOS (Recommended: emacs-mac)

```sh
# Using Homebrew with emacs-mac tap (includes native GUI support)
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules --with-native-comp

# Or build from source for latest features
git clone https://github.com/railwaycat/homebrew-emacsmacport.git
cd homebrew-emacsmacport
brew install --build-from-source ./Casks/emacs-mac.rb
```

### Linux

```sh
# Ubuntu/Debian
sudo apt install emacs

# Arch Linux
sudo pacman -S emacs

# Build from source (Emacs 29+)
git clone https://git.savannah.gnu.org/git/emacs.git
cd emacs
./autogen.sh
./configure --with-native-compilation --with-json
make -j$(nproc)
sudo make install
```

### Windows

Download from https://www.gnu.org/software/emacs/download.html or use MSYS2:
```sh
pacman -S mingw-w64-x86_64-emacs
```

## Features

- **Multi-Agent AI Support** - Seamlessly switch between Claude, Gemini, and Codex
- **Session Management** - Create, resume, and manage AI development sessions
- **MCP Integration** - Model Context Protocol server for AI-Emacs communication
- **Multi-Panel Layout** - NeoTree + WorkArea + AI Chat in a fixed layout
- **GitHub PR-style Review** - Review AI-generated file edits with diff highlighting
- **Common Lisp IDE** - Full SLIME integration
- **Dracula Theme** - Nerd-style dark theme throughout

## How to Install

> **Warning**: This will overwrite files in `~/.emacs.d/init.el` and `~/.emacs.d/hikettei`. Back up your existing configuration!

```sh
git clone git@github.com:hikettei/init.el.git
cd ./init.el
make install
```

## Requirements

- Emacs 28.1+
- `uv` (Python package manager for MCP server)
- `vterm` (terminal emulator)
- `neotree` (file tree)

```sh
# Install uv
curl -LsSf https://astral.sh/uv/install.sh | sh

# Install Node.js (for Claude CLI)
# macOS
brew install node

# Install Claude CLI
npm install -g @anthropic-ai/claude-code
```

## Architecture

```
                    +-------------------+
                    |   Session Wizard  |
                    |  (Startup Screen) |
                    +---------+---------+
                              |
              +---------------+---------------+
              |                               |
              v                               v
    +---------+---------+           +---------+---------+
    |   MCP Session     |           |   Multi-Panel     |
    | (mcp-session.el)  |           | (multi-panel.el)  |
    +---------+---------+           +---------+---------+
              |                               |
              v                               v
    +---------+---------+           +---------+---------+
    |    MCP Server     |           |   Panel Modules   |
    |  (hikettei/mcp/)  |           | (hikettei/panel/) |
    +-------------------+           +-------------------+
```

## Components

### Session Wizard (`hikettei/session-wizard.el`)

The startup screen and session creation interface.

```
+----------------------------------------------------------+
|                                                          |
|    hikettei's Emacs                                      |
|    AI-Powered Development Environment                    |
|                                                          |
|  +----------------------------------------------------+  |
|  |  Select AI Agent                                   |  |
|  |                                                    |  |
|  |    [*] Claude    Anthropic - Complex reasoning     |  |
|  |    [ ] Gemini    Google - Fast responses           |  |
|  |    [ ] Codex     OpenAI - Code generation          |  |
|  +----------------------------------------------------+  |
|                                                          |
|  +----------------------------------------------------+  |
|  |  Session Title:  [Feature Development           ]  |  |
|  |  Workspace:      [~/Projects/my-app             ]  |  |
|  +----------------------------------------------------+  |
|                                                          |
|  [s] Search workspaces  [r] Resume session  [RET] Start  |
+----------------------------------------------------------+
```

**Features:**
- Agent selection (Claude, Gemini, Codex)
- Workspace search and selection
- Session history and resume capability
- Beautiful TUI with Dracula colors

**Key Bindings:**
| Key | Action |
|-----|--------|
| `j/k` or `Up/Down` | Navigate agents |
| `TAB` | Next field |
| `s` | Search workspaces from current directory |
| `S` | Global workspace search |
| `r` | Resume selected past session |
| `RET` | Create/start session |
| `q` | Quit |

### MCP Module (`hikettei/mcp/`)

Model Context Protocol integration for AI-Emacs communication.

```
+-------------+          +-------------+          +-------------+
|   Claude    |  <--->   | MCP Server  |  <--->   |    Emacs    |
|    CLI      |   JSON   |  (Python)   |   elisp  |   Buffers   |
+-------------+          +-------------+          +-------------+
                               |
                               v
                    +-------------------+
                    |  file-editor.el   |
                    | (PR Review Style) |
                    +-------------------+
```

**`hikettei/mcp/file-editor.el`** - GitHub PR-style file edit review interface:
- Full file display with diff highlighting
- Line-by-line commenting
- Approve / Request Changes workflow
- Integration with AI edit suggestions

### MCP Tools

The MCP server (`hikettei/mcp/mcp-server.el`) provides tools that AI agents can use to interact with Emacs.

#### File Editor Tools

| Tool | Description |
|------|-------------|
| `emacs_read_file` | Read file with line numbers (use instead of default Read) |
| `emacs_write_file` | Create or overwrite files (use instead of default Write) |
| `emacs_edit_file` | Partial file edit with PR-style diff review |
| `emacs_eval` | Evaluate arbitrary Emacs Lisp code |
| `emacs_screenshot` | Capture Emacs frame as PNG |
| `emacs_get_pending_review` | Get current pending edit review data |

#### Memory Tools

Persistent memory system for AI agents. Data stored in `{workspace}/.hikettei/memory/`.

| Tool | Description |
|------|-------------|
| `memory_note` | Create/update markdown notes with tags |
| `memory_store` | Store external content (webpage, github, arxiv, pdf) |
| `memory_search` | Search memories by keyword, type, or tags |
| `memory_get` | Retrieve full content by ID |
| `memory_list` | List all memories with pagination |
| `memory_delete` | Delete memory by ID |

**Memory Types:**
- `note` - Markdown memos with YAML front matter
- `webpage` - Web content converted to markdown
- `github` - Repository metadata + README (optional: clone repo)
- `arxiv` - Paper metadata + PDF download
- `pdf` - Local PDF files

**Example Usage:**
```
memory_store(type="github", url="https://github.com/user/repo", clone=true)
→ Clones repo to .hikettei/memory/repos/user_repo/
→ AI can then read files from the cloned repository
```

#### Browser Tools

Control the xwidget-webkit browser in the Explore panel.

| Tool | Description |
|------|-------------|
| `browser_open` | Open browser (switch to Explore panel), optionally navigate to URL |
| `browser_navigate` | Navigate to URL |
| `browser_back` | Go back in history |
| `browser_forward` | Go forward in history |
| `browser_reload` | Reload current page |
| `browser_get_state` | Get current URL and page title |
| `browser_get_content` | Get page content as text or HTML |
| `browser_get_links` | Get all links on page as JSON array |
| `browser_click` | Click element by CSS selector |
| `browser_type` | Type text into input element |
| `browser_scroll` | Scroll page up or down |
| `browser_execute_js` | Execute arbitrary JavaScript |
| `browser_screenshot` | Take screenshot of Emacs frame |
| `browser_wait` | Wait for element to appear (with timeout) |

**Example: Google Search**
```
browser_open(url="https://www.google.com")
browser_type(selector="textarea[name='q']", text="Emacs MCP")
browser_click(selector="input[type='submit']")
browser_wait(selector="#search")
browser_get_content(selector="#search", format="text")
```

### Review Modes

The Autopilot panel supports 4 review modes for AI file edits. Switch modes with `C-x j m`.

```
+------------------------------------------------------------------+
|  🔴 Autopilot [a] [M]  [Terminal] [Explore]           [N] [A]    |
+------------------------------------------------------------------+
                    ^
                    └── Review Mode Indicator: [M] [H] [A] [N]
```

| Mode | Indicator | Description |
|------|-----------|-------------|
| **Manual** | `[M]` | Human reviews all edits (default) |
| **Hybrid** | `[H]` | AI pre-reviews, adds comments → Human confirms |
| **AutoReview** | `[A]` | AI automatically approves/rejects |
| **NoReview** | `[N]` | Auto-approve all edits immediately |

**Key Bindings:**
| Key | Action |
|-----|--------|
| `C-x j m` | Cycle through review modes |
| `C-c C-c` | Approve edit (during review) |
| `C-c C-k` | Reject edit (during review) |
| `c` | Add line comment (during review) |
| `M-n` / `M-p` | Navigate between diff hunks |

**Mode Details:**

- **Manual**: Every AI edit opens the review UI. You see the diff with:
  - Red background for removed lines
  - Green phantom text for added lines
  - Header showing AI's comment

- **Hybrid**: A separate AI process reviews the edit first:
  - Shows `[AI recommends: APPROVE/REJECT]` in header
  - AI may add line comments
  - You make the final decision

- **AutoReview**: Background AI decides automatically:
  - Spawns `claude --print` to review
  - Returns detailed reason for decision
  - No human interaction required

- **NoReview**: Instant auto-approval:
  - Edits applied immediately
  - Useful for trusted refactoring tasks
  - ⚠️ Use with caution!

**Autopilot Buffer Display:**
```
  Autopilot Mode

  Waiting for AI file access...

  Files read or edited by AI will appear here.

  ─────────────────────────────────────

  Review Mode: Manual

  C-x j m - Cycle review mode
```

### Panel System (`hikettei/panel/`)

Modular workspace panels that can be switched via the Feat Tab Bar.

```
+------------------------------------------------------------------+
|  [Autopilot] [Terminal] [Explore] [Git] [Monitor]     [N] [A]    |
+------------------------------------------------------------------+
        |           |          |        |        |
        v           v          v        v        v
   +----------+ +--------+ +-------+ +-----+ +--------+
   | Review   | | vterm  | | EAF   | | Git | | System |
   | AI Edits | | Shell  | | Browse| | Ops | | Stats  |
   +----------+ +--------+ +-------+ +-----+ +--------+
```

**Available Panels:**

| Panel | Key | Description |
|-------|-----|-------------|
| Autopilot | `C-x j a` | AI file edit review mode |
| Terminal | `C-x j w` | General purpose vterm |
| Explore | `C-x j r` | Web browser (EAF/xwidget) |
| Git | `C-x j g` | Git operations |
| Monitor | `C-x j m` | System monitoring |
| Freeform | `C-x j f` | Scratchpad |

**Creating Custom Panels:**

```elisp
;; In hikettei/panel/my-panel.el
(require 'multi-panel)

(defun mp--setup-my-panel (session)
  "Setup my custom panel."
  (when (and mp--workarea-window (window-live-p mp--workarea-window))
    (select-window mp--workarea-window)
    ;; Your panel setup code here
    (switch-to-buffer "*My Panel*")))

(mp-define-feat-tab my-panel
  :name "My Panel"
  :key "p"
  :icon ""
  :setup #'mp--setup-my-panel)

(provide 'panel-my-panel)
```

### `.hikettei/` Directory

Per-workspace session storage (hidden directory).

```
~/Projects/my-app/
├── src/
├── ...
└── .hikettei/
    └── sessions.json    # Session history for this workspace
```

**`sessions.json` Structure:**
```json
{
  "sessions": [
    {
      "id": "session-20250109-223045",
      "agent": "Claude",
      "title": "Feature Development",
      "created_at": "2025-01-09T22:30:45",
      "last_accessed": "2025-01-09T23:15:00",
      "session_id": "abc123def456"
    }
  ]
}
```

This enables:
- Session persistence across Emacs restarts
- Resume previous conversations with AI agents
- Per-project session history

### Agent Configuration (`~/.emacs.d/agents/`)

AI agent configurations stored as JSON files.

```
~/.emacs.d/agents/
├── claude.json
├── gemini.json
└── codex.json
```

**Example `claude.json`:**
```json
{
  "agent": {
    "name": "Claude",
    "icon": "",
    "color": "#ff9f43",
    "description": "Anthropic - Complex reasoning & code review"
  },
  "command": {
    "executable": "claude",
    "args": [],
    "resume_args": ["--resume"]
  },
  "mcp": {
    "enabled": true
  }
}
```

## Key Bindings

### Global (C-x j prefix)
| Key | Action |
|-----|--------|
| `C-x j n` | Toggle NeoTree |
| `C-x j c` | Toggle AI Chat |
| `C-x j a` | Switch to Autopilot panel |
| `C-x j w` | Switch to Terminal panel |
| `C-x j r` | Switch to Explore panel |

### Session Wizard
| Key | Action |
|-----|--------|
| `M-x sw-show` | Open session wizard |

## File Structure

```
~/.emacs.d/
├── init.el                 # Main entry point
├── agents/                 # Agent JSON configurations
│   ├── claude.json
│   ├── gemini.json
│   └── codex.json
└── hikettei/
    ├── 0package-manager.el # Package management (straight.el)
    ├── 1multi-term.el      # Terminal configuration
    ├── 2theme.el           # Dracula theme setup
    ├── 3autocomp.el        # Auto-completion (company, etc.)
    ├── 4common-lisp.el     # SLIME configuration
    ├── 5python.el          # Python development
    ├── 6markdown.el        # Markdown support
    ├── 7eaf.el             # Emacs Application Framework
    ├── 8claude-code.el     # Claude Code integration
    ├── session-wizard.el   # Startup wizard
    ├── mcp-session.el      # Session management
    ├── multi-panel.el      # Panel layout system
    ├── keybindings.el      # Custom keybindings
    ├── style.el            # UI styling
    ├── mcp/
    │   ├── mcp-server.el   # MCP HTTP server & tool dispatcher
    │   ├── file-editor.el  # PR-style review UI
    │   ├── memory.el       # Persistent memory system
    │   └── browser.el      # WebKit browser control
    └── panel/
        ├── autopilot.el    # AI autopilot mode
        ├── terminal.el     # Terminal panel
        ├── explore.el      # Browser panel
        ├── git.el          # Git panel
        ├── monitor.el      # System monitor
        └── ...
```

## Author

hikettei
