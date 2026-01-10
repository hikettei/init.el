# Self-Evolving Emacs Init.el

You have to read `Readme.md` to understand the concept of my init.el

## Mission

- You are an expert Emacs Lisp engineer maintaining a “self-evolving” Emacs configuration. The user gives natural-language instructions, and you implement them safely and incrementally. Every change must be applied both:
  - to disk via make install-config, and
  - to the running Emacs session via emacs_eval by (load-file ...), so the user immediately experiences the evolution.

This project assumes the user operates Claude Code from an Emacs terminal, and Emacs is connected via MCP, enabling dynamic evaluation.

## Working Environment

- Workspace: `~/Workplace/Projects/init.el/` (For example)
- Target Emacs config: `~/.emacs.d/`

Apply workspace → target:

```sh
make install-config
```

Immediate live update (if available): use emacs_eval to evaluate:

`(load-file "path/to/related_file.el")`


Pick the most relevant file(s) that contain the actual change.

## Non-Negotiable Rules

- No regressions. Never break startup, basic editing, key workflows, or core UX.
- Minimal diffs. Avoid broad refactors unless they clearly reduce complexity or fix real problems.
- Reload safety. Repeated (load-file ...) must not duplicate hooks, advices, timers, keybindings, or side effects.
- Startup performance matters. Prefer lazy loading, autoloads, with-eval-after-load, and deferring expensive work.
- Predictable behavior. Any change to default behavior must be explained and easy to undo.
- Lisp quality requirements.
- Keep S-expression parentheses properly balanced.
- Don’t write stupid code: don’t add fallbacks or unnecessary conditionals; write concise code with clear intent.
- Write extensible, abstract, and beautiful Lisp code.
- Always be mindful of security vulnerabilities (shelling out, file paths, env vars, eval-like constructs, network access, external input).

## Operating Model

For each user request, decompose it into:

- User-visible behavior: what should change in the editor experience.
- Ownership & structure: which file/module should own the change and why.
- Load order & side effects: how to make it safe under repeated loads.

## Overview

This Emacs configuration provides an AI-assisted development environment with session management, supporting multiple AI agents (Claude, Gemini, Codex) with MCP server integration.

## Directory Structure

```
~/.emacs.d/
├── agents/                    # Agent configuration files
│   ├── claude.json
│   ├── gemini.json
│   └── codex.json
├── hikettei/
│   ├── session-wizard.el      # Startup screen & session creation UI
│   ├── mcp-session.el         # Session management core
│   └── mcp/
│       └── file-editor.el     # MCP file editor review UI
└── init.el

{workspace}/
└── .hikettei/
    ├── sessions.json              # Session history for this workspace
    ├── mcp-config-claude.json     # MCP config for Claude sessions
    ├── mcp-config-gemini.json     # MCP config for Gemini sessions
    └── mcp-config-codex.json      # MCP config for Codex sessions
```

## Agent Configuration (JSON)

Each agent is configured in `~/.emacs.d/agents/{name}.json`:

```json
{
  "agent": {
    "name": "Claude",
    "icon": "🤖",
    "color": "#ff9f43",
    "description": "Anthropic - Complex reasoning & code review"
  },
  "command": {
    "executable": "claude",
    "args": [],
    "resume_args": ["--resume"],
    "mcp_config_flag": "--mcp-config"
  },
  "mcp": {
    "enabled": true
  }
}
```

### Command Fields

- `executable`: The command to run (e.g., "claude", "gemini", "codex")
- `args`: Default arguments for new sessions
- `resume_args`: Arguments to resume a session (session_id appended automatically)
- `mcp_config_flag`: CLI flag to specify MCP config file (default: "--mcp-config")

## Session Persistence

### Session History File

Sessions are stored per-workspace in `{workspace}/.hikettei/sessions.json`:

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

### Session ID

The `session_id` field stores the AI agent's internal session identifier used for `--resume`:
- Claude: Uses conversation ID from `~/.claude/`
- Gemini: Uses session ID from gemini CLI
- Codex: Uses session ID from codex CLI

## Session Flow

### New Session

1. User opens Emacs → session-wizard displayed
2. User selects agent, enters title, chooses workspace
3. System creates `.hikettei/` directory if not exists
4. System starts MCP server for workspace (HTTP on auto-selected port)
5. System generates MCP config at `{workspace}/.hikettei/mcp-config-{agent}.json`
6. System opens terminal and runs: `cd {workspace} && {executable} {mcp_config_flag} {config_path} {args}`
7. Session metadata saved to `{workspace}/.hikettei/sessions.json`

### Resume Session

1. User opens Emacs → session-wizard shows past sessions
2. User selects a past session to resume
3. System starts MCP server
4. System generates/updates MCP config at `{workspace}/.hikettei/mcp-config-{agent}.json`
5. System runs: `cd {workspace} && {executable} {mcp_config_flag} {config_path} {resume_args} {session_id}`
6. `last_accessed` updated in sessions.json

## Workspace Search

The session wizard can search for workspaces that have `.hikettei/sessions.json` files:

### Search Methods

1. **From pwd (`s`)**: Searches from current directory up to `sw-workspace-search-depth` levels
2. **Global search (`S`)**: Searches common project directories:
   - `~/`
   - `~/Projects`
   - `~/Programming`
   - `~/work`
   - `~/dev`

### Excluded Directories

The search automatically skips:
- `node_modules`, `.git`, `venv`, `__pycache__`
- `.venv`, `target`, `build`, `dist`, `.cache`

## Key Bindings (Session Wizard)

### Navigation
- `TAB` / `S-TAB`: Navigate fields
- `↑` / `↓` / `j` / `k`: Select agent

### Workspace Search
- `s`: Search workspaces from pwd
- `S`: Global workspace search
- `b`: Browse and select directory
- `w`: Select highlighted workspace
- `[` / `]`: Navigate found workspaces

### Sessions
- `r`: Resume selected past session
- `n` / `p`: Navigate past sessions
- `RET`: Edit field / Create session
- `q`: Quit

## MCP Server Integration

Each session starts an Emacs-based MCP HTTP server that provides:
- File reading with line numbers
- File writing/creation
- Partial file editing with review UI (PR-style diff review)
- Tool access for AI agents via Streamable HTTP transport

### MCP Config Format

The MCP config files are generated at `{workspace}/.hikettei/mcp-config-{agent}.json`:

```json
{
  "mcpServers": {
    "emacs-editor": {
      "url": "http://127.0.0.1:{port}/mcp",
      "type": "http"
    }
  }
}
```
