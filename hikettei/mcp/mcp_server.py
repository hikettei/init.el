#!/usr/bin/env python3
"""
MCP Server for Emacs File Editor Integration

Provides MCP tools that integrate with Emacs for reviewing file edits
in a GitHub PR Review style interface.

Usage:
    emacs-mcp-server /path/to/project
    emacs-mcp-server --root /path/to/project

All file operations are restricted to the specified project root.
Multiple instances can run in parallel for different projects.
"""

from __future__ import annotations

import argparse
import asyncio
import hashlib
import json
import os
import shutil
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import Tool, TextContent


# ============================================================
# Server Configuration
# ============================================================

@dataclass
class ServerConfig:
    """Server configuration, set at startup."""
    project_root: Path
    result_file: Path
    review_timeout: int = 300  # 5 minutes

    @classmethod
    def create(cls, root: Path) -> ServerConfig:
        """Create config with unique result file based on project root."""
        root_hash = hashlib.md5(str(root).encode()).hexdigest()[:8]
        result_file = Path(f"/tmp/emacs-file-editor-{root_hash}.json")
        return cls(project_root=root, result_file=result_file)


# Global config (set in main)
CONFIG: ServerConfig | None = None


# ============================================================
# Path Security
# ============================================================

class PathSecurityError(Exception):
    """Raised when path access is denied."""
    pass


def resolve_path(file_path: str) -> Path:
    """
    Resolve file path relative to project root.

    - Relative paths resolve from project root
    - Absolute paths must be within project root
    - Path traversal outside root is blocked

    Raises PathSecurityError if access denied.
    """
    if CONFIG is None:
        raise PathSecurityError("Server not configured")

    root = CONFIG.project_root
    path = Path(file_path)

    # Resolve relative paths from project root
    if not path.is_absolute():
        path = root / path

    # Normalize and resolve symlinks
    try:
        resolved = path.resolve()
    except Exception as e:
        raise PathSecurityError(f"Cannot resolve path: {e}")

    # Verify path is within project root
    try:
        resolved.relative_to(root.resolve())
    except ValueError:
        raise PathSecurityError(
            f"Access denied: '{file_path}' is outside project root"
        )

    return resolved


# ============================================================
# Patch Management
# ============================================================

@dataclass
class Patch:
    """Staged file edit with optimistic locking."""
    file_path: Path
    original_hash: str
    start_line: int = 0
    end_line: int = 0
    content: str = ""
    is_full_overwrite: bool = False

    @classmethod
    async def create(cls, file_path: Path) -> Patch:
        """Create patch and calculate file hash."""
        hash_val = await cls._hash_file(file_path)
        return cls(file_path=file_path, original_hash=hash_val)

    @staticmethod
    async def _hash_file(path: Path) -> str:
        """Calculate SHA256 hash of file."""
        if not path.exists():
            return "new_file"
        try:
            return hashlib.sha256(path.read_bytes()).hexdigest()
        except Exception:
            return "new_file"

    async def read_view(self, offset: int = 0, limit: int = 3000) -> str:
        """Read file content with line numbers."""
        try:
            lines = self.file_path.read_text(encoding='utf-8').splitlines(keepends=True)
        except FileNotFoundError:
            return f"Error: File not found: {self.file_path}"
        except Exception as e:
            return f"Error reading file: {e}"

        total = len(lines)
        end = min(offset + limit, total)

        output = [f"File: {self.file_path} (Lines: {total})", "-" * 50]
        for i in range(offset, end):
            output.append(f"{i + 1:4} | {lines[i].rstrip()}")

        if end < total:
            output.append(f"... ({total - end} more lines, use offset={end})")

        return "\n".join(output)

    def stage_overwrite(self, content: str) -> None:
        """Stage full file overwrite."""
        self.content = content
        self.is_full_overwrite = True

    async def stage_edit(self, start: int, end: int, content: str) -> tuple[str, str]:
        """Stage partial edit. Returns (preview, original_section)."""
        self.start_line = start
        self.end_line = end
        self.content = content
        self.is_full_overwrite = False

        try:
            lines = self.file_path.read_text(encoding='utf-8').splitlines()
        except Exception as e:
            return f"Error: {e}", ""

        # Extract original section (1-indexed)
        start_idx = max(0, start - 1)
        end_idx = min(len(lines), end)
        original = lines[start_idx:end_idx]
        new_lines = content.splitlines()

        # Build preview
        ctx = 3
        ctx_start = max(0, start - 1 - ctx)
        ctx_end = min(len(lines), end + ctx)

        preview = [f"File: {self.file_path} (Edit Preview)", "-" * 50]

        # Context before
        for i in range(ctx_start, start_idx):
            preview.append(f"{i + 1:4}   | {lines[i]}")

        # Removed lines
        for i, line in enumerate(original):
            preview.append(f"{start + i:4} - | {line}")

        # Added lines
        for line in new_lines:
            preview.append(f"     + | {line}")

        # Context after
        for i in range(end_idx, ctx_end):
            preview.append(f"{i + 1:4}   | {lines[i]}")

        return "\n".join(preview), "\n".join(original)

    async def apply(self) -> None:
        """Apply staged edit to file."""
        # Verify file hasn't changed (optimistic lock)
        current_hash = await self._hash_file(self.file_path)
        if current_hash != self.original_hash and self.original_hash != "new_file":
            raise RuntimeError("Conflict: file changed since edit was staged")

        if self.is_full_overwrite:
            self.file_path.parent.mkdir(parents=True, exist_ok=True)
            self.file_path.write_text(self.content, encoding='utf-8')
        else:
            lines = self.file_path.read_text(encoding='utf-8').splitlines(keepends=True)
            start_idx = max(0, self.start_line - 1)
            end_idx = self.end_line

            new_lines = [l + '\n' for l in self.content.splitlines()]
            lines[start_idx:end_idx] = new_lines

            self.file_path.write_text(''.join(lines), encoding='utf-8')


# Patch registry
_patches: dict[str, Patch] = {}
_patch_counter = 0


def register_patch(patch: Patch) -> str:
    """Register patch and return ID."""
    global _patch_counter
    _patch_counter += 1
    patch_id = f"p{_patch_counter:04d}"
    _patches[patch_id] = patch
    return patch_id


def get_patch(patch_id: str) -> Patch | None:
    return _patches.get(patch_id)


def remove_patch(patch_id: str) -> None:
    _patches.pop(patch_id, None)


def clear_patches() -> None:
    _patches.clear()


# ============================================================
# Emacs Integration
# ============================================================

async def emacs_eval(elisp: str) -> str | None:
    """Execute elisp via emacsclient."""
    try:
        proc = await asyncio.create_subprocess_exec(
            "emacsclient", "--eval", elisp,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )
        stdout, _ = await proc.communicate()
        return stdout.decode().strip() if proc.returncode == 0 else None
    except Exception:
        return None


async def open_review_ui(
    file_path: Path,
    start: int,
    end: int,
    original: str,
    new_content: str,
    comment: str = ""
) -> dict[str, Any] | None:
    """Open Emacs review UI and wait for result."""
    if CONFIG is None:
        return None

    request = {
        "file_path": str(file_path),
        "start_line": start,
        "end_line": end,
        "original_content": original,
        "new_content": new_content,
        "comment": comment
    }

    # Escape for elisp string
    json_str = json.dumps(request).replace('\\', '\\\\').replace('"', '\\"')
    elisp = f'(file-editor-open-from-json "{json_str}")'

    # Clear old result
    if CONFIG.result_file.exists():
        CONFIG.result_file.unlink()

    # Call Emacs
    if await emacs_eval(elisp) is None:
        return None

    # Wait for result
    return await wait_for_review_result()


async def wait_for_review_result() -> dict[str, Any] | None:
    """Wait for review result file."""
    if CONFIG is None:
        return None

    timeout = CONFIG.review_timeout
    start = asyncio.get_event_loop().time()

    while asyncio.get_event_loop().time() - start < timeout:
        if CONFIG.result_file.exists():
            try:
                result = json.loads(CONFIG.result_file.read_text())
                CONFIG.result_file.unlink()
                return result
            except Exception:
                pass
        await asyncio.sleep(0.5)

    return None


# ============================================================
# MCP Server
# ============================================================

server = Server("emacs-file-editor")


@server.list_tools()
async def list_tools() -> list[Tool]:
    return [
        Tool(
            name="read_file",
            description="Read file with line numbers. Use offset for pagination.",
            inputSchema={
                "type": "object",
                "properties": {
                    "file_path": {"type": "string", "description": "File path (relative to project root)"},
                    "offset": {"type": "integer", "description": "Start line (0-indexed)", "default": 0},
                    "limit": {"type": "integer", "description": "Max lines", "default": 3000}
                },
                "required": ["file_path"]
            }
        ),
        Tool(
            name="write_file",
            description="Create new file. Use supersede=true to overwrite existing.",
            inputSchema={
                "type": "object",
                "properties": {
                    "file_path": {"type": "string", "description": "File path"},
                    "content": {"type": "string", "description": "File content"},
                    "supersede": {"type": "boolean", "description": "Overwrite if exists", "default": False}
                },
                "required": ["file_path", "content"]
            }
        ),
        Tool(
            name="request_edit",
            description="Request partial file edit. Opens review UI in Emacs for approval.",
            inputSchema={
                "type": "object",
                "properties": {
                    "file_path": {"type": "string", "description": "File path"},
                    "start_line": {"type": "integer", "description": "Start line (1-indexed, inclusive)"},
                    "end_line": {"type": "integer", "description": "End line (1-indexed, inclusive)"},
                    "content": {"type": "string", "description": "New content"},
                    "comment": {"type": "string", "description": "Explanation for reviewer", "default": ""}
                },
                "required": ["file_path", "start_line", "end_line", "content"]
            }
        ),
        Tool(
            name="approve_edit",
            description="Apply staged edit by patch_id.",
            inputSchema={
                "type": "object",
                "properties": {
                    "patch_id": {"type": "string", "description": "Patch ID from request_edit"}
                },
                "required": ["patch_id"]
            }
        ),
        Tool(
            name="reject_edit",
            description="Discard staged edit.",
            inputSchema={
                "type": "object",
                "properties": {
                    "patch_id": {"type": "string", "description": "Patch ID to reject"}
                },
                "required": ["patch_id"]
            }
        ),
        Tool(
            name="delete_file",
            description="Delete file or directory.",
            inputSchema={
                "type": "object",
                "properties": {
                    "file_path": {"type": "string", "description": "Path to delete"},
                    "recursive": {"type": "boolean", "description": "Recursive delete", "default": False}
                },
                "required": ["file_path"]
            }
        )
    ]


@server.call_tool()
async def call_tool(name: str, arguments: dict[str, Any]) -> list[TextContent]:
    """Handle tool calls."""

    def text(msg: str) -> list[TextContent]:
        return [TextContent(type="text", text=msg)]

    def error(msg: str) -> list[TextContent]:
        return text(f"Error: {msg}")

    # Path validation helper
    def safe_path(path_arg: str) -> Path | None:
        try:
            return resolve_path(path_arg)
        except PathSecurityError as e:
            return None

    if name == "read_file":
        path = safe_path(arguments["file_path"])
        if path is None:
            return error(f"Access denied: {arguments['file_path']}")

        patch = await Patch.create(path)
        result = await patch.read_view(
            arguments.get("offset", 0),
            arguments.get("limit", 3000)
        )
        return text(result)

    elif name == "write_file":
        path = safe_path(arguments["file_path"])
        if path is None:
            return error(f"Access denied: {arguments['file_path']}")

        patch = await Patch.create(path)
        if not arguments.get("supersede", False) and patch.original_hash != "new_file":
            return error(f"File exists. Use supersede=true or request_edit.")

        try:
            patch.stage_overwrite(arguments["content"])
            await patch.apply()
            return text(f"Wrote: {arguments['file_path']}")
        except Exception as e:
            return error(str(e))

    elif name == "request_edit":
        path = safe_path(arguments["file_path"])
        if path is None:
            return error(f"Access denied: {arguments['file_path']}")

        patch = await Patch.create(path)
        if patch.original_hash == "new_file":
            return error("File does not exist. Use write_file.")

        try:
            preview, original = await patch.stage_edit(
                arguments["start_line"],
                arguments["end_line"],
                arguments["content"]
            )
            patch_id = register_patch(patch)

            # Try Emacs review UI
            result = await open_review_ui(
                path,
                arguments["start_line"],
                arguments["end_line"],
                original,
                arguments["content"],
                arguments.get("comment", "")
            )

            if result is None:
                # Emacs not available
                return text(f"""Staged edit. Patch ID: {patch_id}

{preview}

Emacs unavailable. Use approve_edit('{patch_id}') or reject_edit('{patch_id}')""")

            # Process review result
            decision = result.get("decision", "request-changes")
            summary = result.get("summary", "")
            comments = result.get("line_comments", [])

            feedback = [f"Review: {decision.upper()}"]
            if summary:
                feedback.append(f"Summary: {summary}")
            for c in comments:
                feedback.append(f"  Line {c.get('line', '?')}: {c.get('comment', '')}")

            if decision == "approve":
                await patch.apply()
                clear_patches()
                return text(f"APPROVED and applied.\n\n" + "\n".join(feedback))
            else:
                return text(f"REJECTED.\n\n" + "\n".join(feedback) +
                           f"\n\nPatch {patch_id} available for retry.")

        except Exception as e:
            return error(str(e))

    elif name == "approve_edit":
        patch = get_patch(arguments["patch_id"])
        if not patch:
            return error(f"Invalid patch_id: {arguments['patch_id']}")

        try:
            await patch.apply()
            clear_patches()
            return text("Edit applied.")
        except Exception as e:
            return error(str(e))

    elif name == "reject_edit":
        remove_patch(arguments["patch_id"])
        return text("Edit rejected.")

    elif name == "delete_file":
        path = safe_path(arguments["file_path"])
        if path is None:
            return error(f"Access denied: {arguments['file_path']}")

        try:
            if arguments.get("recursive", False):
                shutil.rmtree(path)
            else:
                path.unlink()
            return text(f"Deleted: {arguments['file_path']}")
        except Exception as e:
            return error(str(e))

    return error(f"Unknown tool: {name}")


# ============================================================
# Entry Point
# ============================================================

async def _run_server():
    async with stdio_server() as (read_stream, write_stream):
        await server.run(read_stream, write_stream, server.create_initialization_options())


def main():
    global CONFIG

    parser = argparse.ArgumentParser(
        description="MCP Server for Emacs File Editor",
        epilog="Multiple instances can run in parallel for different projects."
    )
    parser.add_argument("root", nargs="?", help="Project root directory")
    parser.add_argument("--root", "-r", dest="root_flag", help="Project root (flag)")

    args = parser.parse_args()
    root_path = args.root_flag or args.root

    if not root_path:
        parser.error("Project root required: emacs-mcp-server /path/to/project")

    root = Path(root_path).resolve()
    if not root.is_dir():
        print(f"Error: Not a directory: {root}", file=sys.stderr)
        sys.exit(1)

    CONFIG = ServerConfig.create(root)
    print(f"MCP Server: {root} (result: {CONFIG.result_file})", file=sys.stderr)

    asyncio.run(_run_server())


if __name__ == "__main__":
    main()
