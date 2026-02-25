# claude-code.el — Emacs Interface to Claude Code

## Overview

An Emacs major mode that wraps the Claude Code CLI (`claude`) as a comint subprocess, providing a magit-inspired chat interface with collapsible tool call sections, syntax-highlighted diffs, transient menus, and session management.

Personal-use package living in the dotfiles repo at `.emacs.d/elisp/claude-code.el`.

## Architecture

### Subprocess Model

- Spawn `claude` CLI as a comint subprocess using `make-comint-in-buffer`
- Pass `--output-format json` to get structured output for reliable parsing
- Assumes `claude` binary is available on `$PATH` — no install checks or custom path configuration
- Use CLI default model — no model selection exposed in the interface

### Output Parsing

- Parse Claude Code's JSON structured output for all tool calls, responses, and status events
- Extract tool type, target file/path, line numbers, status (success/failure), and content
- Use JSON parsing to populate magit-style sections and permission prompts

## Buffer & Mode

### Naming

- Package prefix: `claude-code-`
- Major mode: `claude-code-chat-mode` (derived from `comint-mode`)
- Buffer naming: `*claude-code*`, `*claude-code<2>*`, etc. for multiple sessions
- Each session gets its own buffer with an independent subprocess

### Input

- Comint-style: prompt marker at the bottom of the buffer, user types there and hits `RET` to send
- Standard comint input ring (history) available via `M-p` / `M-n`

### Scrolling

- Smart scroll: auto-scroll to follow output only if point is already at the bottom of the buffer
- If the user has scrolled up to read earlier content, stay put while new output streams in

### Modeline

- Display session status in the chat buffer's modeline: `[Claude:idle]`, `[Claude:streaming]`, `[Claude:error]`
- Only shown in `claude-code-chat-mode` buffers, not globally

## Tool Call Display

### Collapsed Sections (Default)

Tool calls are rendered as magit-style section headers — full-width highlighted lines that are collapsed by default:

```
Edit  src/foo.el  lines 42-58  ✓
Bash  npm test                 ✗ exit:1
Read  src/bar.el               ✓
Grep  "TODO" in src/           ✓
```

- Press `TAB` on a section header to expand/collapse (like magit)
- Sections use distinct faces for the tool type, target, and status indicator

### Expanded Content

When expanded:

- **Edit/Write tool calls**: Show syntax-highlighted diffs using `diff-mode` faces (green additions, red deletions, `+`/`-` line prefixes). Parse the edit tool's old/new content from JSON to generate the diff
- **Bash tool calls**: Show the command and its output as-is
- **Read/Grep/Glob tool calls**: Show output as-is
- **Other tools**: Raw JSON output as fallback

### File Navigation

- File paths in tool call headers are clickable (buttons or text properties)
- `RET` on a file path opens the file in the other window via `find-file-other-window`
- Jump to the specific line number referenced in the tool call (e.g., the start of an edited region)

## Permission Handling

When Claude Code requests permission for a tool call (file write, bash command, etc.):

### Transient Popup

- Display a transient popup (using the `transient` package) with:
  - **Summary line**: One-line description, e.g., `Bash: npm install lodash` or `Edit: src/foo.el:42`
  - **Expand action**: A key (e.g., `d` for details) that expands to show full tool call content (the complete command, or the full edit diff)
  - **Approve**: `y` or `a` to approve
  - **Deny**: `n` or `d` to deny
- The transient blocks streaming output until the user responds
- Subprocess stdin is used to send the approval/denial back to Claude Code

## Session Management

### Multiple Sessions

- Support arbitrary number of concurrent sessions, each with its own buffer and subprocess
- Sessions are independent — different working directories, different conversations

### Resumable Sessions

- Store session IDs (from Claude Code) associated with each buffer
- When creating a new session, offer the option to resume a previous one via `--resume <session-id>`
- Provide a `claude-code-resume-session` command that lists previous session IDs (from Claude Code's own session storage) and lets the user pick one via `completing-read`

### Transient Menu

A single entry keybinding (e.g., `C-c C-l`) opens a transient menu with session management actions:

- **n**: New session (prompts for working directory, defaults to `project-root`)
- **k**: Kill current session (kill subprocess and buffer)
- **l**: List all active sessions (switch to one via `completing-read`)
- **r**: Resume a previous session
- **s**: Switch to another active session buffer

## Keybindings

### In `claude-code-chat-mode`

| Key       | Action                                           |
|-----------|--------------------------------------------------|
| `RET`     | Send input (comint-send-input)                   |
| `C-g`     | Interrupt Claude when streaming (SIGINT); normal `keyboard-quit` when idle |
| `TAB`     | Toggle expand/collapse on tool call section       |
| `C-c C-l` | Open session management transient                |
| `M-p/M-n` | Input history (comint)                           |

### C-g Behavior

- When Claude is streaming (subprocess is active/busy): `C-g` sends `SIGINT` to the subprocess to cancel the current response
- When idle: `C-g` falls back to standard `keyboard-quit`
- Implemented by remapping `keyboard-quit` in the mode's keymap to a smart dispatch function that checks subprocess state

## Integrations

### project.el

- `claude-code-new-session` defaults the working directory to `(project-root (project-current))` when available
- Falls back to `default-directory` if no project is detected

### magit

- Add a keybinding in `magit-status-mode-map` (e.g., `C` or `A c`) to launch a new Claude Code session scoped to the magit repo's root directory
- No automatic git context injection — just uses the directory

## File Sync

- When Claude Code edits files on disk, automatically revert any open Emacs buffers visiting those files
- Use a process filter or post-tool-call hook that detects Edit/Write tool completions from the JSON output
- Call `revert-buffer` (with `noconfirm`) on affected buffers
- Silently skip buffers with unsaved changes (don't clobber user edits) and warn in the echo area

## Error Handling

- When the Claude Code subprocess dies unexpectedly (exit, crash, signal):
  - Display an error message in the chat buffer: `[Claude Code process exited with status N]`
  - Update modeline to `[Claude:error]`
  - Do **not** auto-restart — user manually restarts with a command (`claude-code-restart` or new session from transient)

## Rendering

- Minimal markdown rendering — no full markdown-mode parsing
- Code blocks: detect fenced code blocks (``` ... ```) and apply basic font-lock, but no per-language syntax highlighting
- File paths: rendered as clickable buttons that open-other-window + jump to line
- Everything else: plain text as received from the CLI (after JSON extraction of the assistant message content)

## Dependencies

- `transient` (for menus and permission popups) — bundled with Emacs 29+ via magit or standalone
- `magit` (optional, for magit-status integration)
- `project` (built-in, for project root detection)
- No other external dependencies

## File Location

- Single file: `~/.emacs.d/elisp/claude-code.el`
- Loaded via `init.el` with `(require 'claude-code)` (the `elisp/` directory should already be on `load-path`, or add it)

## Commands Summary

| Command                        | Description                              |
|--------------------------------|------------------------------------------|
| `claude-code-new-session`      | Start a new Claude Code session          |
| `claude-code-kill-session`     | Kill the current session's process/buffer|
| `claude-code-list-sessions`    | List and switch between active sessions  |
| `claude-code-resume-session`   | Resume a previous session by ID          |
| `claude-code-restart`          | Restart a dead session's subprocess      |
| `claude-code-transient`        | Open the session management transient    |

## Design Principle: Loose Coupling to Claude Code

The implementation should avoid tight coupling to Claude Code specifically. While Claude Code is the initial backend, the architecture should make it relatively straightforward to swap in a different CLI tool or model provider later. This doesn't mean building a full abstraction layer — just keeping CLI-specific details (binary name, flags, JSON output parsing) isolated rather than scattered throughout the codebase. Concretely:

- CLI binary name and flags defined in a small number of variables/functions, not hardcoded everywhere
- JSON response parsing isolated to a few functions that produce a generic internal representation
- Buffer rendering and session management work with the internal representation, not raw CLI output

## Non-Goals

- No model selection UI (use CLI default)
- No automatic context injection (no sending current buffer, git status, etc.)
- No global modeline indicator (chat buffer only)
- No MELPA packaging or broad Emacs version compatibility
- No install/prerequisite checks for the `claude` binary
- No auto-restart on subprocess death
