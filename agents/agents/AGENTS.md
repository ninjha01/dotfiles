# Global Agent Instructions

This file is the shared, tool-agnostic source of truth for agent behavior in this dotfiles repo.

Tool-specific entrypoints like `CLAUDE.md` should stay thin and defer here when possible.

## Python

- Always use `uv run` to execute Python. Never use `source .venv/bin/activate` or direct `python` commands.
  - Example: `uv run python -c "..."`
  - Example: `uv run invoke lint`
