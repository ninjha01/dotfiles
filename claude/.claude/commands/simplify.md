---
description: Run elisp-code-simplifier on frontend and backend in parallel
allowed-tools: Task(*)
---

Run the code-simplifier subagent on the emacs codebase:
1. Launch a Task with `subagent_type: "elisp-code-simplifier"` for `.emacs.d/

The elisp-code-simplifier agent simplifies and refines code for clarity, consistency, and maintainability while preserving all functionality. It focuses on recently modified code unless instructed otherwise.

Report the results from the simplification processes.
