---
description: Summarize the session into a .md file for future reference
allowed-tools: Write, Bash, Read
---

Summarize this entire session into a structured markdown file for future reference.

## Steps

1. **Create the output directory** if it doesn't exist:
   ```
   mkdir -p .claude/sessions/
   ```

2. **Generate the filename**: `YYYY-MM-DD-<slug>.md` where:
   - Date is today's date
   - Slug is a 2-4 word kebab-case summary derived from what happened in this session
   - If a file with that name already exists in `.claude/sessions/`, pick a more specific slug
   - Do NOT ask the user to confirm the filename — just pick a good one

3. **Write the file** to `.claude/sessions/<filename>` with the structure below. Write it in a single shot using the Write tool.

## Output Format

```markdown
# Session: <descriptive title>
*<date>*

## Summary
<Direct, technical narrative. Scale length to session complexity:
short for simple sessions, longer for involved ones.
Cover what was done, why, and critical outcomes.>

## Decisions
- <Decision 1>: <rationale>
- <Decision 2>: <rationale>

Include both:
- Explicit decisions discussed with the user
- Implicit choices you made without discussion — flag these with "(implicit)"

## Follow-ups
- [ ] <Open item 1>
- [ ] <Open item 2>

---
<!-- Agent Context Below -->

## Context for Resume
<What a future agent needs to know to continue this work.
Use a hybrid format: prose narrative with embedded code blocks,
file paths, and specific references where they add precision.>

## <Optional sections as needed>
Add additional sections organically if the session warrants them.
Examples: "## Architecture Notes", "## Constraints Discovered",
"## Patterns Established", "## Key Files Modified" — whatever fits.
```

## Rules

- Tone: direct and technical, not conversational
- Always produce the full format with all required sections, even for trivial sessions
- Required sections: Summary, Decisions, Follow-ups, Context for Resume
- Surface implicit decisions (choices you made without discussing with the user) and flag them with "(implicit)"
- Do NOT run any git commands or capture git state — work purely from conversation context
- Do NOT update MEMORY.md — the session file is standalone
- Do NOT modify .gitignore or any other files — only create the session summary file
- After writing, tell the user the file path
