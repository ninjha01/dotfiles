---
argument-hint: [instructions]
description: Interview user in-depth to create a detailed spec
allowed-tools: AskUserQuestion, Write
---

Follow the user instructions and interview me in detail using the AskUserQuestionTool about literally anything: technical implementation, UI & UX, concerns, tradeoffs, etc. but make sure the questions are not obvious. be very in-depth and continue interviewing me until it's complete, then write the spec to a file.

CRITICAL: Ask only ONE question at a time using AskUserQuestion, then WAIT for my response before asking the next question. NEVER call AskUserQuestion multiple times in parallel — each question must be a separate turn. Do NOT use the Agent or Explore tools — only use AskUserQuestion and Write.

<instructions>$ARGUMENTS</instructions>
