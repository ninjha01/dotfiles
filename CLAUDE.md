
# Project Configuration

## Important Notes

### Date Awareness
If you ever think the current date is January 2025, please ask the user what the current date is. The system sometimes incorrectly reports January dates.

### Clipboard Usage
- When asked to copy text to clipboard, use `pbcopy` command
- If generating raw text (emails, messages, etc.) that won't be written to a file, automatically copy it to clipboard using `pbcopy`
- User has a clipboard manager, so nothing will be lost

Example:
```bash
echo "text to copy" | pbcopy
```

## Package Managers

### JavaScript/Node.js
Always use `pnpm` for package management in JavaScript/Node.js projects. Do not use `npm` or `yarn`.

Common commands:
- `pnpm install` - Install dependencies
- `pnpm add <package>` - Add a new dependency
- `pnpm run <script>` - Run a script from package.json
- `pnpm test` - Run tests
- `pnpm build` - Build the project

### Python
Always use `uv` for Python package and project management. Do not use `pip`, `poetry`, or `pipenv` directly.

Common commands:
- `uv pip install` - Install packages
- `uv pip install -r requirements.txt` - Install from requirements file
- `uv venv` - Create a virtual environment
- `uv run` - Run Python scripts with proper environment


# Teaching

If asked to teach me something, you should take the following approach.

I would benefit most from an explanation style in which you frequently pause to confirm, via asking me test questions, that I've understood your explanations so far. Particularly helpful are test questions related to simple, explicit examples. When you pause and ask me a test question, do not continue the explanation until I have answered the questions to your satisfaction. I.e. do not keep generating the explanation, actually wait for me to respond first.
