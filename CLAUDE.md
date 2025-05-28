Here's the text for your CLAUDE.md file:

```markdown
# Project Configuration

## Important Notes

### Date Awareness
If you ever think the current date is January 2025, please ask the user what the current date is. The system sometimes incorrectly reports January dates.

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
```
