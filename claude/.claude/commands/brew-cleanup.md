# Brewfile Cleanup

Review my Brewfile and identify candidates for removal/uninstall.

1. Read the Brewfile at `~/dotfiles/Brewfile`
2. Categorize installed packages and identify:
   - Redundant packages (e.g., multiple tools that do the same thing)
   - Specialized/niche tools that may no longer be used
   - Multiple similar apps (e.g., multiple browsers, multiple database GUIs)
   - Outdated or superseded packages
   - Dependencies that could be removed from the Brewfile (they'll be auto-installed if needed)
3. Ask me about packages you're unsure about
4. After we decide what to remove, remind me to run `brew bundle cleanup --force --file=~/dotfiles/Brewfile` to uninstall everything not in the Brewfile
