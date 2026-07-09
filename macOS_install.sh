#!/bin/bash
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BREWFILE="$DOTFILES_DIR/Brewfile"
EMACSCLIENT_WRAPPER="$DOTFILES_DIR/EmacsclientWrapper.app"
PLIST="$EMACSCLIENT_WRAPPER/Contents/Info.plist"
SKIP_BREW=0
WITH_MAS=0

while [[ "$#" -gt 0 ]]; do
    case "$1" in
        --skip-brew)
            SKIP_BREW=1
            ;;
        --with-mas)
            WITH_MAS=1
            ;;
        *)
            echo "Usage: $0 [--skip-brew] [--with-mas]" >&2
            exit 64
            ;;
    esac
    shift
done

# xcode-select --install

# Install brew.
if [[ "$SKIP_BREW" -eq 0 ]]; then
    if ! command -v brew >/dev/null 2>&1; then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    brew trust --tap d12frosted/emacs-plus
    brew trust --tap stripe/stripe-cli
    HOMEBREW_BUNDLE_JOBS=1 brew bundle --file "$BREWFILE"

    if [[ "$WITH_MAS" -eq 1 ]]; then
        HOMEBREW_BUNDLE_JOBS=1 brew bundle --file "$DOTFILES_DIR/Brewfile.mas"
    fi
fi

# Symlink dotfiles.
mkdir -p "$HOME/.claude" "$HOME/.codex"
stow --dir "$DOTFILES_DIR" --target "$HOME" -R zsh bash emacs claude codex agents macos

# Git config
git config --global rerere.enabled true

# Compile EmacsclientWrapper AppleScript to app bundle
rm -rf "$EMACSCLIENT_WRAPPER"
osacompile -o "$EMACSCLIENT_WRAPPER" "$DOTFILES_DIR/emacsclient-wrapper.applescript"

# Add bundle identifier and document support to Info.plist (required for duti).
if /usr/libexec/PlistBuddy -c "Print :CFBundleIdentifier" "$PLIST" >/dev/null 2>&1; then
    /usr/libexec/PlistBuddy -c "Set :CFBundleIdentifier com.local.EmacsclientWrapper" "$PLIST"
else
    /usr/libexec/PlistBuddy -c "Add :CFBundleIdentifier string com.local.EmacsclientWrapper" "$PLIST"
fi

/usr/libexec/PlistBuddy -c "Delete :CFBundleDocumentTypes" "$PLIST" 2>/dev/null || true
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes array" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0 dict" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:CFBundleTypeName string Documents" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:CFBundleTypeRole string Editor" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:LSHandlerRank string Alternate" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:CFBundleTypeExtensions array" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:CFBundleTypeExtensions:0 string *" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:LSItemContentTypes array" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:LSItemContentTypes:0 string public.item" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:LSItemContentTypes:1 string public.data" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:LSItemContentTypes:2 string public.text" "$PLIST"
/usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:LSItemContentTypes:3 string public.source-code" "$PLIST"

# Register EmacsclientWrapper app with Launch Services
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f "$EMACSCLIENT_WRAPPER"

# Apply file associations (set Emacsclient wrapper as default for code/text files)
duti "$HOME/.duti"

# Disable font smoothing (might?) require reboot
# from https://tonsky.me/blog/monitors/
defaults -currentHost write -g AppleFontSmoothing -int 0

# Sort folders first in Finder
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# Show hidden files in Finder
defaults write com.apple.finder AppleShowAllFiles -bool true

# show list view by default
defaults write com.apple.finder FXPreferredViewStyle -string Nlsv

# Remove pinned apps and recent apps from the Dock
defaults write com.apple.dock persistent-apps -array
defaults write com.apple.dock show-recents -bool false
defaults write com.apple.dock static-only -bool true


# relaunch Finder
killall Finder || true

# relaunch Dock
killall Dock || true
