#!/bin/bash
set -xe
xcode-select --install

# Install brew
if ! type -P brew; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew bundle

./symlink.sh

# Compile EmacsclientWrapper AppleScript to app bundle
osacompile -o ~/dotfiles/EmacsclientWrapper.app ~/dotfiles/emacsclient-wrapper.applescript

# Add bundle identifier to Info.plist (required for duti)
/usr/libexec/PlistBuddy -c "Add :CFBundleIdentifier string com.local.EmacsclientWrapper" ~/dotfiles/EmacsclientWrapper.app/Contents/Info.plist 2>/dev/null || true
/usr/libexec/PlistBuddy -c "Set :CFBundleTypeRole Editor" ~/dotfiles/EmacsclientWrapper.app/Contents/Info.plist 2>/dev/null || true

# Register EmacsclientWrapper app with Launch Services
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f ~/dotfiles/EmacsclientWrapper.app

# Apply file associations (set Emacsclient wrapper as default for code/text files)
duti ~/.duti

# Disable font smoothing (might?) require reboot
# from https://tonsky.me/blog/monitors/
defaults -currentHost write -g AppleFontSmoothing -int 0

# Sort folders first in Finder
defaults write com.apple.finder FXSort Folders First -bool true;

# Show hidden files in Finder
defaults write com.apple.finder AppleShowAllFiles YES;

# show list view by default
defaults write com.apple.finder FXPreferredViewStyle Nlsv;

# Only show open applications in the Dock
defaults write com.apple.dock static-only -bool true; 


# relaunch Finder
killall Finder

# relaunch Dock
killall Dock
