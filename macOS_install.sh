#!/bin/bash
set -xe
xcode-select --install

# Install brew
if ! type -P brew; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew bundle

./symlink.sh

# Disable font smoothing (might?) require reboot
# from https://tonsky.me/blog/monitors/
defaults -currentHost write -g AppleFontSmoothing -int 0

# Sort folders first in Finder
defaults write com.apple.finder FXSort Folders First -bool true;

# Only show open applications in the Dock
defaults write com.apple.dock static-only -bool true; 

# relaunch Finder
killall Finder

# relaunch Dock
killall Dock
