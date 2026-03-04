#!/bin/bash
set -e

if ! command -v stow &> /dev/null; then
    brew install stow
fi

cd ~/dotfiles
mkdir -p ~/.claude
stow -R zsh bash emacs claude macos
