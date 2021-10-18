#!/bin/bash
set -xe
xcode-select --install

if ! type -P brew; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew bundle

./symlink.sh
