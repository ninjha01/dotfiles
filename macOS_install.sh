#!/bin/bash
xcode-select --install

if ! type -P brew; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew install pandoc \
     node \
     ispell \
     imagemagick \
     awscli \
     eslint \
     ffmpeg \
     gnutls \
     prettier \
     redis \
     sqlite \
     the_silver_searcher \
     watchman \
     yarn \
     youtube-dl \
     shellcheck \
     proselint \
     wget \
     postgresql \
     proselint

brew cask install \
     miniconda \
     adoptopenjdk8 \
     aerial \
     alfred \
     android-studio \
     bartender \
     calibre \
     chromedriver \
     docker \
     emacs \
     google-backup-and-sync \
     google-chrome \
     google-cloud-sdk \
     itsycal \
     keepassxc \
     microsoft-office \
     spectacle \
     transmission \
     xquartz
