#!/bin/bash -e

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew install coreutils gnu-tar findutils gawk grep gnu-tar gnu-sed
brew install reattach-to-user-namespace tmux neovim
brew install wget curl cmake htop the_silver_searcher p7zip git-delta


brew install kitty obsidian zotero visual-studio-code telegram-desktop brew/cask/megasync tunnelblick
#brew install cocoapods
#brew tap iina/homebrew-mpv-iina
#brew install mpv-iina
# Zeal: https://github.com/zealdocs/zeal/wiki/Build-Instructions-for-macOS
