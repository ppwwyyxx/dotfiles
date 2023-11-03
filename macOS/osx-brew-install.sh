#!/bin/bash -e

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew install coreutils moreutils gnu-tar findutils gawk grep gnu-tar gnu-sed
brew install reattach-to-user-namespace tmux neovim
brew install watch ack tree aria2 fd fzf tig imagemagick jq mtr \
  node rust proxychains-ng wget curl cmake htop the_silver_searcher p7zip git-delta ncdu
brew install go glider koekeishiya/formulae/skhd ctags numpy opencv


brew install --cask kitty alacritty \
  obsidian zotero visual-studio-code \
  telegram-desktop discord \
  megasync dropbox \
  caffeine scroll-reverser alt-tab \
  tunnelblick iina qqmusic vnc-viewer
#brew install cocoapods
# Zeal: https://github.com/zealdocs/zeal/wiki/Build-Instructions-for-macOS

brew tap homebrew/cask-fonts
brew install font-hack-nerd-font font-fantasque-sans-mono-nerd-font font-fira-code-nerd-font font-noto-color-emoji
