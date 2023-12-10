#!/bin/bash -e

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew install coreutils moreutils gnu-tar findutils gawk grep gnu-tar gnu-sed git git-lfs
brew install reattach-to-user-namespace tmux neovim
brew install watch ack tree aria2 fd fzf tig imagemagick jq mtr \
  wget curl cmake htop the_silver_searcher p7zip git-delta ncdu
brew install node rust go 

# Tools
brew install glab glider proxychains-ng youtube-dl \
    bat pstree tree koekeishiya/formulae/skhd ctags 

# Deps & libs
brew install pygit2 numpy opencv 

# Fonts
brew tap homebrew/cask-fonts
brew install font-hack-nerd-font font-fantasque-sans-mono-nerd-font font-fira-code-nerd-font font-noto-color-emoji

# Apps
brew install --cask kitty alacritty \
  obsidian zotero visual-studio-code adobe-acrobat-reader \
  telegram-desktop discord feem slack \
  megasync dropbox \
  caffeine scroll-reverser alt-tab \
  tunnelblick \
  iina qqmusic vnc-viewer
brew install --cask --no-quarantine syntax-highlight
# Zeal: https://github.com/zealdocs/zeal/wiki/Build-Instructions-for-macOS
