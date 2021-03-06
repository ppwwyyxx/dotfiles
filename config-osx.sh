#!/bin/bash -e
# File: osx.sh

# http://scriptingosx.com/2016/11/editing-property-lists/

echo "Expanding the save panel by default"
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

echo "Automatically quit printer app once the print jobs complete"
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

echo "Displaying ASCII control characters using caret notation in standard text views"
defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true

echo "Reveal IP address, hostname, OS version, etc. when clicking the clock in the login window"
sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

echo "Check for software updates daily, not just once per week"
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

echo "Disable the menubar transparency? (y/n)"
# defaults write com.apple.universalaccess reduceTransparency -bool true

echo "Enabling full keyboard access for all controls (enable Tab in modal dialogs, menu windows, etc.)"
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

echo "Disabling press-and-hold for special keys in favor of key repeat"
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

echo "Setting a blazingly fast keyboard repeat rate"
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 10

echo "Setting trackpad & mouse speed to a reasonable number"
defaults write -g com.apple.trackpad.scaling 2
defaults write -g com.apple.mouse.scaling 2.5

defaults write com.apple.screencapture location -string "$HOME/Pictures"

echo "Enabling subpixel font rendering on non-Apple LCDs"
defaults write NSGlobalDomain AppleFontSmoothing -int 2

echo "Show status bar in Finder by default? (y/n)"
defaults write com.apple.finder ShowStatusBar -bool true

echo "Display full POSIX path as Finder window title? (y/n)"
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

echo "Disable the warning when changing a file extension? (y/n)"
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

echo "Allow quitting Finder via ⌘ + Q; doing so will also hide desktop icons"
defaults write com.apple.finder QuitMenuItem -bool true

echo "Avoid creation of .DS_Store files on network volumes? (y/n)"
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

echo "Allowing text selection in Quick Look/Preview in Finder by default"
defaults write com.apple.finder QLEnableTextSelection -bool true

echo "Setting the icon size of Dock items to 36 pixels for optimal size/screen-realestate"
defaults write com.apple.dock tilesize -int 60

echo "Speeding up Mission Control animations and grouping windows by application"
# may cause Xquartz problems?
# defaults write com.apple.dock expose-animation-duration -float 0.1
# defaults write com.apple.dock "expose-group-by-app" -bool true

# defaults write com.apple.finder DisableAllAnimations -bool true
# defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false

#@ is Command
#$ is Shift
#^ is Control
#~ is Option
echo "Reset Terminal shortcuts"
defaults write com.apple.Terminal NSUserKeyEquivalents -dict-add "Split Pane" "nil"
defaults write com.apple.Terminal NSUserKeyEquivalents -dict-add "Copy" "^\$c"
defaults write com.apple.Terminal NSUserKeyEquivalents -dict-add "Paste" "^\$v"
defaults write com.apple.Terminal NSUserKeyEquivalents -dict-add "Bigger" "^="
defaults write com.apple.Terminal NSUserKeyEquivalents -dict-add "Smaller" "^-"

echo "Enable Secure Keyboard Entry in Terminal.app"
# See: https://security.stackexchange.com/a/47786/8918
defaults write com.apple.terminal SecureKeyboardEntry -bool true

echo "Privacy: Don't send search queries to Apple"
defaults write com.apple.Safari UniversalSearchEnabled -bool false
defaults write com.apple.Safari SuppressSearchSuggestions -bool true

echo "Disable automatic capitalization as it's annoying when typing code"
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false

echo "Trackpad: enable tap to click for this user and for the login screen"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

echo "Show the ~/Library folder"
chflags nohidden ~/Library

