#!/bin/bash -ev
# Modify shortcuts to match Linux experience.
#@ is Command
#$ is Shift
#^ is Control
#~ is Option

IFS=$';'

# General Tabs
echo "
Close Tab;^w
New Tab;^t
Zoom In;^=
Zoom Out;^-
" | tail -n+2 | while read name key; do
  defaults write NSGlobalDomain NSUserKeyEquivalents -dict-add $name $key
done

# Chrome windows
echo "
New Window;^n
New Incognito Window;^\$n
Reopen Closed Tab;^\$t
Always Show Bookmarks Bar;^\$b
Reload This Page;^r
Downloads;^\$j
Back;@\\\\U2190
Forward;@\\\\U2192" | tail -n+2  \
  | while read name key; do
  defaults write com.google.Chrome NSUserKeyEquivalents -dict-add $name $key
done

# Global Copy/paste
echo "
Undo;^z
撤销;^z
Redo;^\$z
重做;^\$z
Cut;^x
剪切;^x
Copy;^c
复制;^c
Paste;^v
粘贴;^v
Paste and Match Style;^\$v
Select All;^a
全选;^a
Find...;^f
Find;^f" | tail -n+2  \
  | while read name key; do
  defaults write NSGlobalDomain NSUserKeyEquivalents -dict-add $name $key
  #defaults write org.zotero.zotero NSUserKeyEquivalents -dict-add $name $key
  #defaults write md.obsidian NSUserKeyEquivalents -dict-add $name $key
done
defaults write com.google.Chrome NSUserKeyEquivalents -dict-add "Select All" "^/"


# Obsidian window

echo "
New Note;^t
Open Quickly;^o
Bold;^b
Code;^\`" | tail -n+2  \
  | while read name key; do
  defaults write md.obsidian NSUserKeyEquivalents -dict-add $name $key
done

# Undo kitty. Otherwise it overwrites kitty's own config.
echo "
Close Tab;
New Tab;" | tail -n+2 | while read name key; do
  defaults write net.kovidgoyal.kitty NSUserKeyEquivalents -dict-add $name "$key"
done
