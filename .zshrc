fpath=($HOME/.zsh/Completion $fpath)
[ -d $HOME/bin ] && export PATH=$HOME/bin:$PATH
[ -d $HOME/.zsh/bin ] && export PATH=$HOME/.zsh/bin:$PATH
[ -d $HOME/.local/bin ] && export PATH=$HOME/.local/bin:$PATH
[ -d $HOME/.cw/def ] && export PATH=$HOME/.cw/def:$PATH
[ -d /usr/lib/colorgcc/bin ] && export PATH=/usr/lib/colorgcc/bin:$PATH
[ -d $HOME/.cabal/bin ] && export PATH=$HOME/.cabal/bin:$PATH

# colors
autoload colors
colors
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
eval _$color='%{$terminfo[bold]$fg[${(L)color}]%}'
eval $color='$fg[${(L)color}]'
(( count = $count + 1 ))
done
FINISH="%{$terminfo[sgr0]%}"

# custom rm command
function rm(){
	if [ "`pwd -P`" =~ "^/home/$USER" ]; then
		mkdir -p $HOME/.Trash
		mv "$@" $HOME/.Trash --backup=numbered -f
	else
		mv "$@" /ssd_home/wyx/tmp/.Trash --backup=numbered -f
	fi
}

# prompt
autoload -U promptinit
promptinit
source $HOME/.zsh/git-prompt/zshrc.sh
PROMPT=$(echo '$CYAN╭─$GREEN [%n@$YELLOW%M]$MAGENTA [%T] $GREEN%~$(git_super_status)$CYAN\n╰─\$')
#PROMPT=$'$CYAN┌─$MAGENTA%D %T $CYAN%n@$YELLOW%M:$GREEN%~$CYAN\n└─\$'
local return_code="%(?..%{$fg[RED]%}%?)%{$reset_color%}"
export RPS1="${return_code}"
case $TERM in (*xterm*|*rxvt*|(dt|k|E)term)
	precmd () { print -Pn "\e]0;%~\a" }
	preexec () { print -Pn "\e]0;%n@%M//%/\ $1\a" }
	;;
esac

# alias
source $HOME/.aliasrc
alias mv='nocorrect mv -i'
alias mkdir='nocorrect mkdir'
alias cp='nocorrect cp -rvi'
alias -s pdf=evince
for i in wmv mkv mp4 mp3 avi rm rmvb flv; alias -s $i=mplayer
for i in jpg png gif; alias -s $i=feh
for i in xls xlsx doc docx ppt pptx; alias -s $i=libreoffice

# Basic
unsetopt hup				# don't close background program when exiting shell
setopt AUTO_PUSHD			# cd to auto pushd
setopt pushdignoredups
setopt NO_FLOW_CONTROL		# disable Ctrl+s
setopt notify				# show bg jobs status immediately
limit coredumpsize 0		# disable core dumps
WORDCHARS='*?[]~=&;!#$%^(){}<>'
setopt EXTENDED_GLOB
unsetopt CASE_GLOB
setopt correctall
zmodload zsh/mathfunc
autoload -U zsh-mime-setup
zsh-mime-setup

# History
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_FUNCTIONS
setopt SHARE_HISTORY
export HISTFILE=/home/wyx/.zsh_history
export HISTSIZE=1000000
export SAVEHIST=$HISTSIZE

# key binding
bindkey -e
autoload edit-command-line
zle -N edit-command-line
bindkey -M viins '^x^e' edit-command-line
#bindkey '^e' end-of-line
#bindkey '^d' beginning-of-line
bindkey '^h' backward-char
bindkey '^l' forward-char
bindkey '^b' backward-word
bindkey '^w' forward-word
bindkey '^u' backward-delete-word
#bindkey -M viins ' ' magic-space
#bindkey -M vicmd 'u' undo
#bindkey -M vicmd "q" push-line
autoload zkbd
[[ ! -f ${ZDOTDIR:-$HOME}/.zsh/zkbd/$TERM ]] && zkbd
source ${ZDOTDIR:-$HOME}/.zsh/zkbd/$TERM
[[ -n ${key[Backspace]} ]] && bindkey "${key[Backspace]}" backward-delete-char
[[ -n ${key[Insert]} ]] && bindkey "${key[Insert]}" overwrite-mode
[[ -n ${key[Home]} ]] && bindkey "${key[Home]}" beginning-of-line
#[[ -n ${key[PageUp]} ]] && bindkey "${key[PageUp]}" up-line-or-history
#[[ -n ${key[PageDown]} ]] && bindkey "${key[PageDown]}" down-line-or-history
[[ -n ${key[PageUp]} ]] && bindkey "${key[PageUp]}" history-substring-search-up
[[ -n ${key[PageDown]} ]] && bindkey "${key[PageDown]}" history-substring-search-down
[[ -n ${key[Delete]} ]] && bindkey "${key[Delete]}" delete-char
[[ -n ${key[End]} ]] && bindkey "${key[End]}" end-of-line
[[ -n ${key[Up]} ]] && bindkey "${key[Up]}" up-line-or-search
[[ -n ${key[Down]} ]] && bindkey "${key[Down]}" down-line-or-search
[[ -n ${key[Left]} ]] && bindkey "${key[Left]}" backward-char
[[ -n ${key[Right]} ]] && bindkey "${key[Right]}" forward-char

# add sudo
sudo-command-line() {
	[[ -z $BUFFER ]] && zle up-history
	[[ $BUFFER != sudo\ * ]] && BUFFER="sudo $BUFFER"
	zle end-of-line
	#recolor-cmd
}
zle -N sudo-command-line
bindkey "${key[F2]}" sudo-command-line

# Complete
autoload -U compinit
compinit
setopt AUTO_LIST
setopt AUTO_MENU
setopt MENU_COMPLETE
setopt complete_in_word   # complete /v/c/a/p
setopt nonomatch		  # enhanced bash wildcard completion

# ignore the current directory
zstyle ':completion:*:cd:*' ignore-parents parent pwd

zstyle ':completion::complete:*' cache-path .zcache
zstyle ':completion:*' verbose yes
zstyle ':completion:*' menu select
zstyle ':completion:*:*:default' force-list always
zstyle ':completion:*' select-prompt '%SSelect:  lines: %L  matches: %M  [%p]'
zstyle ':completion:*:match:*' original only
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:predict:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct

# Path Completion
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-shlashes 'yes'
zstyle ':completion::complete:*' '\\'

# Colorful Completion
eval $(dircolors -b)
export ZLSCOLORS="${LS_COLORS}"
zmodload zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

# Fix case and typo
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Grouping Completion
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:descriptions' format $'\e[01;33m -- %d --\e[0m'
zstyle ':completion:*:messages' format $'\e[01;35m -- %d --\e[0m'
zstyle ':completion:*:warnings' format $'\e[01;31m -- No Matches Found --\e[0m'
zstyle ':completion:*:corrections' format $'\e[01;32m -- %d (errors: %e) --\e[0m'

# huge list
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:default' menu 'select=0'
# Completing order
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' completer _complete _prefix _user_expand _correct _prefix _match
# newer file first
zstyle ':completion:*' file-sort modification reverse
# Separate man page sections.
zstyle ':completion:*:manuals' separate-sections true
# Egomaniac! XXX
zstyle ':completion:*' list-separator 'wyx'
# complete with a menu for xwindow ids
zstyle ':completion:*:windows' menu on=0
zstyle ':completion:*:expand:*' tag-order all-expansions

#kill completion
compdef pkill=kill
compdef pkill=killall
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:*:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps -au$USER '

# host and ip completion
host_completion=(root@10.42.0.89
wyx@59.66.132.22
ppwwyyxx@server3.net9.org
ppwwyyxx@server4.net9.org
ppwwyyxx@59.66.131.63
wyx@omni.tuna.tsinghua.edu.cn
wyx@59.66.16.35
yuxin.wu@10.20.0.204
)
zstyle -e ':completion:*' hosts 'reply=($host_completion)'
zstyle ':completion:*:ping:*' www.google.com 59.66.132.1 166.111.8.28

# buffer words completion for tmux
tmux_buffer_completion() {
	local expl
	local -a w npane
	npane=$(tmux list-panes |tail -n 1 |sed 's/:.*//g')
	if [[ -z "$TMUX_PANE" ]]; then
		_message "not running inside tmux!"
		return 1
	fi
	for i in $(seq 0 $npane); do
		w=$w${(u)=$(tmux capture-pane -t $i \; show-buffer \; delete-buffer)}
	done
	w=(  $(echo $w)  )							# why must use a echo
	_wanted values expl 'words from pane buffer' compadd -a w
}
zle -C tmux-pane-words-prefix   complete-word _generic
zle -C tmux-pane-words-anywhere complete-word _generic
bindkey '^P' tmux-pane-words-prefix
bindkey '^X^O' tmux-pane-words-anywhere
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer tmux_buffer_completion
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'

# vim ignore
zstyle ':completion:*:*:vim:*:*files' ignored-patterns '*.(o|avi|mkv|rmvb|pyc|sqlite3|png|jpg|gif|aux|toc|pyg|mp3|wmv)'

# pinyin completion
[[ -d $HOME/.zsh/Pinyin-Completion ]] && source $HOME/.zsh/Pinyin-Completion/shell/pinyin-comp.zsh

# npm c	ompletion
eval "$(npm completion 2 > /dev/null)"

# hub completion
eval "$(hub alias -s)"

# ... complete
user-complete(){
	if [[ -z $BUFFER ]]; then
		BUFFER="cd "
		zle end-of-line
		return
	fi
	if [[ $BUFFER =~ "^\.\.\.*$" ]]; then
		BUFFER=`echo "$BUFFER" |sed 's/^/cd\ /g'`
		zle end-of-line
		user-complete
		return
	elif [[ $BUFFER =~ ".*\.\.\..*$" ]] ;then
		BUFFER=`echo "$BUFFER" |sed 's/\.\.\./\.\.\/\.\./g'`
		zle end-of-line
		user-complete
		return
	fi
	zle expand-or-complete
	#recolor-cmd
}
zle -N user-complete
bindkey "\t" user-complete
autoload compinstall

# Custom Return
path_parse(){
	if [[ $BUFFER = "" ]] ;then
		BUFFER="ls"
		zle end-of-line
	elif [[ $BUFFER = "." ]]; then
		BUFFER=".."
		path_parse
	elif [[ $BUFFER =~ ".*\.\.\..*" ]] ;then
		BUFFER=`echo "$BUFFER" |sed 's/\.\.\./\.\.\/\.\./g'`
		path_parse
	elif [[ $BUFFER =~ "^\.\..*" ]]; then
		if [[ -d `echo "$BUFFER" |sed 's/\\\ /\ /g'|sed 's/l$//g' |sed 's/ls$//g'` ]]; then
			BUFFER=`echo "$BUFFER" |sed 's/^/cd\ /g'`
			path_parse
		fi
		zle accept-line
	elif [[ $BUFFER =~ "^cd .*/ls*$" ]] ; then
		BUFFER=`echo "$BUFFER" |sed 's/l[^\/]*$/;ls/g' `
		zle end-of-line
	fi
}
special_command(){
	cmd=`echo $BUFFER | sed 's/^\ *//g' | sed 's/\ .*//g'`
	# command running in background
	bg_list=(pdf gqview libreoffice word evince)
	in_array $cmd "${bg_list[@]}" && BUFFER=`echo $BUFFER |sed 's/[&]*\s*$/\ 2>\/dev\/null\ \&/g'`

	# command ending with alert
	alert_list=(mencoder aria2c axel notify-send)
	in_array $cmd "${alert_list[@]}" && BUFFER=`echo $BUFFER |sed 's/$/\ ;disp finished/g'`

	#pacman color
	BUFFER=`echo "$BUFFER" |sed 's/pacman\ /pacman-color\ /g'`
}

user-ret(){
	path_parse
	special_command
	zle accept-line
}
zle -N user-ret
bindkey "\r" user-ret

# command not found
function command_not_found_handler() {
	local command="$1"
	[ -n "$command" ] && [ -x /usr/bin/pkgfile ] && {
		echo -e "searching for \"$command\" in repos..."
		local pkgs="$(pkgfile -b -v -- "$command")"
		if [ ! -z "$pkgs" ]; then
			echo -e "\"$command\" may be found in the following packages:\n\n${pkgs}\n"
		fi
	}
	return 1
}

# for mathmu
#export LD_LIBRARY_PATH+=/usr/local/bin
# for ubuntu include
#export LIBRARY_PATH=/lib/
#export CPLUS_INCLUDE_PATH=/usr/include/x86_64-linux-gnu

# plugins
if [[ -d $HOME/.zsh ]]; then
	source $HOME/.zsh/extract.zsh
	source $HOME/.zsh/syntax-highlighting/zsh-syntax-highlighting.zsh
	source $HOME/.zsh/history-substring-search.zsh
	source $HOME/.zsh/etc/profile.d/autojump.zsh
fi
if [ $commands[fasd] ]; then
	eval "$(fasd --init auto zsh-wcomp zsh-wcomp-install)"
	eval "$(fasd --init zsh-wcomp zsh-wcomp-install)"
	alias o='f -e xdg-open'
	alias fv='f -e vim'
	bindkey '^X^A' fasd-complete
fi
