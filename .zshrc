fpath=($HOME/.zsh/Completion $fpath)

[ -d $HOME/bin ] && export PATH=$HOME/bin:$PATH
[ -d $HOME/.zsh/bin ] && export PATH=$HOME/.zsh/bin:$PATH
[ -d $HOME/.local/bin ] && export PATH=$HOME/.local/bin:$PATH
[ -d $HOME/.cw/def ] && export PATH=$HOME/.cw/def:$PATH
[ -d $HOME/.cabal/bin ] && export PATH=$HOME/.cabal/bin:$PATH
[ -d /home/opt/texlive/2012/ ] && export PATH=/home/opt/texlive/2012/bin/x86_64-linux:$PATH
[ -d /usr/lib/colorgcc/bin ] && export PATH=/usr/lib/colorgcc/bin:$PATH
[ -d /home/opt/quartus/quartus/bin ] && export PATH=/home/opt/quartus/quartus/bin:$PATH

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
[ -d $HOME/.rvm/bin ] && export PATH=$PATH:$HOME/.rvm/bin			# Add RVM to PATH for scripting

export NODE_PATH=$HOME/.local/lib/node_modules/
export JDK_HOME=/usr/lib/jvm/java-7-openjdk
export LD_LIBRARY_PATH=/lib/:/home/wyx/.local/lib/wkhtmltox/
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig
#export DISTCC_POTENTIAL_HOSTS='10.20.0.204/8'
export DISTCC_POTENTIAL_HOSTS='166.111.71.24/8 166.111.71.25/16'
export MAKEFLAGS="-j4"
export CXXFLAGS="-Wall -Wextra -std=c++11 -g"

# colors
autoload colors zsh/terminfo
colors
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	eval _$color='%{$terminfo[bold]$fg[${(L)color}]%}'
	eval $color='$fg[${(L)color}]'
done
FINISH="%{$terminfo[sgr0]%}"

# custom rm command
function rm() {
	for file in $@; do
		local FILE_LOC="`readlink -f $file`"
		echo $FILE_LOC
		if [[ $FILE_LOC == /ssd_home/wyx* ]] ; then
			mkdir -p /ssd_home/wyx/tmp/.Trash
			mv "$file" /ssd_home/wyx/tmp/.Trash/ --backup=numbered -fv
		elif [[ $FILE_LOC == /home/* ]]; then
			mkdir -p $HOME/.Trash
			mv "$file" $HOME/.Trash/ --backup=numbered -fv
		else
			/bin/rm "$@" -rvf
		fi
	done
}

autoload -U promptinit
promptinit

if [[ $HOST == "KeepMoving" ]]; then
  alias poweroff='vboxmanage controlvm win7 savestate; sudo poweroff'
  alias reboot='vboxmanage controlvm win7 savestate; sudo reboot'
  export SUDO_PROMPT=$'[\e[31;5msudo\e[m] password for \e[34;1m%p\e[m: (meow~~) '
else
  alias -g halt=
  alias -g poweroff=
  alias -g shutdown=
  alias -g reboot=
  export SUDO_PROMPT=$'[\e[31;5mYou\'re on %H!\e[m] password for \e[34;1m%p\e[m on\e[0;31m %H\e[m: '
fi


source $HOME/.zsh/git-prompt/zshrc.sh

local return_code="%(?..%{$fg[RED]%}%?)%{$reset_color%}"
export RPS1="${return_code}"

function precmd () {
	if [[ $USER == "wyx" ]] && [[ $HOST == "KeepMoving" ]]; then
		PROMPT_PART=""
	elif [[ $HOST == "KeepMoving" ]]; then
		PROMPT_PART="$GREEN [%n]"
	else
		PROMPT_PART="$GREEN [%n@$YELLOW%M]"
	fi

	local promptsize=${#----%D%H-%M--$(git_super_status)}
	(( PR_PWDLEN=${COLUMNS} - $promptsize - 11 - ${#PROMPT_PART}))
	PS1='$CYAN╭─${PROMPT_PART}$MAGENTA [%D{%H:%M}] $GREEN%$PR_PWDLEN<...<%~%<< $(git_super_status)$CYAN
╰─\$'
	PS2='$BLUE($GREEN%_$BLUE)$FINISH'
	PS3='$GREEN Select:'
}
preexec () { print -Pn "\e]0;%n@%M//%/\ $1\a" }

# alias
source $HOME/.aliasrc
alias mv='nocorrect mv -i'
alias mkdir='nocorrect mkdir'
alias cp='nocorrect cp -rvi'
alias -s pdf=mupdf
for i in wmv mkv mp4 mp3 avi rm rmvb flv; alias -s $i=mplayer
for i in jpg png gif; alias -s $i=feh
for i in xls xlsx doc docx ppt pptx; alias -s $i=libreoffice
for i in html,mhtml; alias -s $i=chromium

# Basic
setopt autocd				# cd without 'cd'
setopt braceccl				# ls {a-e.1}
unsetopt hup				# don't close background program when exiting shell
setopt NO_FLOW_CONTROL		# disable Ctrl+s
setopt NOTIFY				# show bg jobs status immediately
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
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=1000000
export SAVEHIST=$HISTSIZE

# key binding
bindkey -e
autoload edit-command-line
zle -N edit-command-line
bindkey -M viins '^v' edit-command-line
bindkey '^e' end-of-line
bindkey '^d' beginning-of-line
bindkey '^h' backward-char
bindkey '^l' forward-char
bindkey '^b' backward-word
bindkey '^f' forward-word
bindkey '^w' backward-delete-word
bindkey '^c' kill-buffer
#bindkey -M viins ' ' magic-space
bindkey ' ' magic-space
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
export LS_COLORS="$LS_COLORS*.f4v=01;35:"		# add custom ls_color

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

# vim edit remote file
function vscp() {
    if [[ -z $1 ]]; then
        echo "usage: vscp [[user@]host1:]file1 ... [[user@]host2:]file2"
        return
    fi
    declare -a targs=()
	echo "Editing Remote Files"
    for iarg in $@; do
        targ="scp://$(echo $iarg | sed -e 's@:/@//@' | sed -e 's@:@/@')"
        targs=("${targs[@]}" $targ)
    done
	echo ${targs[@]}
    vim ${targs[@]}
}
compdef vscp=scp
compdef telnet=scp

# specific filetype
_pic() { _files -g '*.(jpg|png|bmp|gif|ppm|pbm|jpeg)(-.)' }
compdef _pic gimp
compdef _pic feh

# vim ignore
zstyle ':completion:*:*:vim:*:*files' ignored-patterns '*.(avi|mkv|rmvb|pyc|wmv)'

# Pinyin Completion
[[ -d $HOME/.zsh/Pinyin-Completion ]] && source $HOME/.zsh/Pinyin-Completion/shell/pinyin-comp.zsh && export PATH=$PATH:$HOME/.zsh/Pinyin-Completion/bin

# npm completion
which npm > /dev/null 2>&1 && eval "$(npm completion 2 > /dev/null)"

# hub completion
#which hub > /dev/null 2>&1 && eval "$(hub alias -s)"

# ... completion
user_complete(){
	if [[ -z $BUFFER ]]; then
		return
	fi
	if [[ $BUFFER =~ "^\.\.\.*$" ]]; then
		BUFFER=`echo "$BUFFER" |sed 's/^/cd\ /g'`
		zle end-of-line
		user_complete
		return
	elif [[ $BUFFER =~ ".*\.\.\..*$" ]] ;then
		BUFFER=`echo "$BUFFER" |sed 's/\.\.\./\.\.\/\.\./g'`
		zle end-of-line
		user_complete
		return
	fi
	zle expand-or-complete
	#recolor-cmd
}
zle -N user_complete
bindkey "\t" user_complete
autoload compinstall

# Custom Return
path_parse(){
	if [[ $BUFFER = "." ]]; then
		BUFFER="cd ../"
		return
	elif [[ $BUFFER =~ "^\./.*" ]]; then		# automatic cd to directory when executed
		if [[ ! -f $BUFFER ]] && [[ -d $BUFFER ]]; then
			BUFFER="cd $BUFFER"
			path_parse
		fi
	elif [[ $BUFFER =~ ".*\.\.\..*" ]] ;then	# complete ...
		BUFFER=`echo "$BUFFER" |sed 's/\.\.\./\.\.\/\.\./g'`
		path_parse
	elif [[ $BUFFER =~ "^\.\..*" ]]; then		# auto add cd
		if [[ -d `echo "$BUFFER" |sed 's/\\\ /\ /g; s/l$//g; s/ls$//g'` ]]; then
			BUFFER=`echo "$BUFFER" |sed 's/^/cd\ /g'`
			path_parse
		fi
		zle accept-line
	elif [[ $BUFFER =~ "^cd .*/ls*$" ]] ; then	# fix ls typo
		BUFFER=`echo "$BUFFER" |sed 's/l[^\/]*$/;ls/g' `
		zle end-of-line
	fi
}
bg_list=(pdf geeqie libreoffice word evince)
special_command(){
	cmd=`echo $BUFFER | sed 's/^\ *//g' | sed 's/\ .*//g'`
	# command running in background
	in_array $cmd "${bg_list[@]}" && BUFFER=`echo $BUFFER |sed 's/[&]*\s*$/\ 2>\/dev\/null\ \&/g'`

	## command ending with alert
	alert_list=(mencoder aria2c axel)
	in_array $cmd "${alert_list[@]}" && BUFFER=`echo $BUFFER |sed 's/$/\ ;notify-send finished/g'`
}

user-ret(){
	path_parse
	if [[ $HOST == "KeepMoving" ]]; then
		special_command
	fi
	zle accept-line
}
zle -N user-ret
bindkey "\r" user-ret

# command not found
function command_not_found_handler() {
	local command="$1"
	# to avoid infinite recursion
	[ -x /usr/bin/fortune ] && [ -x /usr/bin/cowsay ] && {
		fortune -s | cowsay -W 70
	}
	[ -n "$command" ] && [ -x /usr/bin/pkgfile ] && {
		echo -e "searching for \"$command\" in repos..."
		local pkgs="$(pkgfile -b -v -- "$command")"
		if [ ! -z "$pkgs" ]; then
			echo -e "\"$command\" may be found in the following packages:\n\n${pkgs}\n"
		fi
	}
	return 1
}

# plugins
if [[ -d $HOME/.zsh ]]; then
	source $HOME/.zsh/extract.zsh
	# the next two have to be this order
	source $HOME/.zsh/syntax-highlighting/zsh-syntax-highlighting.zsh
	source $HOME/.zsh/history-substring-search.zsh
	HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS="I"			# sensitive search
	source $HOME/.zsh/autojump/etc/profile.d/autojump.zsh
fi
if [ $commands[fasd] ]; then
	eval "$(fasd --init zsh-hook zsh-wcomp zsh-wcomp-install)"
	#eval "$(fasd --init zsh-wcomp zsh-wcomp-install)"	 # this should be enabled periodically
	alias o='f -e xdg-open'
	alias fv='f -e vim'
	bindkey '^X^O' fasd-complete
fi

