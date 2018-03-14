# learning: A-q: push-line. c-r + c-o: accept-line-and-down-history
# !history expansion; !:1, !$, !:1-3, !:* take word from last command
# !-4 fourth last command; !ls, most recent command with ls; !# current buffer
# ^foo^bar^:G  global substitution on last command, !?str?:s^foo^bar^:G, on last command containing str
#http://lilydjwg.is-programmer.com/2012/3/19/thress-zsh-line-editor-tips.32549.html

if [[ $(uname) == "Darwin" ]]; then
	export _CFG_ON_MAC=1
	source /etc/profile
fi

[[ -d $HOME/.zsh/Completion ]] && fpath=($HOME/.zsh/Completion $fpath)

# ENV f[[
export PYTHONPATH=
export TERM=screen-256color
export TERMINFO=$HOME/.terminfo
export LC_ALL=en_US.UTF-8
export SSH_ASKPASS=
# neovim#2048 suggests: infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti; tic $TERM.ti

function safe_export_path() { [[ -d $1 ]] && export PATH=$1:$PATH }
function safe_source() { [[ -s $1 ]] && source $1 }
safe_source $HOME/.profile

[[ -n $_CFG_ON_MAC ]] && safe_export_path /usr/local/opt/coreutils/libexec/gnubin
safe_export_path $HOME/bin
safe_export_path $HOME/.local/bin
safe_export_path $HOME/.zsh/bin
safe_export_path $HOME/.cabal/bin
safe_export_path /opt/texlive/2015/bin/x86_64-linux
safe_export_path /opt/intel/bin
safe_export_path /opt/intel/vtune_amplifier_xe_2015.3.0.403110/bin64
safe_export_path /usr/lib/colorgcc/bin
safe_export_path /opt/lingo14/bin/linux64
safe_export_path $HOME/.linuxbrew/bin
safe_export_path $HOME/.rvm/bin		# Add RVM to PATH for scripting
export GOPATH=$HOME/.local/gocode
safe_export_path $GOPATH/bin

# local prefix
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$HOME/.local/lib
export LIBRARY_PATH=${LIBRARY_PATH+$LIBRARY_PATH:}$HOME/.local/lib
export CPATH=${CPATH+$CPATH:}$HOME/.local/include
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$HOME/.local/lib/pkgconfig

# override tmux master key under ssh
if [[ -n "$TMUX" ]] && [[ -n "$SSH_CLIENT" ]]; then
	tmux set -g status-bg cyan
	tmux unbind C-q
	tmux set -g prefix C-a
	tmux bind C-a send-prefix
fi


# dev libraries
if [[ -d /opt/intel/mkl ]]; then
	export MKLROOT=/opt/intel/mkl
	export LD_LIBRARY_PATH=`readlink -f $MKLROOT/../compiler/lib/intel64`:$MKLROOT/lib/intel64:$LD_LIBRARY_PATH;
	export LIBRARY_PATH=`readlink -f $MKLROOT/../compiler/lib/intel64`:$MKLROOT/lib/intel64:$LIBRARY_PATH;
	#export CPATH=$MKLROOT/include/:$CPATH
fi
function try_use_cuda_home() {
	if [[ -d "$1/lib64" ]]; then
		export CUDA_HOME="$1"
		export LD_LIBRARY_PATH=$CUDA_HOME/lib64:$LD_LIBRARY_PATH
		if [[ -d "$CUDA_HOME/extras/CUPTI/lib64" ]]; then
			export LD_LIBRARY_PATH=$CUDA_HOME/extras/CUPTI/lib64:$LD_LIBRARY_PATH
		fi
		export LIBRARY_PATH=$CUDA_HOME/lib64:$LIBRARY_PATH
		#export CPATH=$CUDA_HOME/include
		export PATH=$PATH:$CUDA_HOME/bin
	fi
}
function try_use_cudnn() {
	if [[ -d "$1/lib64" ]]; then
		export LD_LIBRARY_PATH=$1/lib64:$LD_LIBRARY_PATH
		export LIBRARY_PATH=$1/lib64:$LIBRARY_PATH
		#export CPATH=$1/include:$CPATH
	fi
}
try_use_cuda_home /usr/local/cuda
try_use_cuda_home /opt/cuda		# ArchLinux
try_use_cudnn /usr/local/cudnn

export MAKEFLAGS="-j8"
export CXXFLAGS="-Wall -Wextra"
export NODE_PATH=$HOME/.local/lib/node_modules/
[[ -s ~/.config/python/startup.py ]] && export PYTHONSTARTUP=~/.config/python/startup.py

export EDITOR=vim
export BROWSER=chromium
export PAGER="/usr/bin/less -r"
export LESS_TERMCAP_mb=$YELLOW
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
export SDCV_PAGER="sed 's/\ \ \([1-9]\)/\n\n◆\1/g' |less"
cdpath=(~)
# f]]

# PROMPT: f[[
autoload -U promptinit
promptinit

# colors
autoload colors zsh/terminfo
colors
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	eval _$color='%{$terminfo[bold]$fg[${(L)color}]%}'
	eval $color='$fg[${(L)color}]'
done
FINISH="%{$terminfo[sgr0]%}"

if [[ $HOST == "KeepLearning" ]]; then
  alias poweroff='vboxmanage controlvm win7 savestate; sudo poweroff'
  alias reboot='vboxmanage controlvm win7 savestate; sudo reboot'
  export SUDO_PROMPT=$'[\e[31;5msudo\e[m] password for \e[34;1m%p\e[m: (meow~~) '
else
  # avoid shutting server down by mistake!
  alias -g halt=
  alias -g poweroff=
  alias -g shutdown=
  alias -g reboot=
  export SUDO_PROMPT=$'[\e[31;5mYou\'re on %H!\e[m] password for \e[34;1m%p\e[m on\e[0;31m %H\e[m: '
fi

# Config git-prompt
export ZSH_THEME_GIT_PROMPT_CACHE=1
safe_source $HOME/.zsh/git-prompt/zshrc.sh
[[ -d $HOME/.zsh/git-prompt ]] && {
	# init git status on zsh start
	update_current_git_vars
} || { function  git_super_status() {} }

function preexec() {
COMMAND_TIMER=${COMMAND_TIMER:-$((SECONDS + $(date "+%N") / 1000000000.0))}
}
function precmd() {
	local separator1=
  local separator2=
  local separator3=
	local TIMECOLOR="%{%b%F{211}%}"
	local PINK="%{%b%F{213}%}"
	local YELLOWGREEN="%{%b%F{154}%}"
	local YELLOWGREENB="%{%b%K{154}%F{black}%}"
	local PURPLE="%{%b%F{171}%}"

	if [[ $USER == "wyx" ]] && [[ $HOST == "KeepLearning" ]]; then
		PROMPT_PART="" # on my laptop
	else
		PROMPT_PART="$GREEN [%{%F{171}%}%n@%{%F{219}%}%M$GREEN]"
	fi

	# to calculate length
	local git_status="$(git_super_status)"
	local prompt_nodir="-----$(date +%H:%M)---$git_status$PROMPT_PART"
	local zero='%([BSUbfksu]|([FB]|){*})'	# used to calculate length withou control sequence
	local part_length=${#${(S%%)prompt_nodir//$~zero/}}
	local pwdlen=$((${COLUMNS} - $part_length - 3))
	local START_BOLD=$'\e[1m'		# bold on
	local END_BOLD=$'\e[22m'		# bold off

	local INDICATOR="\$"
	#local INDICATOR="❱"
	#local INDICATOR="%{$fg_bold[red]%}❮%{$reset_color%}%{$fg[red]%}❮❮%{$reset_color%}"
	[[ -n "$VIRTUAL_ENV" ]] && VIRTUAL="(`basename $VIRTUAL_ENV`)"
	# my magic prompt
	#export PROMPT="$START_BOLD$CYAN╭─${VIRTUAL}${PROMPT_PART}\
#$TIMECOLOR [%D{%H:%M}] \
#$YELLOWGREEN%$pwdlen<...<%~%<< \
#%{$reset_color%}$git_status%{$CYAN%}
#╰─%{$reset_color%}🐻%{$CYAN%}%(?..%{$fg[red]%})$INDICATOR%{$reset_color%}"
	export PROMPT="$START_BOLD$CYAN╭─${VIRTUAL}${PROMPT_PART}\
$TIMECOLOR [%D{%H:%M}] \
$YELLOWGREEN%$pwdlen<...<%~%<< \
%{$reset_color%}$git_status%{$CYAN%}
╰─%{$reset_color%}%{$CYAN%}%(?..%{$fg[red]%})$INDICATOR%{$reset_color%}"

	#local return_status="%{$fg[red]%}%(?..%?⏎)%{$reset_color%}"	# return code is useless
	local return_status="%{$fg[red]%}%(?..⏎)%{$reset_color%}"
	RPROMPT="${return_status}"
	if [ $COMMAND_TIMER ]; then
		local diff=$((SECONDS + $(date "+%N") / 1000000000.0 - COMMAND_TIMER))
		diff=`printf "%.2f" $diff`
		if [[ $diff > 1 ]]; then
			RPROMPT=$RPROMPT"$PINK${diff}s %{$reset_color%}"
		fi
		unset COMMAND_TIMER
	fi

	PROMPT2='$BLUE($PINK%_$BLUE)$FINISH%{$reset_color%}'
	PROMPT3='$PINK Select:'
}
# f]]

# alias
safe_source $HOME/.zsh/alias.zsh
alias -s pdf jpg png gif html mhtml=xdg-open
alias -s djvu=djview4
alias -s obj=meshlab
alias -s wmv mkv mp4 mp3 avi rm rmvb flv=mpv
alias -s xls xlsx doc docx ppt pptx=libreoffice

# Basic
setopt autocd				# cd without 'cd'
setopt braceccl				# ls {a-e.1}
unsetopt hup				# don't close background program when exiting shell
stty stop undef
setopt transient_rprompt      # clear rprompt
setopt NO_FLOW_CONTROL		# disable Ctrl+s
setopt NOTIFY				# show bg jobs status immediately
limit coredumpsize 0		# disable core dumps
WORDCHARS='*?[]~!#$%^(){}<>'
setopt EXTENDED_GLOB
#unsetopt CASE_GLOB
setopt correctall
zmodload zsh/mathfunc 2>/dev/null
autoload -U zsh-mime-setup
zsh-mime-setup

autoload -U url-quote-magic		# auto add quote on url
zle -N self-insert url-quote-magic

# History
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_FUNCTIONS
setopt SHARE_HISTORY
unsetopt EXTENDED_HISTORY
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=100000
export SAVEHIST=80000
alias nohistory='unset HISTFILE'

# key binding f[[
bindkey -e
autoload edit-command-line
zle -N edit-command-line
bindkey -M viins '^v' edit-command-line
bindkey '^h' backward-char
bindkey '^l' forward-char
bindkey '^b' backward-word
bindkey '^f' forward-word
bindkey '^w' backward-delete-word
bindkey ' ' magic-space		# history expansion + space
autoload zkbd
[[ -f $HOME/.zsh/zkbd/$TERM ]] && source $HOME/.zsh/zkbd/$TERM || {
	echo "Did not find '$HOME/.zkbd'. You might be using incomplete dotfiles."
	zkbd
}
[[ -n ${key[Backspace]} ]] && bindkey "${key[Backspace]}" backward-delete-char
[[ -n ${key[Insert]} ]] && bindkey "${key[Insert]}" overwrite-mode
[[ -n ${key[Home]} ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n ${key[PageUp]} ]] && bindkey "${key[PageUp]}" history-substring-search-up
[[ -n ${key[PageDown]} ]] && bindkey "${key[PageDown]}" history-substring-search-down
[[ -n ${key[Delete]} ]] && bindkey "${key[Delete]}" delete-char
[[ -n ${key[End]} ]] && bindkey "${key[End]}" end-of-line
[[ -n ${key[Up]} ]] && bindkey "${key[Up]}" up-line-or-search
[[ -n ${key[Down]} ]] && bindkey "${key[Down]}" down-line-or-search
[[ -n ${key[Left]} ]] && bindkey "${key[Left]}" backward-char
[[ -n ${key[Right]} ]] && bindkey "${key[Right]}" forward-char


# Move along shell argument, aka 'Big Word' (defined as separate by spaces)
zsh-word-movement () {
  # by lilydjwg, http://lilydjwg.is-programmer.com/posts/41712
  local -a word_functions
  local f

  word_functions=(backward-kill-word backward-word
    capitalize-word down-case-word
    forward-word kill-word
    transpose-words up-case-word)

  if ! zle -l $word_functions[1]; then
    for f in $word_functions; do
      autoload -Uz $f-match
      zle -N zsh-$f $f-match
    done
  fi
  # set the style to shell
  zstyle ':zle:zsh-*' word-style shell
}
zsh-word-movement
unfunction zsh-word-movement
bindkey "\eB" zsh-backward-word
bindkey "\eF" zsh-forward-word
bindkey "\eW" zsh-backward-kill-word

# add sudo
sudo-command-line() {
	[[ -z $BUFFER ]] && zle up-history
	[[ $BUFFER != sudo\ * ]] && BUFFER="sudo $BUFFER"
	zle end-of-line
}
zle -N sudo-command-line
bindkey "${key[F2]}" sudo-command-line

# f]]

# Complete f[[
autoload -U compinit
compinit
zmodload zsh/complist
bindkey -M menuselect '^@' accept-and-menu-complete
setopt AUTO_LIST
setopt AUTO_MENU
setopt MENU_COMPLETE
setopt complete_in_word   # complete /v/c/a/p
setopt no_nomatch		  # enhanced bash wildcard completion
setopt magic_equal_subst
setopt noautoremoveslash
setopt null_glob

# alias completion
compdef cpv=cp
compdef rsync=scp
compdef telnet=ssh
compdef st=ssh

# specific filetype
_pic() { _files -g '*.(jpg|png|bmp|gif|ppm|pbm|jpeg|xcf|ico)(-.)' }
compdef _pic gimp
compdef _pic feh

zstyle ':completion:*:cd:*' ignore-parents parent pwd
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
which dircolors NN && eval $(dircolors -b)
export ZLSCOLORS="${LS_COLORS}"
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
export LS_COLORS="$LS_COLORS*.f4v=01;35:*.pdf=01;35:*.djvu=01;35:"		# add custom ls_color

# Fix case and typo
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Grouping Completion
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:descriptions' format $'\e[01;33m -- %d --\e[0m'

# huge list
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:default' menu 'select=0'
# Completing order
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' completer _complete _prefix _user_expand _correct _prefix _match
# Separate man page sections.
zstyle ':completion:*:manuals' separate-sections true

# kill completion
compdef pkill=kill
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:*:killall:*:processes-names' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:*:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps a -u $USER -o pid,tname,state,command '

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
bindkey '^X^P' tmux-pane-words-prefix
bindkey '^X^O' tmux-pane-words-anywhere
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer tmux_buffer_completion
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'

# file completion ignore
zstyle ':completion:*:*:*vim:*:*files' ignored-patterns '*.(avi|mkv|rmvb|pyc|wmv|mp3|pdf|doc|docx|jpg|png|bmp|gif|npy|bin|o)'
zstyle ':completion:*:*:cat:*:*files' ignored-patterns '*.(avi|mkv|rmvb|pyc|wmv|mp3|pdf|doc|docx|jpg|png|bmp|gif|npy|bin|o)'

# f]]


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

# .... path completion
user-complete(){
	if [[ -z $BUFFER ]]; then
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
}
zle -N user-complete
bindkey "\t" user-complete
autoload compinstall

# Custom Return
path_parse(){
	if [[ $BUFFER = "." ]]; then
		BUFFER="cd ../"
		return
	elif [[ $BUFFER =~ ".*\.\.\..*" ]] ;then	# expand ...
		BUFFER=`echo "$BUFFER" |sed 's/\.\.\./\.\.\/\.\./g'`
		path_parse
	elif [[ $BUFFER =~ "^\.\..*" ]]; then		# auto add cd to the beginning of ...
		if [[ -d `echo "$BUFFER" |sed 's/\\\ /\ /g; s/l$//g; s/ls$//g'` ]]; then
			BUFFER=`echo "$BUFFER" |sed 's/^/cd\ /g'`
			path_parse
		fi
		zle accept-line
	elif [[ $BUFFER =~ "^cd .*/ls*$" ]] ; then	# auto fix ls typo
		BUFFER=`echo "$BUFFER" |sed 's/l[^\/]*$/;ls/g' `
		zle end-of-line
	fi
}

# commands which should always be executed in background
special_command(){
	bg_list=(mupdf geeqie libreoffice word powerpoint evince matlab mathematica llpp foxitreader)
	local cmd=`echo $BUFFER | awk '{print $1}'`
	# command running in background
	in_array $cmd "${bg_list[@]}" && BUFFER=`echo $BUFFER |sed 's/\s\+2>\/dev\/null//g; s/[&]*\s*$/\ 2>\/dev\/null\ \&/g'`
	## command ending with alert
	#alert_list=(mencoder aria2c axel)
	#in_array $cmd "${alert_list[@]}" && BUFFER="$BUFFER ; notify-send \"$cmd finished! \""
}

user-ret(){
	path_parse
	special_command
	#RPS1= zle reset-prompt	# reset rps1 of last command
	#BUFFER=${BUFFER/mms:\/\/officetv/rtsp:\/\/officetv}		# mms IPTV urls in China are actually in rtsp!
	zle accept-line
}
zle -N user-ret
bindkey "\r" user-ret

# command not found
function command_not_found_handler() {
	local command="$1"
	local fortuneList; fortuneList=("tang300" "song100" "chucknorris" "love")
	# avoid recursive command-not-found when /usr/bin/ is mistakenly lost in PATH
	[ -x /usr/bin/fortune ] && [ -x /usr/bin/cowthink ] && {
		/usr/bin/fortune ${fortuneList[@]} | /usr/bin/cowthink -W 70
	}
	[ -n "$command" ] && [ -x /usr/bin/pkgfile ] && {
		echo -e "searching for \"$command\" in repos..."
		local pkgs="$(/usr/bin/pkgfile -b -v -- "$command")"
		if [ ! -z "$pkgs" ]; then
			echo -e "\"$command\" may be found in the following packages:\n\n${pkgs}\n"
		fi
	}
	return 1
}

# plugins
safe_source $HOME/.zsh/Pinyin-Completion/shell/pinyin-comp.zsh
safe_export_path $HOME/.zsh/Pinyin-Completion/bin

safe_source $HOME/.zsh/extract.zsh
# the next two have to be this order
safe_source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
safe_source $HOME/.zsh/history-substring-search.zsh
safe_source $HOME/.zsh/cdnav.zsh
safe_source $HOME/.zsh/transfer.sh
safe_source "$HOME/.rvm/scripts/rvm"		# Load RVM into a shell session *as a function*
HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS="I"			# sensitive search

if [ $commands[fasd] ]; then
	fasd_cache="$HOME/.vimtmp/fasd-cache"
	eval "$(fasd --init posix-alias zsh-hook zsh-wcomp zsh-wcomp-install)"
	alias o='f -e xdg-open'
	alias fv='f -e vim'
	alias j='fasd_cd -d'
	alias jj='fasd_cd -d -i'	# interactive
	unalias s
	unalias d
	bindkey '^X^O' fasd-complete
fi

safe_source $HOME/.zshrc.local

# dedup paths
which awk NN && {
	LD_LIBRARY_PATH=$(echo -n "$LD_LIBRARY_PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++' | head -c-1)
	PATH=$(echo -n "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++' | head -c-1)
	CPATH=$(echo -n "$CPATH" | awk -v RS=':' -v ORS=":" '!a[$1]++' | head -c-1)
	PKG_CONFIG_PATH=$(echo -n "$PKG_CONFIG_PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++' | head -c-1)
}
