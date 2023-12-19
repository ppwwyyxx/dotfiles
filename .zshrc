# !history expansion; !:1, !$, !:1-3, !:* take word from last command
# !-4 fourth last command; !ls, most recent command with ls; !# current buffer
# ^foo^bar^:G  global substitution on last command, !?str?:s^foo^bar^:G, on last command containing str
#http://lilydjwg.is-programmer.com/2012/3/19/thress-zsh-line-editor-tips.32549.html
# To profile:
#   ZSH_PROF=1 zsh -i -c exit
#   /usr/bin/time zsh -c 'for i in {1..50}; do zsh -i -c exit; done'
if [[ -n $ZSH_PROF ]]; then zmodload zsh/zprof; fi

if [[ $(uname) == "Darwin" ]]; then
	export _CFG_ON_MAC=1
	source /etc/profile
fi
if [[ -n $SSH_CLIENT || -n $SSH_TTY || -n $SSH_CONNECTION ]]; then
  export _CFG_ON_SSH=1
fi



# https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

function safe_export_path() { [[ -d $1 ]] && export PATH=$1:$PATH }
function safe_source() { [[ -s $1 ]] && source $1 }

# Fast path for cache that doesn't persist across restarts:
if [[ -d /dev/shm ]]; then _MY_ZSH_CACHE=/dev/shm/zsh-cache-$USER; else _MY_ZSH_CACHE=/tmp/zsh-cache-$USER; fi
local __OLD_XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
zmodload -F zsh/files b:zf_mkdir
zf_mkdir -p $_MY_ZSH_CACHE

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
safe_source "$_MY_ZSH_CACHE/p10k-instant-prompt-${(%):-%n}.zsh"

[[ -d $HOME/.zsh/Completion ]] && fpath=($HOME/.zsh/Completion $fpath)
[[ -d $HOME/.zsh/functions ]] && fpath=($HOME/.zsh/functions $fpath)

_ZSH_SNAP_BASE=$HOME/.zsh/snap/
typeset -gH _comp_dumpfile=$_MY_ZSH_CACHE/snap-compdump
[[ -f $_ZSH_SNAP_BASE/zsh-snap/znap.zsh ]] ||
    git clone --depth 1 -- \
        https://github.com/marlonrichert/zsh-snap.git $_ZSH_SNAP_BASE/zsh-snap
source $_ZSH_SNAP_BASE/zsh-snap/znap.zsh

# ENV f[[
export TERMINFO=${TERMINFO:-$HOME/.terminfo}
export LANG=en_US.UTF-8
# export SSH_ASKPASS=
# neovim#2048 suggests: infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti; tic $TERM.ti

safe_source $HOME/.profile

if [[ -n $_CFG_ON_MAC ]]; then
  safe_export_path /opt/homebrew/bin
  safe_export_path /opt/homebrew/sbin
  safe_export_path /opt/homebrew/opt/coreutils/libexec/gnubin
  safe_export_path /opt/homebrew/opt/findutils/libexec/gnubin
  safe_export_path /opt/homebrew/opt/gawk/libexec/gnubin
  safe_export_path /opt/homebrew/opt/grep/libexec/gnubin
  safe_export_path /opt/homebrew/opt/gnu-sed/libexec/gnubin
  safe_export_path /opt/homebrew/opt/gnu-tar/libexec/gnubin
  safe_export_path $(brew --prefix python)/libexec/bin
  export MANPATH=/opt/homebrew/opt/coreutils/libexec/gnuman:$MANPATH
  #safe_export_path /usr/local/opt/openssh/bin
fi
safe_export_path $HOME/bin
safe_export_path $HOME/.local/bin
safe_export_path $HOME/.zsh/bin

# local prefix
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$HOME/.local/lib
export LIBRARY_PATH=${LIBRARY_PATH+$LIBRARY_PATH:}$HOME/.local/lib
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$HOME/.local/lib/pkgconfig

# override tmux master key under ssh
if [[ -n "$TMUX" ]] && [[ -n $_CFG_ON_SSH ]]; then
	tmux set status-bg cyan
	tmux unbind C-q
	tmux set prefix C-a
	tmux bind C-a send-prefix
fi

safe_source ~/.zsh/config_libs.zsh

export MAKEFLAGS="-j8"
export CXXFLAGS="-Wall -Wextra"

(( ${+commands[nvim]} )) && export EDITOR=nvim || export EDITOR=vim
export PAGER="/usr/bin/less -r"
cdpath=(~)
# f]]

## Colors
autoload colors zsh/terminfo
colors
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
  eval _$color='%{$terminfo[bold]$fg[${(L)color}]%}'
  eval $color='$fg[${(L)color}]'
done

if [[ $commands[vivid] ]]; then
  export LS_COLORS="$(vivid generate my-solarized)"
elif [[ $commands[dircolors] ]]; then
  eval $(dircolors -b)
  export LS_COLORS=${LS_COLORS:gs/=01;/=00;/}  # Disable bold colors. Don't look nice on CJK.
fi

## PROMPT f[[
autoload -U promptinit
promptinit

if [[ -n $_CFG_ON_SSH ]]; then
  export SUDO_PROMPT=$'[\e[31;5msudo\e[m] password for \e[34;1m%p\e[m: (meow~~) '
else
  export SUDO_PROMPT=$'[\e[31;5mYou are on %H!\e[m] password for \e[34;1m%p\e[m on\e[0;31m %H\e[m: '
fi

# Can be customized in local configs.
typeset -g MY_PROMPT_HOST=$HOST
typeset -g MY_PROMPT_USER=$USER
if [[ $USER == "wyx" ]] || [[ $USER == yuxin* ]]; then
  typeset -g MY_PROMPT_USER=
fi
if [[ $HOST == Keep* ]] && [[ -z $_CFG_ON_SSH ]]; then
  typeset -g MY_PROMPT_HOST=
fi

# Move p10k cache to tmpfs. It has frequent IO.
export XDG_CACHE_HOME=$_MY_ZSH_CACHE
# But don't move gitstatus cache.
export GITSTATUS_CACHE_DIR=$HOME/.cache/gitstatus
znap source romkatv/powerlevel10k
safe_source ~/.zsh/p10k.zsh
export XDG_CACHE_HOME=$__OLD_XDG_CACHE_HOME
#safe_source ~/.zsh/custom_prompt.zsh

function _my_update_title_cmd() { echo -ne "\e]0;${1%% *}\a" }
# %3~ to show last 3 components of the path
function _my_update_title_pwd() { echo -ne "\e]0;${(%):-"%3~"}\a" }
autoload -Uz add-zsh-hook
add-zsh-hook preexec _my_update_title_cmd
add-zsh-hook precmd _my_update_title_pwd
# f]]

# Basic
setopt autocd				# cd without 'cd'
setopt braceccl				# ls {a-e.1}
unsetopt hup				# don't close background program when exiting shell
unsetopt beep       # disable all builtin beep
stty stop undef 2>/dev/null || true
setopt NO_FLOW_CONTROL		# disable Ctrl+s
setopt NOTIFY				# show bg jobs status immediately
#limit coredumpsize 10000  # `ulimit -c unlimited` to revert this
WORDCHARS='*?[]~!#$%^(){}<>-'
setopt EXTENDED_GLOB
#unsetopt CASE_GLOB
unsetopt correctall
setopt correct
zmodload zsh/mathfunc 2>/dev/null

autoload -Uz url-quote-magic		# auto add quote on url
zle -N self-insert url-quote-magic
autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic


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

# key binding
safe_source $HOME/.zsh/bindings.zsh

# Complete f[[
autoload -Uz compinit && compinit
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
compdef _pip pip2
compdef _pip pip3

zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' verbose yes
zstyle ':completion:*' menu select
zstyle ':completion:*:*:default' force-list always
zstyle ':completion:*' select-prompt '%SSelect:  lines: %L  matches: %M  [%p]'
zstyle ':completion:*:match:*' original only
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:predict:*' completer _complete
zstyle ':completion:*' users root $USER  # Fix lag if there are too many users.

# Path Completion
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-shlashes 'yes'
zstyle ':completion::complete:*' '\\'

# Colorful Completion
export ZLSCOLORS="${LS_COLORS}"
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

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
function tmux_buffer_completion() {
  local expl
  local -a w npane
  npane=$(tmux list-panes |tail -n 1 |sed 's/:.*//g')
  if [[ -z "$TMUX_PANE" ]]; then
    _message "not running inside tmux!"
    return 1
  fi
  for i in $(seq 0 $npane); do
    # u: unique
    # grep: remove words length <=3
    w=$w${(u)=$(tmux capture-pane -t $i \; show-buffer \; delete-buffer | \
      sed 's/ /\n/g' | \grep -E '([a-zA-Z0-9_].*){4}'  | tr '\n' ' ')}
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

# Custom path expansion f[[
# .... path completion
__expand_dots() {
  # Add / after sequence of ......, if not there
  local cur=`echo "$1" | sed 's/\( \|^\)\(\.\.\.\.*\)\([^\/\.]\|$\)/\1\2\/\3/g'`
  while true; do  # loop to expand ...
    local new=`echo $cur | sed 's/\.\.\./\.\.\/\.\./g'`
    if [[ $new == $cur ]]; then
      break
    fi
    cur=$new
  done
  BUFFER=$cur
}
__user_complete(){
  if [[ -z $BUFFER ]]; then
    return
  fi
  if [[ $BUFFER =~ "^\.\.\.*$" ]]; then
    __expand_dots "$BUFFER"
    zle end-of-line
    return
  elif [[ $BUFFER =~ ".* \.\.\..*$" ]] ;then
    __expand_dots "$BUFFER"
    zle end-of-line
    return
  fi
  zle expand-or-complete
}
zle -N __user_complete
bindkey "\t" __user_complete
autoload compinstall

# Custom Return
__path_parse(){
  if [[ $BUFFER == "." ]]; then
    BUFFER="cd ../"
    return
  elif [[ $BUFFER =~ "^\.\.\.?$" ]] ;then  # expand ...
    __expand_dots "cd $BUFFER"
    return
  elif [[ $BUFFER =~ ".* \.\.\..*" ]] ;then  # expand ...
    __expand_dots "$BUFFER"
    return
  elif [[ $BUFFER =~ "^\.\..*" ]]; then    # auto add cd to the beginning of ...
    if [[ -d `echo "$BUFFER" |sed 's/\\\ /\ /g; s/l$//g; s/ls$//g'` ]]; then
      BUFFER=`echo "$BUFFER" |sed 's/^/cd\ /g'`
      __path_parse
    fi
    zle accept-line
  elif [[ $BUFFER =~ "^cd .*/ls*$" ]] ; then  # auto fix ls typo
    BUFFER=`echo "$BUFFER" |sed 's/l[^\/]*$/;ls/g' `
    zle end-of-line
  fi
}

# commands which should always be executed in background
special_command(){
	bg_list=(mupdf geeqie libreoffice word powerpoint evince matlab mathematica llpp foxitreader gwenview)
	local cmd=`echo $BUFFER | awk '{print $1}'`
	# command running in background
	in_array $cmd "${bg_list[@]}" && BUFFER=`echo $BUFFER |sed 's/\s\+2>\/dev\/null//g; s/[&]*\s*$/\ 2>\/dev\/null\ \&/g'`
}

__user_ret(){
  if [[ $BUFFER == "j" && $commands[fzf] ]]; then
    BUFFER=""
    zle fzf-cd-widget
    return
  fi
  __path_parse
  special_command
  zle accept-line
}
zle -N __user_ret
bindkey "\r" __user_ret
# f]]

# Plugins f[[
autoload -Uz jump-target
zle -N jump-target
bindkey "^J" jump-target

safe_source $HOME/.zsh/cdnav.zsh   # alt-up/left/right/i

safe_source $HOME/.zsh/Pinyin-Completion/shell/pinyin-comp.zsh
safe_export_path $HOME/.zsh/Pinyin-Completion/bin
znap source ohmyzsh/ohmyzsh plugins/{extract,transfer}
safe_source $HOME/.zsh/fzf-fasd.zsh
# znap source ohmyzsh/ohmyzsh plugins/ssh-agent

### The next two plugins have to be this order
HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS="I"			# sensitive search
znap source zsh-users/zsh-history-substring-search   # PageUp/Dn
znap source zsh-users/zsh-syntax-highlighting
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=red,standout

# f]]

safe_source $HOME/.zsh/alias.zsh  # aliases
safe_source $HOME/.zshrc.local

## dedup paths
if [[ $commands[awk] ]]; then
  LD_LIBRARY_PATH=$(echo -n "$LD_LIBRARY_PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++' | head -c-1)
  PATH=$(echo -n "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++' | head -c-1)
  CPATH=$(echo -n "$CPATH" | awk -v RS=':' -v ORS=":" '!a[$1]++' | head -c-1)
  PKG_CONFIG_PATH=$(echo -n "$PKG_CONFIG_PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++' | head -c-1)
fi


if [[ -n $ZSH_PROF ]]; then
  zprof
  # Show zsh latency of each cmd.
  precmd () { start=$(date '+%s.%N') }
  zle-line-init () { PREDISPLAY="[$(printf %.3f $(( $(date '+%s.%N') - $start )))] " }
  zle -N zle-line-init
fi
