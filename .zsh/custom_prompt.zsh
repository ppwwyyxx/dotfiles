setopt transient_rprompt      # clear rprompt
_PROMPT_FINISH="%{$terminfo[sgr0]%}"

function preexec() {
  if [[ $TERM == "xterm-termite" ]]; then
    # set win title to the command
    echo -ne "\033]0;$1 \007"
  fi
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

  if [[ -z $MY_PROMPT_USER ]] && [[ -z $MY_PROMPT_HOST ]]; then
    PROMPT_PART=""
  else
    PROMPT_PART="$GREEN [%{%F{171}%}$MY_PROMPT_USER@%{%F{219}%}$MY_PROMPT_HOST$GREEN]"
  fi

  # to calculate length
  local git_status="$(gitprompt)"
  local prompt_nodir="-----$(date +%H:%M)---$git_status$PROMPT_PART"
  local zero='%([BSUbfksu]|([FB]|){*})'	# used to calculate length withou control sequence
  local part_length=${#${(S%%)prompt_nodir//$~zero/}}
  local pwdlen=$((${COLUMNS} - $part_length - 3))
  local START_BOLD=$'\e[1m'		# bold on
  local END_BOLD=$'\e[22m'		# bold off

  local INDICATOR="\$"
  #local INDICATOR="❱"
  [[ -n "$VIRTUAL_ENV" ]] && VIRTUAL="(`basename $VIRTUAL_ENV`)"
  # my magic prompt
  export PROMPT="%{$START_BOLD%}%{$CYAN%}╭─${VIRTUAL}${PROMPT_PART}\
$TIMECOLOR [%D{%H:%M}] \
$YELLOWGREEN%$pwdlen<...<%~%<< \
%{$reset_color%}$git_status%{$CYAN%}
%{$START_BOLD%}╰🐻%{$reset_color%}%{$CYAN%}%(?..%{$fg[red]%})$INDICATOR%{$reset_color%}"
  if [[ $TERM == "xterm-termite" ]]; then
    # set win title to pwd
    echo -ne "\033]0;$(pwd) \007"
  fi

  local return_status="%{$fg[red]%}%(?..⏎)%{$reset_color%}"
  RPROMPT="${return_status}"
  # print time for commands that run > 1s
  if [ $COMMAND_TIMER ]; then
    local diff=$((SECONDS + $(date "+%N") / 1000000000.0 - COMMAND_TIMER))
    diff=`printf "%.2f" $diff`
    if [[ $diff > 1 ]]; then
      RPROMPT=$RPROMPT"$PINK${diff}s %{$reset_color%}"
    fi
    unset COMMAND_TIMER
  fi

  PROMPT3="$PINK Select:"
}

if [[ $commands[awk] ]]; then
  # ZSH_GIT_PROMPT_NO_ASYNC=1
  ZSH_GIT_PROMPT_FORCE_BLANK=1
  ZSH_THEME_GIT_PROMPT_UNSTAGED="%{$fg_bold[blue]%}+"
  ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}✓"
  znap source woefe/git-prompt.zsh git-prompt.zsh
fi
