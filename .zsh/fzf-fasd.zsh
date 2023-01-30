# Misc useful configurations related to fzf & fasd.

source ${${(%):-%x}:P:h}/my_fasd.zsh

if [[ -z $commands[fzf] ]]; then
  return
fi

# Configure default fzf behavior:
if [[ $commands[fd] ]]; then
  export FZF_DEFAULT_COMMAND='fd --type f -c always'
fi
export FZF_DEFAULT_OPTS="--ansi  --bind 'ctrl-y:execute-silent(echo -n {2..} | yank)+abort' "
export FZF_TMUX_HEIGHT="50%"

# Configure the official fzf plugin https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh
export FZF_CTRL_T_COMMAND="sort $_FASD_DATA -t '|' -k 2 -n -r | cut -d '|' -f 1"
export FZF_ALT_C_COMMAND="=grep '/|' $_FASD_DATA | sort -t '|' -k 2 -n -r | cut -d '|' -f 1"
export FZF_CTRL_R_OPTS="
  --preview 'echo {}' --preview-window up:3:hidden:wrap
  --bind 'ctrl-/:toggle-preview'
"
znap source ohmyzsh/ohmyzsh plugins/fzf    # Support Ctrl-R to complete history.
# Remove unused Alt-C and Ctrl-T bindings from the plugin:
bindkey -r '\ec'
bindkey -r '^T'

# Like the official fzf-file-widget, but support completing partial filenames.
fzf-file-widget() {
  local word="${LBUFFER/* /}"  # Find current word under cursor.
  if [[ -n "$word" ]]; then
    FZF_CTRL_T_OPTS="-q \"$word\""  # Pre-fill fzf with the query word.
    local output=$(__fsel)
    if [[ -n $output ]]; then  # Replace current word with selection if anything is selected.
      LBUFFER="${LBUFFER:0:$(( ${#LBUFFER} - ${#word} ))}$output"
    fi
  else
    FZF_CTRL_T_OPTS=""
    LBUFFER="${LBUFFER}$(__fsel)"
  fi
  local ret=$?
  zle reset-prompt
  return $ret
}
bindkey '^X^F' fzf-file-widget  # Complete recent files and directories.


# Like the official fzf-cd-widget, but support completing partial filenames.
fzf-cd-widget() {
  local word="${LBUFFER/* /}"  # Find current word under cursor.
  local FZF_CD_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore,tab:down,btab:up ${FZF_DEFAULT_OPTS-}"
  if [[ -n "$word" ]]; then
    # Pre-fill fzf with the query word.
    FZF_CD_OPTS="$FZF_CD_OPTS -q \"$word\""
  fi
  local output=$(eval "$FZF_ALT_C_COMMAND" | FZF_DEFAULT_OPTS="$FZF_CD_OPTS" fzf +m)
  if [[ -n "$output" ]]; then
    BUFFER="builtin cd -- ${(q)output}"
    zle accept-line
    unset output word
    zle reset-prompt
  else
    zle redisplay
  fi
}

# Define tab completion for the "j" command to use fzf-cd-widget.
# Ref: https://github.com/wookayin/fzf-fasd/blob/master/fzf-fasd.plugin.zsh
__fzf_fasd_zsh_completion() {
  local args=(${(z)LBUFFER})
  local cmd=${args[1]}
  if [[ "$cmd" == "j" ]]; then  # Triggered only at the command 'j'
    zle fzf-cd-widget
  else  # Fallback to default.
    zle ${__fzf_fasd_default_completion:-expand-or-complete}
  fi
}
# Save the current completer.
[ -z "$__fzf_fasd_default_completion" ] && {
  binding=$(bindkey '^I')
  [[ $binding =~ 'undefined-key' ]] || __fzf_fasd_default_completion=$binding[(s: :w)2]
  unset binding
}
zle      -N  __fzf_fasd_zsh_completion
bindkey '^I' __fzf_fasd_zsh_completion

# Search flags from history.
if [ $commands[rg] ]; then
  znap source ppwwyyxx/fzf-complete-flags fzf-complete-flags.zsh
  bindkey -r '^Q'
  bindkey '^X^R' fzf-flag-widget
fi


# Advanced customization of fzf options via _fzf_comprun function
# - The first argument to the function is the name of the command.
# - You should make sure to pass the rest of the arguments to fzf.
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf --preview 'tree -C {} | head -200'   "$@" ;;
    export|unset) fzf --preview "eval 'echo \$'{}"         "$@" ;;
    *)            fzf --preview 'bat -n --color=always {}' "$@" ;;
  esac
}
