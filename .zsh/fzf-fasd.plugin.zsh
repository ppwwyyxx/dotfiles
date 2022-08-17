# fzf-fasd integration
# author: @wookayin
# MIT License (c) 2017-2020
# vim: set ft=zsh ts=2 sts=2 ts=2:
# https://github.com/wookayin/fzf-fasd/blob/master/fzf-fasd.plugin.zsh
# ppwwyyxx: replace z by j


# fasd+fzf integration (ZSH only)
__fzf_fasd_zsh_completion() {
  local args cmd slug selected

  args=(${(z)LBUFFER})
  cmd=${args[1]}

  # triggered only at the command 'j'; fallback to default
  if [[ "$cmd" != "j" ]]; then
    zle ${__fzf_fasd_default_completion:-expand-or-complete}
    return
  fi

  if [[ "${#args}" -gt 1 ]]; then
    eval "slug=${args[-1]}"
  fi

  # generate completion list from fasd
  local matches_count
  matches_count=$(__fzf_fasd_generate_matches "$slug" | head | wc -l)
  if [[ "$matches_count" -gt 1 ]]; then
    # >1 results, invoke fzf
    selected=$(__fzf_fasd_generate_matches "$slug" \
        | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS $FZF_FASD_OPTS" \
          fzf --query="$slug" --reverse --bind 'shift-tab:up,tab:down'
    )
  elif [[ "$matches_count" -eq 1 ]]; then
    # 1 result, just complete it
    selected=$(__fzf_fasd_generate_matches "$slug")
  else;
    # no result
    return
  fi
  #echo [DEBUG] $selected $matches_count

  # return completion result with $selected
  if [[ -n "$selected" ]]; then
    selected=$(printf %q "$selected")
    if [[ "$selected" != */ ]]; then
      selected="${selected}/"
    fi
    LBUFFER="$cmd $selected"
  fi

  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
}

__fzf_fasd_generate_matches() {
  # currently only 'z' (fasd -d) is supported; list all dirs (without score)
  # -R: make entries with higher score comes earlier
  fasd -d -l -R "$@"
}

[ -z "$__fzf_fasd_default_completion" ] && {
  binding=$(bindkey '^I')
  [[ $binding =~ 'undefined-key' ]] || __fzf_fasd_default_completion=$binding[(s: :w)2]
  unset binding
}

zle      -N  __fzf_fasd_zsh_completion
bindkey '^I' __fzf_fasd_zsh_completion
