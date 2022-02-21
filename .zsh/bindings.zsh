bindkey -e
bindkey -r "^G"  # C-G was bind to break in emacs
bindkey -r "^[c"
bindkey -r "^[l"
bindkey -r "^[u"
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
	echo "Did not find '$HOME/.zsh/zkbd/$TERM'."
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
