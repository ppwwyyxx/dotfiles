# use pinyin-comp to perform completion based upon pinyin acronym
function _pinyin_comp()
{
    # script print one candidate per line
    # this looks weird, bug IFS='\n' does not work in interactive shell
    local IFS=$'\n'
    if [ "$words[1]" = "cd" ] ; then
        reply=($(pinyin-comp x-d $*) $*)
    else
        reply=($(pinyin-comp 0 $*) $*)
    fi
}

# force rehash when command not found
_force_rehash_pinyin_comp() {
    (( CURRENT == 1 )) && rehash
    return 1 # Because we did not really complete anything
}

# pinyin-comp is performed as one part of user-expand
zstyle ':completion:*' user-expand _pinyin_comp

# omit original and all expansions when showing the result of user-expand
zstyle ':completion:*:user-expand:*' tag-order '!original all-expansions'

# make use-expand perform as last, when needed
# Adding this breaks something. It's defined in zshrc already.
#zstyle ':completion:*' completer _oldlist _expand _force_rehash_pinyin_comp _complete #_match _user_expand

function _pinyin_comp_completer() {
    local IFS=$'\n'
    local expl
    if [ "$words[1]" = "cd" ] ; then
        reply=($(pinyin-comp x-d $PREFIX))
    else
        reply=($(pinyin-comp 0 $PREFIX))
    fi
    _wanted values expl "Pinyin Completion" compadd -U -a reply
}
zle -C pinyin-comp complete-word _generic
zstyle ':completion:pinyin-comp:*' completer _pinyin_comp_completer
# If other completer succeeds, pinyin will not trigger. This key will force trigger.
bindkey '^X^Z' pinyin-comp
