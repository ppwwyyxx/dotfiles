# https://gist.github.com/xiaq/a318acccfdcf6b477ae1
typeset -gA cdnav
cdnav=(histno 1 listing 0 maxlist 5)
typeset -ga cdnav_hist
cdnav_hist=( "$PWD" )

cdnav::index() {
    print -- "$cdnav_hist[$1]"
}

cdnav::push() {
    cdnav_hist[$((++cdnav[histno])),-1]=($@)
}

# Index the cdnav_hist array relative to the top.
cdnav::top() {
    local offset=$1
    cdnav::index "$((cdnav[histno] + offset))"
}

cdnav::goto() {
    cdnav[histno]=$1
    cd "`cdnav::top`"
}


cdnav::precmd() {
    cdnav[listing]=0
}

cdnav::chpwd() {
    local offset
    for offset in 0 -1 1; do
        if [[ $PWD == "`cdnav::top $offset`" ]]; then
            : $(( cdnav[histno] += offset ))
            return
        fi
    done
    cdnav::push $PWD
}

cdnav::step() {
    local offset=$1
    local print
    if [[ $2 == zle ]]; then
        print=(zle -M --)
    else
        print=(print --)
    fi

    if [[ -z "`cdnav::top $offset`" ]]; then
        $print "Hit end of history..."
        return 1
    fi

    : $(( cdnav[histno]+=offset ))
    if cd "`cdnav::top`"; then
        [[ $2 == zle ]] && {
            zle reset-prompt
            cdnav::list_zle
        }
    else
        $print "Things went wrong with cd, oh no."
        return 2
    fi
}

cdnav::back() { cdnav::step -1 }
cdnav::forth() { cdnav::step +1 }
cdnav::back_zle() { cdnav::step -1 zle }
cdnav::forth_zle() { cdnav::step +1 zle }

cdnav::up_zle() {
    cd ..
    zle reset-prompt
    cdnav::list_zle
}

cdnav::go() {
    local index=$1
    if [[ -z "`cdnav::index $index`" ]]; then
        echo Too far away...
        return 1
    fi
    cdnav::goto $index
}

cdnav::goto_zle() {
    cdnav::go $NUMERIC
}

cdnav::list() {
    cdnav[listing]=1
    if [[ "$*" == '-a' ]]; then
        lower=1
    else
        local maxlist
        : ${maxlist:=cdnav[maxlist]}
        lower=$(( cdnav[histno] - cdnav[maxlist] + 1 ))
        if [[ $lower -lt 1 ]]; then
            lower=1
        fi
    fi
    for (( i=$lower; i<=$#cdnav_hist; i++ )); do
        if [[ $i == $cdnav[histno] ]]; then
            printf "☛ "
        else
            printf "  "
        fi
        printf "%d %s\n" $i $cdnav_hist[$i]
    done
}

cdnav::list_zle() {
    cdnav[listing]=1
    zle -M -- "$(maxlist=$NUMERIC cdnav::list)"
}

cdnav::toggle_list_zle() {
    if [[ $cdnav[listing] == 0 ]]; then
        cdnav::list_zle
    else
        cdnav[listing]=0
        zle -M ''
    fi
}

zle -N cdnav::back_zle
zle -N cdnav::forth_zle
zle -N cdnav::up_zle
zle -N cdnav::toggle_list_zle

precmd_functions+=(cdnav::precmd)
chpwd_functions+=(cdnav::chpwd)

# might not work on all terminals; modify accordingly.
bindkey '\e[1;3D' cdnav::back_zle # alt-left
bindkey '\e[1;3C' cdnav::forth_zle # alt-right
bindkey '\e[1;3A' cdnav::up_zle # alt-up
bindkey '\ei' cdnav::toggle_list_zle # alt-i
