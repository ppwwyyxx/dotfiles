#!/usr/bin/env zsh
# A faster version of https://github.com/clvv/fasd/blob/master/fasd
# (fasd queries are extremely slow: it tries to check existence of every file in the database!
#  Instead we just parse the database directly.)

_FASD_DATA=$HOME/.fasd
_FASD_SHIFT="sudo busybox"
_FASD_IGNORE="ls echo"
_FASD_MAX=${_FASD_MAX:-4000} 
_FASD_SINK=$HOME/.cache/fasd.log

function _fasd_sanitize() {
  # https://github.com/clvv/fasd/blob/90b531a5daaa545c74c7d98974b54cbdb92659fc/fasd#L290-L292
  printf %s\\n "$*" | sed 's/\([^\]\)$( *[^ ]* *\([^)]*\)))*/\1\2/g
     s/\([^\]\)[|&;<>$`{}]\{1,\}/\1 /g'
}

function _fasd_list_paths() {
    while [ "$1" ]; do
      local fp=$(readlink -f "$1")
      if [[ -f "$fp" ]]; then
        printf %s\\n "$fp"
      elif [[ -d "$fp" ]]; then
        # Ends dir with a "/", so files and dirs can be distinguished during query time.
        printf %s/\\n "$fp"
      fi
      shift
    done
    if [ "$PWD" != "$HOME" ]; then
      echo $(readlink -f $PWD)/
    fi
}

# Usage: _fasd_add cmd ~/a c\ d /t/space\ a
function _fasd_add() {
    [ -f "$_FASD_DATA" -a ! -O "$_FASD_DATA" ] && return

    # Ignores
    while true; do
      case " $_FASD_SHIFT " in
        *\ $1\ *) shift;;
        *) break;;
      esac
    done
    case " $_FASD_IGNORE " in
      *\ $1\ *) return;;
    esac
    shift # Shift to skip command.

    local paths=$(_fasd_list_paths $@ | tr '\n' '|')
    # echo $paths; return
    [ -z "${paths##\|}" ] && return   # stop if we have nothing to add

    # Maintain the file. TODO use a better algorithm.
    local tempfile
    tempfile="$(mktemp "$_FASD_DATA".XXXXXX)" || return
    awk -v list="$paths" -v now="$(date +%s)" -v max="$_FASD_MAX" -F"|" '
      BEGIN {
        split(list, files, "|")
        for(i in files) {
          path = files[i]
          if(path == "") continue
          paths[path] = path # array for checking
          rank[path] = 1
          time[path] = now
        }
      }
      $2 >= 1 {
        if($1 in paths) {
          rank[$1] = $2 + 1 / $2
          time[$1] = now
        } else {
          rank[$1] = $2
          time[$1] = $3
        }
        count += $2
      }
      END {
        if(count > max)
          for(i in rank) print i "|" 0.9*rank[i] "|" time[i] # aging
        else
          for(i in rank) print i "|" rank[i] "|" time[i]
      }' "$_FASD_DATA" 2>> "$_FASD_SINK" >| "$tempfile"
    if [ $? -ne 0 -a -f "$_FASD_DATA" ]; then
      env rm -f "$tempfile"
    else
      env mv -f "$tempfile" "$_FASD_DATA"
    fi
}

# Define the fasd hook to record history, similar to `fasd --init zsh-hook`
_fasd_preexec() {
  if [[ "$1" = [[:space:]]* ]]; then return; fi  # Ignore command starts with space
  if [ ${#2} -ge 300 ]; then return; fi  # Ignore long commands
  { eval "_fasd_add \$(_fasd_sanitize \$2)"; } >> "$_FASD_SINK" 2>&1
}
add-zsh-hook preexec _fasd_preexec

# Define the j command. Ref: https://github.com/clvv/fasd/blob/90b531a5daaa545c74c7d98974b54cbdb92659fc/fasd#L89-L97
function fasd_cd() {
  if [ -d $1 ]; then 
    builtin cd -- "$1"; return 
  fi
  # Remove trailing slash from query.
  local ret=$(=grep "${1%/}.*/|" $_FASD_DATA | sort -t '|' -k 2 -n | tail -n1 | cut -d '|' -f 1)
  [ -z "$ret" ] && return
  [ -d "$ret" ] && builtin cd -- "$ret" || printf %s\n "$ret"
}
alias j='fasd_cd'
