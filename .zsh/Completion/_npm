#compdef npm

# Zsh completion script for npm, modified from mercurial version.
# Rename this file to _npm and copy
# it into your zsh function path (/usr/share/zsh/site-functions for
# instance)
#
# If you do not want to install it globally, you can copy it somewhere
# else and add that directory to $fpath. This must be done before
# compinit is called. If the file is copied to ~/.zsh.d, your ~/.zshrc
# file could look like this:
#
# fpath=("$HOME/.zsh.d" $fpath)
# autoload -U compinit
# compinit
#
# Mercurial zsh completion scrpit:
# Copyright (C) 2005, 2006 Steve Borho <steve@borho.org>
# Copyright (C) 2006-10 Brendan Cully <brendan@kublai.com>
#
# Npm zsh completion script:
# Maintainer: Qing 'kuno' Guan <neokuno AT gmail DOT com>
# Contributor: Johan Sundström <>
#
# Permission is hereby granted, without written agreement and without
# licence or royalty fees, to use, copy, modify, and distribute this
# software and to distribute modified versions of this software for any
# purpose, provided that the above copyright notice and the following
# two paragraphs appear in all copies of this software.
#
# In no event shall the authors be liable to any party for direct,
# indirect, special, incidental, or consequential damages arising out of
# the use of this software and its documentation, even if the authors
# have been advised of the possibility of such damage.
#
# The authors specifically disclaim any warranties, including, but not
# limited to, the implied warranties of merchantability and fitness for
# a particular purpose.  The software provided hereunder is on an "as
# is" basis, and the authors have no obligation to provide maintenance,
# support, updates, enhancements, or modifications.

emulate -LR zsh
setopt extendedglob

local curcontext="$curcontext" state line
typeset -A _npm_cmd_globals

_npm() {
  local cmd
  integer i=2
  _npm_cmd_globals=()

  while (( i < $#words ))
  do
    if [[ -z "$cmd" ]]
    then
      cmd="$words[$i]"
      words[$i]=()
      (( CURRENT-- ))
    fi
    (( i++ ))
  done

  if [[ -z "$cmd" ]]
  then
    _arguments -s -w : $_npm_global_opts \
      '*:npm command:_npm_commands'
    return
  fi

  # resolve abbreviations and aliases
  if ! (( $+functions[_npm_cmd_${cmd}] ))
  then
    local cmdexp
    (( $#_npm_cmd_list )) || _npm_get_commands

    cmdexp=$_npm_cmd_list[(r)${cmd}*]
    if [[ $cmdexp == $_npm_cmd_list[(R)${cmd}*] ]]
    then
      # might be nice to rewrite the command line with the expansion
      cmd="$cmdexp"
    fi
    if [[ -n $_npm_alias_list[$cmd] ]]
    then
      cmd=$_npm_alias_list[$cmd]
    fi
  fi

  curcontext="${curcontext%:*:*}:npm-${cmd}:"

  zstyle -s ":completion:$curcontext:" cache-policy update_policy

  if [[ -z "$update_policy" ]]
  then
    zstyle ":completion:$curcontext:" cache-policy _npm_cache_policy
  fi

  if (( $+functions[_npm_cmd_${cmd}] ))
  then
    _npm_cmd_${cmd}
  else
    # complete unknown commands normally
    _arguments -s -w : $_npm_global_opts \
      '*:files:_npm_targets'
  fi
}

_npm_cache_policy() {
  typeset -a old

  # cache for a minute
  old=( "$1"(mm+10) )
  (( $#old )) && return 0

  return 1
}

_npm_get_commands() {
  typeset -ga _npm_cmd_list
  typeset -gA _npm_alias_list
  local hline cmd cmdalias

  _call_program npm npm completion 2>/dev/null | while read -A hline; do
  cmd=$hline[1]
  _npm_cmd_list+=($cmd)

  for cmdalias in $hline[2,-1]; do
    _npm_cmd_list+=($cmdalias)
    _npm_alias_list+=($cmdalias $cmd)
  done
done

_npm_cmd_list+=('list')
}

_npm_commands() {
  (( $#_npm_cmd_list )) || _npm_get_commands
  _describe -t commands 'npm command' _npm_cmd_list
}

# Furtur improvement in the future?
_npm_get_targets() {
  typeset -ga _npm_target_list

  for t in $(ls .); do
    _npm_target_list+=($t)
  done
}

_npm_targets() {
  (( $#_npm_file_list)) || _npm_get_targets
  _describe -t commands 'npm targets' _npm_target_list
}

_npm_get_installed_packages() {
  typeset -ga _npm_installed_package_list
  local hline package

  _call_program npm npm list installed 2>/dev/null | while read -A hline; do
  package=$hline[1]
  _npm_installed_package_list+=($package)
done
}

_npm_installed_packages() {
  (( $#_npm_package_list )) || _npm_get_installed_packages
  _describe -t commands 'npm installed package' _npm_installed_package_list
}

_npm_get_ls_filters() {
  typeset -ga _npm_ls_filter_list

  for f in 'installed' 'stable'; 
  do
    _npm_ls_filter_list+=($f)
  done
}

_npm_ls_filters() {
  (( $#_npm_ls_filter_list )) || _npm_get_ls_filters
  _describe -t commands 'npm list filter' _npm_ls_filter_list
}

_npm_get_config_methods() {
  typeset -ga _npm_config_method_list

  for m in 'set' 'get' 'list' 'delete' 'edit'; do
    _npm_config_method_list+=($m)
  done
}

_npm_config_methods() {
  (( $#_npm_config_method_list )) || _npm_get_config_methods
  _describe -t commands 'npm config method' _npm_config_method_list
}

_npm_get_config_key_list() {
  typeset -ga _npm_config_key_list
  local hline key

  _call_program npm npm config list 2>/dev/null | while read -A hline; do
  key=$hline[1]
  _npm_config_key_list+=($key)
done
}

_npm_config_keys() {
  (( $#_npm_config_key_list )) || _npm_get_config_key_list
  _describe -t commands 'npm config key' _npm_config_key_list
}

_npm_cmd_config() {
  _arguments -s -w : $_npm_global_opts \
    '*:npm command:_npm_config_methods'  
}

_npm_cmd_help() {
  _arguments -s -w : $_npm_global_opts \
    '*:npm command:_npm_commands' 
}

_npm_cmd_ls() {
  _arguments -s -w : $_npm_global_opts \
    '*:npm command:_npm_ls_filters'  
}

_npm_cmd_list() {
  _npm_cmd_ls 
}

_npm_cmd_uninstall() {
  _arguments -s -w : $_npm_global_opts \
    '*:npm command:_npm_installed_packages'  
}

_npm "$@"
