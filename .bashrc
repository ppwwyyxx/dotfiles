# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

source /etc/environment
export PATH=$PATH:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:$HOME/.local/bin

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

[[ -f $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm


HISTCONTROL=ignoredups:ignorespace
shopt -s histappend
HISTSIZE=1000000
HISTFILESIZE=1000000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# customed settings
#     this is title | this is the line in terminal
PS1='\[\e]0;\w\007\]\[\033[32m\]\D{%Y-%m-%d %H:%M:%S}\[\033[01;32m\] \u@\h \[\033[01;34m\]\W\[\033[00m\]\n\$ '


alias l='ls -F --color=auto'
alias ls='ls -F --color=auto'
alias lss='ls -F --color=auto'
alias L=less
alias sv='sudo vim'
