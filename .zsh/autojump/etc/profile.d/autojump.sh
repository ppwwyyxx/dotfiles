# source autojump on BASH or ZSH depending on the shell

shell=`echo ${SHELL} | awk -F/ '{ print $NF }'`

# check local install
if [ -s ~/.autojump/etc/profile.d/autojump.${shell} ]; then
	source ~/.autojump/etc/profile.d/autojump.${shell}

# check global install
elif [ -s /etc/profile.d/autojump.${shell} ]; then
	source /etc/profile.d/autojump.${shell}

# check custom install locations (modified by Homebrew or using --destdir option)
elif [ -s /home/wyx/.zsh/autojump/etc/profile.d/autojump.${shell} ]; then
	source /home/wyx/.zsh/autojump/etc/profile.d/autojump.${shell}

fi
