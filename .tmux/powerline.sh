#!/bin/bash -e
# File: powerline.sh
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

if [ "$1" == "left" ]; then
	HOST=`hostname`
	R="#[fg=colour234,bg=colour148][#S]$HOST #[default]#[fg=colour148,bg=colour33]"

	R=$R"#[fg=colour0,bg=colour33]#[default]#[fg=colour33,bg=colour141]"
	# #[fg=colour234,bg=colour148] abc #[default]#[fg=colour148,bg=colour33]
	# #[default]#[fg=colour88,bg=colour29] #[fg=colour5] #[fg=colour88]git #[default]#[fg=colour29,bg=colour141]#[default]

elif [ "$1" == "right" ]; then
	BATTERY=$(acpi |cut -d ' ' -f 4 | sed 's/,//g')
	TEMP=$(sensors |grep 'coretemp' -C 5 |tail -n4 |head -n1 |egrep -o '[0-9\.]+°C' |head -n1 |sed 's/\.0//')
	DATE=$(date "+%m-%d %H:%M")
	R="#[fg=colour237,bg=colour141]#[default]#[fg=colour167,bg=colour237] $TEMP $BATTERY #[default]#[fg=colour37,bg=colour237]"
	R=$R"#[default]#[fg=colour235,bg=colour37]$DATE #[default]"
else
	R=""
fi
echo $R
