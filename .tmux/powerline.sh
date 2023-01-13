#!/bin/bash -e

if [ "$1" == "left" ]; then
    HOST=`hostname`
    if [[ $HOST == *corp.google.com ]]; then
      HOST=corp
    fi
    R="#[fg=colour234,bg=colour148][#S]$HOST #[default]#[fg=colour148,bg=colour33]"

    R=$R"#[fg=colour0,bg=colour33]#[default]#[fg=colour33,bg=colour141]"
    # #[fg=colour234,bg=colour148] abc #[default]#[fg=colour148,bg=colour33]

elif [ "$1" == "right" ]; then
    # Battery 0: Not charging, 94%
    BATTERY=$(acpi | awk '{print $NF}')
    if [[ -n $BATTERY ]]; then
      BATTERY=$BATTERY
    fi

    TEMP=$(sensors |grep 'coretemp' -A 5 | grep -oP '[0-9\.]+(?=°C)' |head -n1 |sed 's/\.0//')
    TEMPSTR="$TEMP"
    if (( TEMP > 80 )); then
      TEMPSTR="#[fg=red]$TEMPSTR#[fg=colour167]"
    fi

    MEM=$(free -h | awk '/Mem:/{printf "%.0f", $3/$2*100}')
    MEMSTR="🖫$MEM%"
    if (( MEM > 90 )); then
      MEMSTR="#[fg=red]$MEMSTR#[fg=colour167]"
    elif (( MEM > 70 )); then
      MEMSTR="#[fg=yellow]$MEMSTR#[fg=colour167]"
    fi

    DATE=$(date "+%m/%d %H:%M")
    R="#[fg=colour237,bg=colour141]#[default]#[fg=colour167,bg=colour237] $TEMPSTR $BATTERY $MEMSTR #[default]#[fg=colour37,bg=colour237]"
    R=$R"#[default]#[fg=colour235,bg=colour37]$DATE #[default]"
else
    R=""
fi
echo $R
