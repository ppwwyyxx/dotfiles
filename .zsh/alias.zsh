# programming blocks
in_array() {
	# usage:  list=(a b c)
	# in_array a && echo 1
    local hay needle=$1
    shift
    for hay; do
        [[ $hay == $needle ]] && return 0
    done
    return 1
}

# common command
alias cdl='cd'
alias dc='cd'
alias mv='nocorrect mv -iv'
alias mkdir='nocorrect mkdir'
alias cp='nocorrect cp -rvi'
alias cpv="rsync -pogh -e /dev/null -P --"	# cp with progress
alias watch='watch '	# allow watching an alias

alias l="ls -F --color=auto --quoting-style=literal"
alias l.="ls -d .* --color=auto"
alias ls="ls -F --color=auto --quoting-style=literal"
alias sl="ls -F --color=auto --quoting-style=literal"
alias lss="ls -F --color=auto --quoting-style=literal"
alias lsf="ls -1f"	# fast ls
alias lll='ls++'
function ll(){
	ls -AhlXF --color=auto --time-style="+[34m[[32m%g-%m-%d [35m%k:%M[33m][m" $@
	[[ "$*" == "$1" ]] && echo -e " $GREEN  --[$LIGHTBLUE  Dir:    $CYAN`ls -Al $@ | grep '^drw' | wc -l`$LIGHTGREEN|$YELLOW \
	 File: $GREEN`ls -Al $@ | grep -v '^drw' | grep -v total | wc -l` ]-- $WHITE"
}

alias L=less
alias C='cat'
function ccat() { highlight --out-format xterm256 $1 2>/dev/null || =cat $1 }
function cless() { ccat $1 | less -r }
alias -g B='|sed -r "s:\x1B\[[0-9;]*[mK]::g"'       # remove color, make things boring
alias -g N='>/dev/null'
alias -g NN='>/dev/null 2>&1'
which rg NN && {
	alias -g G='|rg'
	alias ag='rg -i'
	alias agp='rg -i -tpy'
} || {
	which ag NN && {
		alias -g G='|ag'
		alias agp='ag --python'
	} || alias -g G='|grep'
}

alias awk-sum="awk '{if (\$1+0!=\$1) { print \"Fail! \"\$0, NR; exit; }; s+=\$1} END {print s, s / NR}' "
alias awk-last="awk '{print \$NF}'"
alias awk-transpose="awk '
{
	for (i=1; i<=NF; i++)
		a[NR,i] = \$i;
}
(NF>p) {p = NF}
END {
for(j=1; j<=p; j++) {
	str=a[1,j];
	for(i=2; i<=NR; i++)
		str=str\"\t\"a[i,j];
	print str
}}'"

# rm moves things to trash
function rm() {
	for file in $@; do
		local FILE_LOC="`readlink -f $file`"
		if [[ $FILE_LOC == /home/* ]]; then
			mkdir -p $HOME/.Trash
			mv "$file" $HOME/.Trash/ --backup=numbered -fv
		else
			=rm "$file" -rvf
		fi
	done
}


which nvim NN && {
	alias v='nvim'
	alias vi='nvim'
	alias iv='nvim'
	alias vim='nvim'
} || {
	alias v='=vim'
	alias vi='=vim'
	alias iv='=vim'
}
alias sv='sudo vim'

alias sort='LC_ALL=C sort'
alias uniq='LC_ALL=C uniq'
alias cls='clear'
alias grep='grep -IE --color=auto --exclude=.tags --exclude-dir="node_modules" --exclude-dir=".git" --exclude-dir=".env"'
alias tmuxa='tmux a || tmux'
alias du='du -sh'
alias strace='strace -yy'
alias tail='tail -n $((${LINES:-`tput lines 4>/dev/null||echo -n 12`} - 3))'
alias head='head -n $((${LINES:-`tput lines 4>/dev/null||echo -n 12`} - 3))'
alias rf='readlink -f'
function sdu () {	# human-readable sorted du
  [[ "$#" -eq 1 && -d "$1" ]] && cd "$1"
	du -sh {*,.*} | sort -h
}
function openedfile() {
  if [[ -n $1 ]]; then
		find /proc/$1/fd -xtype f -printf "%l\n" | =grep -P '^/(?!dev|proc|sys)'
	else
		find /proc/*/fd -xtype f -printf "%l\n" | =grep -P '^/(?!dev|proc|sys)' | sort | uniq -c | sort -n
	fi
}

function linkto() {
	[[ -d $1 ]] || return 1
	dest="$1"
	f=`readlink -f "$2"`
	cd $dest
	ln -sv $f ./
	cd -
}

# network
alias p='ping'
alias meow='ping'
alias p6='ping -6'
alias pc='proxychains4'
alias iwc='iwconfig wlp2s0; ifconfig wlp2s0'
alias port='sudo netstat -ntlpu'
alias listen='lsof -P -i -n'
alias scp='scp -r'
alias rsync='rsync -avP'
alias speedtest='wget -O /dev/null http://speedtest-sfo2.digitalocean.com/100mb.test'
alias m_rsync='rsync --progress --partial --delete --size-only -rlv --bwlimit=5m'
alias myip='dig +short myip.opendns.com @resolver1.opendns.com'
function view-email() { mhonarc -single $1 | w3m -dump -T text/html }
alias chromium-socks='chromium --proxy-server=socks5://localhost:8080'
alias chromium-http='chromium --proxy-server=localhost:7777'
alias google-keep='chromium --profile-directory=Default --app-id=hmjkmjkepdijhoojdojkdfohbdgmmhki'
alias gg='google -r'
alias gl='google -o'
alias weather='curl -s http://wttr.in/\?m | head -n-1'

alias ssh-reverse='ssh -R 6333:localhost:22 -ServerAliveInterval=60'
function st() { ssh "$1" -t 'tmux a -d || tmux' }
function ssh-proxy { ssh $2 -o ProxyCommand="ssh -q $1 nc %h %p" }
function gmtr() {
	sudo mtr -lnc 1 "$1" | paste - - \
		| awk 'function geo(ip) {
			s="geoiplookup "ip;
			s|&getline; s|&getline;
			split($0, a, "[,:] ");
			return a[3]","a[4]","a[5]","a[6]
		  };
		  { print $2"\t"$3"\t"$6/1000"ms\t"geo($3) }'
}
function proxy() {
	p="$1"
	https_proxy=$p http_proxy=$p ${@: 2}
}
alias vnc-quick='vncviewer -QualityLevel=0 -CompressLevel=3 -PreferredEncoding=ZRLE -FullScreen=1 -Shared=1'
alias rdesktop-nana='rdesktop-vrdp -K -u wyx -p - 59.66.131.64:3389'


# develop utils
which colormake NN && {
	alias make='colormake'
}
which ccache NN && {
	alias mk='CXX="ccache g++" make'
} || { alias mk='make' }
alias mr='make run'
alias mc='make clean'
alias mkc='make clean'
alias cmk='mkdir -p build; cd build; cmake ..; make; cd ..'
alias gits='git s'
alias gsh='git -c color.status=always status | head -n20'
alias gcl='git clone --recursive'
alias indent='indent -linux -l80'
alias gdb='gdb -q'
alias R='R --vanilla'
alias ctags='ctags -R -f .tags --c++-kinds=+p --fields=+iaS --extra=+q'
alias valgrind='valgrind --leak-check=full --track-origins=yes --show-possibly-lost=yes'
which colordiff NN && alias diff='colordiff'
alias googlelink='python2 -c "import urlparse, sys; print urlparse.parse_qs(urlparse.urlparse(sys.argv[1]).query)[\"url\"][0]"'
# cd to git repo root
function cdp () {
	dir=$(git rev-parse --show-toplevel 2>/dev/null)
	if [ $? -eq 0 ]; then
		CDP=$dir
		cd $dir
	else
		echo "'$PWD' is not git repos"
	fi
}


# tools
which aunpack NN && alias x=aunpack
alias gq='geeqie'
alias strings='strings -atx'
alias which='which -a'
alias ibus-daemon='ibus-daemon --xim'
alias zh-CN="LC_ALL='zh_CN.UTF-8'"
alias manzh="LC_ALL='zh_CN.UTF-8' man"
alias free='free -hw'
which dfc NN && alias df='dfc' || alias df='df -Th'
alias convmv='convmv -f GBK -t UTF-8 --notest -r'
alias window='wmctrl -a '
alias cp2clip='xclip -i -selection clipboard'
alias screenkey='screenkey -s small -t 0.8 --opacity 0.3'
alias adate='for i in Asia/Shanghai US/{Eastern,Pacific}; do printf %-22s "$i ";TZ=:$i date +"%F %a %T %Z";done'
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias clean-trash='=rm /ssd_home/.Trash/{.,}* -rf; =rm ~/.Trash/{.,}* -rf'
alias sacct='sacct -o User,JobID,Jobname,partition,state,MaxRss,MaxVMSize,avediskread,nnodes,ncpus,nodelist,start,end,elapsed'

alias win='cd; virtualbox --startvm win7 & ; cd -'
alias osx='cd; virtualbox --startvm osx & ; cd -'

alias please='sudo'
alias umount='sudo umount'
alias mount='sudo mount'
alias iotop='sudo iotop'
alias iftop='sudo iftop -B'
alias powertop='sudo powertop'
alias sy='sudo systemctl'
alias dstat='dstat -dnmcl --socket --top-io -N wlp2s0'
function agenda() {
	gcalcli --details=length agenda '12am' $(date --date="${1:-1} day" +"%Y%m%d")
}
alias calw='gcalcli calw'
function systemd-run-env() {
	# env vars that need do be passed to the process
	local names=('PATH' 'LD_LIBRARY_PATH' 'TENSORPACK_DATASET'
							 'CPATH' 'PYTHONPATH' 'LIBRARY_PATH')
	local earg=""
	for i in $names; do
		earg="$earg --setenv $i=${(P)i}"
	done
	systemd-run -t --user ${=earg} $@
}

# hardware
function km() {	# only for my laptop
	xmodmap ~/.Xmodmap
	xset r rate 200 40
	xcape -e "Control_L=Escape;Hyper_L=XF86Mail"
	#xinput set-button-map $(xinput | grep -o "TouchPad.*id=[0-9]*" | grep -o "[0-9]*") 1 0 0
}
alias dmesg='dmesg -H || dmesg | less'
alias keyb='xinput disable $(xinput | grep -o "TouchPad.*id=[0-9]*" |grep -o "[0-9]*")'
alias unkeyb='xinput enable $(xinput | grep -o "TouchPad.*id=[0-9]*" |grep -o "[0-9]*")'
function modulegraph() { lsmod | perl -e 'print "digraph \"lsmod\" {";<>;while(<>){@_=split/\s+/; print "\"$_[0]\" -> \"$_\"\n" for split/,/,$_[3]}print "}"' | dot -Tpng | feh -; }
alias lsblk="lsblk -o NAME,SIZE,FSTYPE,MOUNTPOINT,LABEL,VENDOR,MODEL"

function usbon () {
	id=$1
	if [[ -z $1 ]]; then
		echo 'usage: $1 is the first 4 characters of id in `lsusb`'
		return
	fi
	for i in $(find -L /sys/bus/usb/devices -maxdepth 2 -name idVendor); do
		if [[ $(cat $i) == $id ]]; then
			f=$i
		fi
	done
	echo "USB in: $(dirname $f)"
	powerf=$(dirname $f)/power/control
	echo -n "USB power state: "
	cat $powerf
	sudo bash -c "echo on > $powerf"
	echo -n "USB power state now: "
	cat $powerf
}
alias __nvq='nvidia-smi --query-gpu=temperature.gpu,clocks.current.sm,pstate,power.draw,utilization.gpu,utilization.memory,memory.free --format=csv | tail -n+2'
which nl NN && {
	alias nvq='(echo "GPU,temp, clocks, pstate, power, util.GPU, util.MEM, freeMEM" && __nvq | nl -s, -w1 -v0) | column -t -s,'
} || {
	alias nvq='(echo "temp, clocks, power, util.GPU, util.MEM, freeMEM" && __nvq) | column -t -s ,'
}
alias __nvp="nvidia-smi | awk '/PID/ { seen=1 } seen {print} ' \
	| tail -n+3 | head -n-1  |  awk '{print \$2, \$(NF-1), \$3}' \
	| grep -v '^No' \
	| awk 'BEGIN{OFS=\"\\t\"} { cmd=(\"ps -ho '%a' \" \$3); cmd | getline v; close(cmd); \$4=v; print }'"
alias nvp="(echo \"GPU\tMEM\tPID\tCOMMAND\" && __nvp) | column -t -s $'\t'"
alias nsmi='watch -n 0.5 nvidia-smi'

function b(){
	=acpi -V | head -n1
	sensors | grep Physical
	sensors | grep RPM
	cat /proc/acpi/ibm/fan | head -n3 |tail -n1
	#echo "Graphic Card:  `nvidia-settings -q gpucoretemp |grep "Keep" |grep -o "\ [0-9]+" `  °C "
	for ((i=0; i<1; i++)); do
		echo -n "cpu$i : "
		cat "/sys/devices/system/cpu/cpu$i/cpufreq/scaling_governor"
	done
	sudo cpupower frequency-info -w | sed 'N;s/\n//g'
}

# softwares
alias mathc='/opt/Mathematica/Executables/math'
alias mathematica='/opt/Mathematica/Executables/Mathematica -nosplash'
which matlab NN || {
  [[ -d /opt/Matlab ]] && alias matlab='/opt/Matlab/bin/matlab'
} && {
	alias matlabc='matlab -nodisplay -r clc '

	# fix matlab on archlinux
	export J2D_D3D="false"
	[[ -d /usr/lib/jvm/java-8-openjdk/jre ]] && export MATLAB_JAVA=/usr/lib/jvm/java-8-openjdk/jre
}
alias rstudio='/opt/RStudio/lib/rstudio/bin/rstudio'
alias maple='/opt/Maple/bin/xmaple'
function gource() {
local f=${1:-gource}
=gource -s .1 -1280x720 --auto-skip-seconds .1 \
    --multi-sampling --stop-at-end \
    --key --highlight-users --file-idle-time 0 --max-files 0  \
    --background-colour 000000 --font-size 22 \
		--title "`basename $(readlink -f $f)`" \
    --output-ppm-stream - --output-framerate 30 > /dev/null
    #| avconv -y -r 30 -f image2pipe -vcodec ppm -i - -b 65536K movie.mp4
}
export PYCHARM_JDK=/opt/java-oracle
export RUBYMINE_JDK=/opt/java-oracle
export IDEA_JDK=/opt/java-oracle
export WEBIDE_JDK=/opt/java-oracle

alias wine32='WINEARCH=win32 LC_ALL=zh_CN.utf-8 WINEPREFIX=~/.wine32 wine'
alias net9='luit -encoding gb18030 -- ssh ppwwyyxx@bbs.net9.org'
alias smth='luit -encoding gb18030 -- ssh -1 ppwwyyxx@bbs.smth.org'
alias freenode='weechat-curses -r "/connect freenode"'	# /join #tuna

# media
alias idf='identify'
alias dot='dot -Tpng -O -v'
alias 2pdf='libreoffice --headless --convert-to pdf' # unoconv -f pdf
alias 2csv='libreoffice --headless --convert-to csv'

alias mirror='mplayer -tv driver=v4l2:device=/dev/video0 tv:// -vf-add mirror'
alias tune-pitch='mplayer -af scaletempo=speed=pitch'
alias record='ffmpeg -f alsa -ac 1 -i pulse -f x11grab -s 1366x768 -r 40 -show_region 1 -i :0.0 ~/Video/out.mpg'
m_sub_param='-subcp utf-8 -subfont-text-scale 2.5 -subfont "/usr/share/fonts/wenquanyi/wqy-microhei/wqy-microhei.ttc"'
m_avc_param="-oac mp3lame -lameopts fast:preset=medium -ovc x264 -x264encopts subq=5:8x8dct:frameref=2:bframes=3:weight_b:threads=auto"
f_avc_param_old="-c:v libx264 -preset slow -crf 22 -c:a libmp3lame"
f_avc_param="-map 0 -c:v libx265 -preset medium -x265-params crf=23 -c:a aac -strict experimental -b:a 128k"
function ffmpeg_compress() { ffmpeg -i "$1" `echo $f_avc_param` -vf subtitles=$1 $1.mp4 }
function mencoder_compress() { mencoder "$1" -o $1.avi `echo $m_avc_param` }
function ffmpeg_audio() { ffmpeg -i "$1" -vn "${1%.*}".mp3}
function colormap(){
	for i in {0..255}; do
		print -Pn "%{$reset_color%}$i: "
		print -Pn "%{%b%F{$i}%}Hello World, "
		print -P "%{%F{$i}%B%}Hello World"
	done
}


# processes
alias psg='nocorrect pgrep -a'
function pstack() { =gdb -q -nx -p $1 <<< 't a a bt' | sed -ne '/^#/p' }

local top_version=$(=top -h 2>/dev/null | head -n1 | grep -o '[0-9]*$')
if [[ "$top_version" -ge 10 ]]; then
	alias top='top -d 0.5 -o %CPU -c'
else
	alias top='top -d 0.5 -c'
fi
alias topme='top -u $USER'
alias psmem="ps aux|awk '{print \$4\"\\t\"\$11}'|grep -v MEM|sort -n | tail -n20"
memgrep() { grep VmHWM /proc/$(pgrep -d '/status /proc/' "$1")/status; }
function killz() {
	ppid=$(ps -oppid $1 | tail -n1)
	kill -SIGHUP $ppid
}
function waitpid() { while test -d "/proc/$1"; do sleep 1; done }
function retry() {
# Usage: retry <max-number> <command>
# or: retry <command>
	case $1 in
		''|*[!0-9]*)
				local cmd="${@}"
				while true; do
					eval $cmd && break || {
						echo "Retry $cmd ..."; sleep 0.1
					}
				done
			;;
		*)
			local cmd="${@: 2}"
			local n=1
			until [[ $n -ge $1 ]]; do
				eval $cmd && break || {
					((n++))
					echo "Retry No.$n $cmd ..."; sleep 0.1
				}
			done
			;;
	esac
}

# python
alias py='PYTHONPATH=$HOME/.config/python:$PYTHONPATH python3'
alias ipy='ipython'
function pydbg () { ipython --pdb -c "%run $1" }
alias bp2='bpython2'
alias piu='pip install --user'
alias piuu='pip install --user -U'
alias piu2='pip2 install --user'
alias piuu2='pip2 install --user -U'
alias piu3='pip3 install --user'
alias piuu3='pip3 install --user -U'
alias pyftp='python2 -m pyftpdlib'
function pytwistd() { twistd web --path "$1" -p tcp:"${2:-8000}" }
function web() {
	pytwistd
  #ruby -run -e httpd "$1" -p "${2:-8000}"
}
alias pipup="pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install --user -U; pip2 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip2 install --user -U"
alias unquote='python2 -c "import sys, urllib as ul; [sys.stdout.write(ul.unquote(l)) for l in sys.stdin]"'

# package
which pacman NN && {
	alias pS='pacaur -S'
	alias pU='sudo pacman -U'
	alias pSs='pacaur -Ss'
	alias pSi='pacman -Si'
	alias pQo='pacman -Qo'
	alias pSy='sudo pacman -Syy'
	alias pR='sudo pacman -R'
	alias pSu='pacaur -Syu'
	alias pQl='pacman -Ql'
	alias paur='pacman -Qm'
	#alias pacman-size="paste <(pacman -Q | awk '{ print \$1; }' | xargs pacman -Qi | grep 'Size' | awk '{ print \$4\$5; }') <(pacman -Q | awk '{print \$1; }') | sort -h | column -t"
	alias pacman-size="expac -H M '%m\t%n' | sort -h"
	function pacman-orphan() {
	  if [[ ! -n $(pacman -Qdt) ]]; then
			echo "No orphans to remove."
	  else
			sudo pacman -Rns $(pacman -Qdtq)
	  fi
	}
} || {

[[ -n $_CFG_ON_MAC ]] && {
	alias pS='brew install'
	alias pSs='brew search'
	alias pSu='brew upgrade'
	alias pR='brew uninstall'
	alias pQl='brew list'
} || {
	which apt-get NN && {
			which apt NN && {
			  alias pS='sudo apt install'
				alias pR='sudo apt remove'
				alias pSs='apt search'
				alias pSy='sudo apt update'
				alias pSu='sudo apt upgrade'
			} || {
				alias pS='sudo aptitude install'
				alias pR='sudo aptitude purge'
				alias pSs='aptitude search'
				alias pSy='sudo aptitude update'
				alias pSu='sudo aptitude upgrade'
			}
			alias pQo='apt-file search'
			alias pQl='dpkg-query -L'
			#alias pQl2='apt-file list'
			alias pU='sudo dpkg -i'
		} || {
			alias pS='sudo yum install'
			alias pR='sudo yum remove'
			alias pSy='sudo yum check-update'
			alias pSu='sudo yum update'
			alias pSs='yum search'
			alias pQo='yum whatprovides'
			alias pQl='rpm -ql'
		}
	}
}


# weird stuff
function theano() {
	if [[ "$1" == gpu* ]] ; then
		device="$1"
		args=("${@: 2}")
	else
		device=gpu$(nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits | nl | sort -n -k2 | awk '{print $1-1; exit}')
		args=("${@: 1}")
	fi
	OMP_NUM_THREADS=1 THEANO_FLAGS="device=$device,floatX=float32,allow_gc=False,linker=cvm_nogc,warn_float64=warn" $args
}

function tpgrep() {
# a function to grep tensorpack logs
# $1: string to grep
# $2+: dirs or logs
	[[ -n $1 ]] || return 1
	local pat=$1
	local cmd="paste"
	for d in ${@:2}; do
		[[ -d $d ]] && {
			cmd="$cmd <(cat "$d/log.log" | grep '$pat' | awk-last)"
		} || {
			cmd="$cmd <(cat "$d" | grep '$pat' | awk-last)"
		}
	done
	eval $cmd
}
