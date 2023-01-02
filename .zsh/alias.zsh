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

[[ -z $_CFG_ON_MAC ]] && {
  alias open='xdg-open'
}
alias -s {pdf,PDF,JPEG,JPG,jpg,jpeg,png,PNG,gif,html,mhtml,obj,wmv,mkv,mp4,MP4,mp3,avi,rm,rmvb,flv}=xdg-open

# common command
alias cdl='cd'
alias dc='cd'
alias mv='nocorrect mv -iv'
alias mkdir='nocorrect mkdir'
alias buck='nocorrect buck'
alias cp='nocorrect cp -rvi'
alias hg='nocorrect hg'
alias cpv="rsync -pogh -e /dev/null -P --"  # cp with progress
alias watch='watch '  # allow watching an alias

() {
  if [[ -z $_CFG_ON_SSH ]]; then
    _hyperlink="--hyperlink=auto"
  fi
  alias l="ls -F --color=auto --quoting-style=literal $_hyperlink"
  alias l.="ls -d .* --color=auto $_hyperlink"
  alias ls="ls -F --color=auto --quoting-style=literal $_hyperlink"
  alias sl="ls -F --color=auto --quoting-style=literal $_hyperlink"
  alias lss="ls -F --color=auto --quoting-style=literal $_hyperlink"
}
alias lsf="ls -1f"  # fast ls
alias lll='ls++'
function ll(){
  ls -AhlF --color=auto --hyperlink=auto --time-style="+[34m[[32m%g-%m-%d [35m%k:%M[33m][m" $@
  [[ "$*" == "$1" ]] && echo -e " $GREEN  --[$LIGHTBLUE  Dir:    $CYAN`ls -Al $@ | grep '^drw' | wc -l`$LIGHTGREEN|$YELLOW \
   File: $GREEN`ls -Al $@ | grep -v '^drw' | grep -v total | wc -l` ]-- $WHITE"
}

alias L=less
alias C='cat'
alias -g B='|sed -r "s:\x1B\[[0-9;]*[mK]::g"'       # remove color, make things boring
alias -g N='>/dev/null'
alias -g NN='>/dev/null 2>&1'
which batcat NN && {
  alias cat='batcat --style=numbers,grid'
}
which bat NN && {
  alias cat='bat --style=numbers,grid'
}
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

rsync_watch() {  # copy A to B whenever A changes.
  rsync --copy-links --progress --recursive "$1" "$2"
  while inotifywait -r -e create,delete,modify "$1";
    do rsync --copy-links --progress --recursive "$1" "$2"
  done
}

# rm moves things to trash
unalias rm 2>/dev/null || true  # undef alias, if there is one
function __find_disk_of_file() {
  # https://www.cyberciti.biz/faq/linux-unix-command-findout-on-which-partition-file-directory-exits/
  =df -T $1 | awk '/^\/dev/ {print $1}'
}
function rm() {
  mkdir -p $HOME/.Trash
  for file in $@; do
    if [[ $file == -* ]]; then
      continue
    fi
    if [[ -L $file ]]; then  # link
      =rm $file -vf
      continue
    fi
    if [[ ! -e $file ]]; then
      echo "$file: No such file or directory"
      continue
    fi
    local f_disk=$(__find_disk_of_file $file)
    # TODO custom list of trash dir in zshrc.local
    local trash_disk=$(__find_disk_of_file $HOME/.Trash)
    if [[ $f_disk == $trash_disk ]]; then
      mv "$file" $HOME/.Trash/ --backup=numbered -fv
    else
      =rm "$file" -rvf
    fi
  done
}

function compress-dir() {
  if [[ ! -d $1 ]]; then
    echo "$1 is not a directory"
    exit
  fi
  local name=$1
  tar czvf ${name%/}.tgz $1 && rm -rf $1
}

function colorline() {
  local cols
  cols=($fg[green] $fg[white] $fg[blue] $fg[white] $fg[cyan] $fg[white]
    $fg[magenta] $fg[white] "\e[38;05;154m" $fg[white])
  local ncol=${#cols}
  local i=1
  while IFS= read line; do
    echo -n $cols[$i]
    echo "$line"
    i=$(( $i % $ncol + 1 ))
  done
  echo -n $reset_color
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
which emacsclient NN && {
    alias e='emacsclient'
}
alias sv='sudo vim'

alias sort='LC_ALL=C sort'
alias uniq='LC_ALL=C uniq'
alias grep='grep -IE --color=auto --exclude=.tags --exclude-dir="node_modules" --exclude-dir=".git" --exclude-dir=".env"'
alias du='du -sh'
alias strace='strace -yy'
alias tail='tail -n $((${LINES:-`tput lines 4>/dev/null||echo -n 12`} - 3))'
alias head='head -n $((${LINES:-`tput lines 4>/dev/null||echo -n 12`} - 3))'
alias rf='readlink -f'
alias printurl=$'printf \'\e]8;;%s\e\\%s\e]8;;\e\\\n\''

# Tmux-related:
alias tmuxa='tmux a || tmux'
alias tmux-reset-prefix='tmux set -g status-bg colour141; tmux unbind C-a; tmux set -g prefix C-q; tmux bind C-q send-prefix'
which timg NN && {
  which tmux-escape NN && {
    alias timg='tmux-escape kittyimg'
  }
}
which fzf-tmux NN && {
  alias fzf-tmux='fzf-tmux -d 20% --multi --reverse'
}

function sdu () {  # human-readable sorted du
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

function patch1() { patch -p1 < $1 }


# Avoid accidental shut down
if [[ -n $_CFG_ON_SSH ]]; then
  alias poweroff='vboxmanage controlvm win7 savestate; sudo poweroff'
  alias reboot='vboxmanage controlvm win7 savestate; sudo reboot'
else
  alias -g halt=
  alias -g poweroff=
  alias -g shutdown=
  alias -g reboot=
fi

# network
alias p='ping'
alias meow='ping'
alias iwc='iwconfig; ifconfig'
alias port='sudo netstat -ntlpu'
alias listen='lsof -P -i -n'
alias scp='scp -r'
alias rsync='rsync -avP'
alias speedtest='wget -O /dev/null http://speedtest-sfo2.digitalocean.com/100mb.test'
alias m_rsync='rsync --progress --partial --delete --size-only -rlv'

alias chromium-socks='chromium --proxy-server=socks5://localhost:8080'
alias chromium-http='chromium --proxy-server=localhost:7777'
alias google-keep='chromium --profile-directory=Default --app-id=hmjkmjkepdijhoojdojkdfohbdgmmhki'
alias weather='curl -s http://wttr.in/\?m | head -n-1'

# vim edit remote file
function vscp() {
  if [[ -z $1 ]]; then
    echo "usage: vscp [[user@]host1:]file1 ... [[user@]host2:]file2"
    return
  fi
  declare -a targs=()
  echo "Editing Remote Files"
  for iarg in $@; do
    targ="scp://$(echo $iarg | sed -e 's@:/@//@' | sed -e 's@:@/@')"
    targs=("${targs[@]}" $targ)
  done
  echo ${targs[@]}
  vim ${targs[@]}
}
compdef vscp=scp

function pasteimage() {
  local url=$(curl -F "name=@$1" https://img.vim-cn.com)
  echo $url | xclip -i -selection clipboard
  echo "$url copied to clipboard."
}

alias ssh-reverse='ssh -R 6333:localhost:22 -ServerAliveInterval=60'
function st() { ssh "$1" -t 'tmux a -d || tmux' }
function ssh-proxy { ssh $2 -o ProxyCommand="ssh -q $1 nc %h %p" }
# don't check or add host keys
alias ssh-temp='ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null'
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


# develop utils
which colormake NN && {
  alias make='colormake'
}
which ccache NN && {
  alias mk='CXX="ccache g++" make'
} || { alias mk='make' }
alias mkc='make clean'
alias cmk='mkdir -p build; cd build; cmake ..; make; cd ..'
alias gits='git s'
alias gsh='git -c color.status=always status | head -n20'
alias gcl='git clone --recursive'
alias indent='indent -linux -l80'
alias gdb='gdb -q'
alias ctags='ctags -R -f .tags --c++-kinds=+p --fields=+iaS --extra=+q'
alias valgrind='valgrind --leak-check=full --track-origins=yes --show-possibly-lost=yes'
which colordiff NN && alias diff='colordiff'
alias googlelink='python3 -c "import sys, urllib.parse as up; print(up.parse_qs(up.urlparse(sys.argv[1]).query)[\"url\"][0])"'
alias disasm='objdump -d -M att -r -C'
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
function gitdate () {
  local date=$(date --date="$1")
  echo $date
  shift
  GIT_AUTHOR_DATE=$date GIT_COMMITTER_DATE=$date git $@
}


# tools
alias gq='geeqie'
alias strings='strings -atx'
alias which='which -a'
alias zh-CN="LC_ALL='zh_CN.UTF-8'"
alias manzh="LC_ALL='zh_CN.UTF-8' man"
alias free='free -hw'
which dfc NN && alias df='dfc -T -t -squashfs' || alias df='df -Th'
alias convmv-gbk2utf8='convmv -f GBK -t UTF-8 --notest -r'
alias window='wmctrl -a '
which yank NN && {
  alias cp2clip=yank
} || {
  alias cp2clip='xclip -i -selection clipboard'
}
alias screenkey='screenkey -s small -t 0.8 --opacity 0.3'
alias adate='for i in Asia/Shanghai US/{Eastern,Pacific}; do printf %-22s "$i ";TZ=:$i date +"%F %a %T %Z";done'
function notify-send() {
  if [[ $_CFG_ON_SSH ]]; then
    tmux-escape notify $1
  else
    =notify-send $@
  fi
}
alias clean-trash='=rm ~/.Trash/* -rf; =rm ~/.Trash/.* -rf'
function iconvcp936() {
  iconv -f cp936 -t utf-8 "$1" | sponge "$1"
}

which squeue NN && {
  alias sacct='=sacct -S $(date +"%m/%d" -d "-4days") -o jobid,jobname%30,alloccpus%3,reqmem%5,state%8,nodelist%16,start%16,end%16,elapsed%5 | colorline'
  alias sacct-all='=sacct -S $(date +"%m/%d" -d "-5hours") -a -o User%10,JobID,Jobname,state%5,MaxRss,MaxVMSize,avediskread,nnodes%3,ncpus%3,nodelist,start%16,end%16,elapsed%5 | colorline'
  alias squeue='=squeue -u $(whoami) -o "%i|%u|%30j|%t|%M|%R|node:%D|cpu:%c|%b" | column -s "|" -t | sort -n -k 1 | colorline'
  alias squeue-all='=squeue -o "%i|%u|%30j|%t|%M|%R|node:%D|cpu:%c|%b" | column -s "|" -t | colorline'
  alias slurm-gpu-per-user="=squeue -o %u:%D:%b | tail -n+2 | awk -F ':' '{a[\$1]+=\$2*\$NF} END {for (i in a) {print i, a[i]; s+=a[i];} print \"Total\", s}' | sort -n -k2 | column -t | grep -z $USER"
  alias sinfo-by-type="=sinfo -o '%f %A %N %m %G' | column -t"
}

which docker NN && {
  alias docker-prune='docker stop $(docker ps -a -q); docker system prune -f'
}

alias please='sudo'
alias umount='sudo umount'
alias mount='sudo mount'
alias mount_ntfs='sudo mount -o iocharset=utf8,dmask=022,fmask=133,uid=`id -u`,gid=`id -g` -t ntfs-3g'
alias mount_vfat='sudo mount -o dmask=022,fmask=133,uid=`id -u`,gid=`id -g`'
alias iotop='sudo iotop'
alias iftop='sudo iftop -B'
alias powertop='sudo powertop'
alias sy='sudo systemctl'
alias dstat='dstat -dnmcl --socket --top-io -N wlp2s0'
function systemd-run-env() {
  # env vars that need do be passed to the process
  local names=('PATH' 'LD_LIBRARY_PATH' 'TENSORPACK_DATASET'
               'PYTHONPATH' 'LIBRARY_PATH')
  local earg=""
  for i in $names; do
    earg="$earg --setenv $i=${(P)i}"
  done
  systemd-run -t --user ${=earg} $@
}

# hardware
function km() {  # only for my laptop
  killall xcape
  xmodmap ~/.Xmodmap
  xset r rate 200 40
  xcape -e "Control_L=Escape;Hyper_L=XF86Mail"
  #xinput set-button-map $(xinput | grep -o "TouchPad.*id=[0-9]*" | grep -o "[0-9]*") 1 0 0
}
alias dmesg='dmesg -wH || dmesg | less'
function modulegraph() { lsmod | perl -e 'print "digraph \"lsmod\" {";<>;while(<>){@_=split/\s+/; print "\"$_[0]\" -> \"$_\"\n" for split/,/,$_[3]}print "}"' | dot -Tpng | feh -; }
alias lsblk="lsblk -o NAME,SIZE,FSTYPE,MOUNTPOINT,PARTLABEL,LABEL"
alias disk-writeback='watch -n1 grep -e Dirty: -e Writeback: /proc/meminfo'
xrandr-newmode() {
  local line=$(gtf $@ | grep -o 'Modeline.*' | cut -d ' ' -f 2-)
  xrandr --newmode $(echo $line)
}

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
which nvidia-smi NN && {
  alias __nvq='nvidia-smi --query-gpu=temperature.gpu,clocks.current.sm,pstate,power.draw,utilization.gpu,utilization.memory,memory.free --format=csv | tail -n+2'
  which nl NN && {
    alias nvq='(echo "GPU,temp, clocks, pstate, power, util.GPU, util.MEM, freeMEM" && __nvq | nl -s, -w1 -v0) | column -t -s,'
  } || {
    alias nvq='(echo "temp, clocks, power, util.GPU, util.MEM, freeMEM" && __nvq) | column -t -s ,'
  }
  alias __nvp="nvidia-smi | awk '/GPU.*PID/ { seen=1 }; /==========/{if (seen) pp=1; next} pp ' \
    | head -n-1  |  awk '{print \$2, \$(NF-1), \$3 == \"N/A\" ? \$5 : \$3}' \
    | grep -v '^No' \
    | awk 'BEGIN{OFS=\"\\t\"} { cmd=(\"ps -ho '%a' \" \$3); cmd | getline v; close(cmd); \$4=v; print }'"
  alias nvp="(echo \"GPU\tMEM\tPID\tCOMMAND\" && __nvp) | column -t -s $'\t' | cut -c 1-\$(tput cols)"
  alias nvpkill="nvp | awk '{print \$3}' | tail -n+2 | xargs -I {} sh -c 'echo Killing {}; kill {} || echo failed'"
  alias fuser-nvidia-kill="fuser -v /dev/nvidia* 2>&1 |grep -o '$USER.*'  | awk '{print \$2}' | xargs -I {} sh -c 'echo Killing {}; kill {} || echo failed'"
}
alias kill-forkserver="ps aux | grep 'forkserver|spawn_main' | grep -v grep | awk '{print \$2}' | xargs -I {} sh -c 'echo Killing {}; kill {} || echo failed'"

function b(){
  =acpi -V | head -n1
  sensors | grep Physical
  sensors | grep RPM
  cat /proc/acpi/ibm/fan | head -n3 |tail -n1
  for ((i=0; i<1; i++)); do
    echo -n "cpu$i : "
    cat "/sys/devices/system/cpu/cpu$i/cpufreq/scaling_governor"
  done
  sudo cpupower frequency-info -w | sed 'N;s/\n//g'
}

# softwares
alias mathematica='/opt/Mathematica/Executables/Mathematica -nosplash'
which matlab NN || {
  [[ -d /opt/Matlab ]] && alias matlab='/opt/Matlab/bin/matlab'
} && {
  alias matlabc='matlab -nodisplay -r clc '

  # fix matlab on archlinux
  export J2D_D3D="false"
  [[ -d /usr/lib/jvm/java-8-openjdk/jre ]] && export MATLAB_JAVA=/usr/lib/jvm/java-8-openjdk/jre
}
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

alias wine32='WINEARCH=win32 LC_ALL=zh_CN.utf-8 WINEPREFIX=~/.wine32 wine'
alias net9='luit -encoding gb18030 -- ssh ppwwyyxx@bbs.net9.org'
alias smth='luit -encoding gb18030 -- ssh -1 ppwwyyxx@bbs.smth.org'
alias freenode='weechat-curses -r "/connect freenode"'  # /join #tuna
alias baidupan='/opt/deepinwine/apps/Deepin-BaiduNetDisk/run.sh'
alias wechat='/opt/deepinwine/apps/Deepin-WeChat/run.sh'

# media
alias idf='identify'
alias dot='dot -Tpng -O -v'
alias 2pdf='libreoffice --headless --convert-to pdf' # unoconv -f pdf
alias 2csv='libreoffice --headless --convert-to csv'

function mirror() {
  device=${1:-/dev/video0}
  mplayer -tv driver=v4l2:device=$device tv:// -vf-add mirror
}
function webcam-to-v4l2 () {
  # https://github.com/bluezio/ipwebcam-gst/
  URL=$1
  device=$2
  gst-launch-1.0 souphttpsrc \
    location=http://$URL \
    do-timestamp=true is-live=true \
      '!' queue  '!' multipartdemux '!' decodebin '!' videoconvert '!' videoscale '!' \
      video/x-raw,format=YUY2 '!' v4l2sink device=$device
}
alias tune-pitch='mplayer -af scaletempo=speed=pitch'
alias record='ffmpeg -f alsa -ac 1 -i pulse -f x11grab -s 1366x768 -r 40 -show_region 1 -i :0.0 ~/Video/out.mpg'
alias ffprobe='ffprobe -hide_banner'
m_sub_param='-subcp utf-8 -subfont-text-scale 2.5 -subfont "/usr/share/fonts/wenquanyi/wqy-microhei/wqy-microhei.ttc"'
m_avc_param="-oac mp3lame -lameopts fast:preset=medium -ovc x264 -x264encopts subq=5:8x8dct:frameref=2:bframes=3:weight_b:threads=auto"
f_avc_param_old="-c:v libx264 -preset slow -crf 23 -c:a libmp3lame"
#f_avc_param="-map 0 -c:v libx265 -preset medium -x265-params crf=28 -c:a aac -strict experimental -b:a 128k"
f_avc_param="-map 0 -c:v libx265 -preset medium -x265-params crf=25 -c:a copy"
f_avc_param_apple="$f_avc_param_old -pix_fmt yuv420p"
function ffmpeg_compress() {
  if [[ -n $2 ]]; then
    ffmpeg -i "$1" `echo $f_avc_param` -vf subtitles=$2 $1.mkv
  else
    ffmpeg -i "$1" `echo $f_avc_param` -c:s copy $1.mkv
  fi
}
function mencoder_compress() { mencoder "$1" -o $1.avi `echo $m_avc_param` }
function ffmpeg_audio() { ffmpeg -i "$1" -vn "${1%.*}".mp3}
# https://www.reddit.com/r/ffmpeg/comments/lokjhs/how_do_you_check_the_audio_level_inside_a_video/
function ffmpeg_loudnorm() { ffmpeg -i "$1" -filter:a 'loudnorm=i=-24' -c:v copy -c:a aac "${1%.*}"_loudnorm.mkv}
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


[[ -n $_CFG_ON_MAC ]] && {
  alias top="htop"
  alias topme='htop -u $USER'
} || {
  local top_version=$(=top -h 2>/dev/null | head -n1 | grep -o '[0-9]*$')
  if [[ "$top_version" -ge 10 ]]; then
    alias top='top -d 0.5 -o %CPU -c'
  else
    alias top='top -d 0.5 -c'
  fi
  alias topme='top -u $USER'
}
alias htopme='htop -u $USER'
alias psmem="ps aux|awk '{print \$4\"\\t\"\$11}'|grep -v MEM|sort -n | tail -n20"
function memgrep() { grep VmHWM /proc/$(pgrep -d '/status /proc/' "$1")/status; }
function killz() {
  ppid=$(ps -oppid $1 | tail -n1 | sed 's/ //g')
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

function send-event() {
  local name=$1
  local fname=/t/my_msg_fifo_$name
  mkfifo $fname 2>/dev/null || true
  bash -c "echo Done > $fname" &
}

function wait-event() {
  local name=$1
  local fname=/t/my_msg_fifo_$name
  mkfifo $fname 2>/dev/null || true
  cat $fname > /dev/null
  =rm $fname
}

# python
alias ipy='ipython'
function pydbg () { ipython --pdb -c "%run $1" }
alias piu='pip install --user'
alias piu2='pip2 install --user'
alias piu3='pip3 install --user'
alias pyftp='python3 -m pyftpdlib'
function pylibinfo() {
  if [[ -z "$1" ]]; then echo "Usage: pylibinfo libname"; return; fi
  python -c "import $1 as X; print(X.__file__, end=' '); print(X.__version__)"
}
function pytwistd() { twistd web --path "$1" -p tcp:"${2:-8000}" }
alias pipup="pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install --user -U"
alias unquote='python3 -c "import sys, urllib.parse as up; [sys.stdout.write(up.unquote(l)) for l in sys.stdin]"'

# tensorflow/torch
function tf-verbose() { export TF_CPP_MIN_LOG_LEVEL=0; export TF_CPP_MIN_VLOG_LEVEL=10 }
function uninstall-tf() {
  for p in tensorflow tensorflow-gpu tf-nightly tf-nightly-gpu tensorflow-tensorboard tensorboard tb-nightly; do
    pip uninstall $p -y
  done
}
function uninstall-pt() {
  for p in torch torchvision torch-nightly detectron2; do
    pip uninstall $p -y
  done
}

# package; https://github.com/icy/pacapt
which pacman NN && {
  alias pS='paru -S'
  alias pU='sudo pacman -U'
  alias pSs='paru -Ss'
  alias pSi='pacman -Si'
  alias pQo='pacman -Qo'
  alias pSy='sudo pacman -Syy'
  alias pR='sudo pacman -R'
  alias pSu='paru -Syu'
  alias pQl='pacman -Ql'
  alias pScc='sudo pacman -Scc'
  alias paur='pacman -Qem'
  alias pQq='pacman -Q'
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
        alias pSu='sudo apt update; sudo apt upgrade'
        alias pQq='apt list --installed'
    } || {
        alias pS='sudo aptitude install'
        alias pR='sudo aptitude purge'
        alias pSs='aptitude search'
        alias pSy='sudo aptitude update'
        alias pSu='sudo aptitude upgrade'
    }
    which apt-file NN && {
        alias pQo='apt-file search'
    } || {
        alias pQo='dpkg -S'    # only works for already-installed packages
    }
    alias pQl='dpkg-query -L'
    alias pU='sudo dpkg -i'
    alias pScc='sudo apt-get clean'
} || {
  alias pS='sudo yum install'
  alias pR='sudo yum erase'
  alias pSy='sudo yum check-update'
  alias pSu='sudo yum update'
  alias pSs='yum search'
  alias pQo='yum whatprovides'
  alias pQl='rpm -ql'
  alias pU='yum localinstall'
}
}
}


# Command not found
which pkgfile NN && {
  # Archlinux
  function command_not_found_handler() {
    local command="$1"
    # avoid recursive command-not-found when /usr/bin/ is mistakenly lost in PATH
    [ -x /usr/bin/fortune ] && [ -x /usr/bin/cowthink ] && {
      /usr/bin/fortune chinese | /usr/bin/cowthink -W 70
    }
    [ -n "$command" ] && [ -x /usr/bin/pkgfile ] && {
      echo -e "searching for \"$command\" in repos..."
      local pkgs="$(/usr/bin/pkgfile -b -v -- "$command")"
      if [ ! -z "$pkgs" ]; then
        echo -e "\"$command\" may be found in the following packages:\n\n${pkgs}\n"
      fi
    }
    return 1
  }
}


# weird personal stuff
function tpgrep() {
# a function to grep tensorpack logs
# $1: string to grep
# $2+: dirs or logs
  [[ -n "$1" ]] || return 1
  local pat=$1
  shift
  local cmd="paste"
  for d in ${@}; do
    [[ -d "$d" ]] && {
      cmd="$cmd <(echo $d; cat "$d/log.log" | grep '$pat' | awk-last)"
    } || {
      cmd="$cmd <(echo $d; cat "$d" | grep '$pat' | awk-last)"
    }
  done
  eval $cmd
}
function ptgrep() {
# A function to parse json metrics produced by detectron2
# $1: string to grep
# $2+: json logs
  [[ -n "$1" ]] || return 1
  local pat="$1"
  local cmd="paste"
  for d in ${@:2}; do
    [[ -d "$d" ]] && {
      if [[ -d "$d/output" ]]; then
        cmd="$cmd <(cat \"$d/output/metrics.json\" | jq -r '.\"$pat\"')"
      else
        cmd="$cmd <(cat \"$d/metrics.json\" | jq -r '.\"$pat\"')"
      fi
    } || {
      cmd="$cmd <(cat \"$d\" | jq -r '.\"$pat\"')"
    }
  done
  eval $cmd
}
