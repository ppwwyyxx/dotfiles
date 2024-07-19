# Config TF & PyTorch
export TF_NEED_GCP=0
export TF_NEED_ROCM=0
export TF_NEED_HDFS=0
export TF_NEED_OPENCL=0
export TF_NEED_JEMALLOC=1
export TF_ENABLE_XLA=1
export TF_NEED_VERBS=0
export TF_NEED_AWS=0
export TF_NEED_KAFKA=0
export TF_SET_ANDROID_WORKSPACE=0
export TF_NEED_MPI=0
export TF_NEED_GDR=0
export TF_NEED_S3=0
export TF_NEED_OPENCL_SYCL=0
export TF_NEED_COMPUTECPP=0
export TORCH_SHOW_CPP_STACKTRACES=1

# Config MKL / CUDA
if [[ -d /opt/intel/mkl ]]; then
  export MKLROOT=/opt/intel/mkl
  export LD_LIBRARY_PATH=$MKLROOT/lib/intel64:$LD_LIBRARY_PATH
  export LIBRARY_PATH=$MKLROOT/lib/intel64:$LIBRARY_PATH
fi
function try_use_cuda_home() {
  if [[ -d "$1/lib64" ]]; then
    export CUDA_HOME="$1"
    export LD_LIBRARY_PATH=$CUDA_HOME/lib64:$LD_LIBRARY_PATH
    if [[ -d "$CUDA_HOME/extras/CUPTI/lib64" ]]; then
      export LD_LIBRARY_PATH=$CUDA_HOME/extras/CUPTI/lib64:$LD_LIBRARY_PATH
    fi
    export LIBRARY_PATH=$CUDA_HOME/lib64:$LIBRARY_PATH
    export PATH=$PATH:$CUDA_HOME/bin
  fi
}
function try_use_cudnn() {
  if [[ -d "$1/lib64" ]]; then
    export LD_LIBRARY_PATH=$1/lib64:$LD_LIBRARY_PATH
    export LIBRARY_PATH=$1/lib64:$LIBRARY_PATH
    # for pytorch build
    export CUDNN_ROOT_DIR=$1
    export CUDNN_INCLUDE_DIR=$1/include
    export CUDNN_LIB_DIR=$1/lib64
  fi
}
try_use_cuda_home /usr/local/cuda
try_use_cuda_home /opt/cuda    # ArchLinux
try_use_cudnn /usr/local/cudnn


# Config misc languages
export GOPATH=$HOME/.local/gocode
safe_export_path $GOPATH/bin

export NODE_PATH=$HOME/.local/lib/node_modules/

safe_export_path $HOME/.cabal/bin
safe_export_path $HOME/.cargo/bin
safe_export_path /opt/intel/bin
safe_export_path /usr/lib/colorgcc/bin
safe_export_path $HOME/.rvm/bin		# Add RVM to PATH for scripting

# https://issuetracker.google.com/issues/353554174
# export PYTHONSAFEPATH=1
[[ -s ~/.config/python/startup.py ]] && export PYTHONSTARTUP=~/.config/python/startup.py

# Config Less & SDCV
export LESS_TERMCAP_mb=$YELLOW
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
export SDCV_PAGER="sed 's/\ \ \([1-9]\)/\n\n◆\1/g' |less"
