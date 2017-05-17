#!/bin/bash -e
# File: dumps.sh
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

gconftool-2 --dump /apps/guake > apps-guake.xml
