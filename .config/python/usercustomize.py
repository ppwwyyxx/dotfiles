#!/usr/bin/env python2
# -*- coding: UTF-8 -*-
# File: usercustomize.py
# Date: Wed Apr 01 19:33:10 2015 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

import sys, inspect, os

from IPython.core import ultratb
from IPython.terminal.embed import InteractiveShellEmbed
import termcolor

colorhook = ultratb.FormattedTB(mode='Plain',
     color_scheme='Linux', call_pdb=0)

def debughook(type, value, tb):
   if hasattr(sys, 'ps1') or not sys.stderr.isatty():
      # we are in interactive mode or we don't have a tty-like
      # device, so we call the default hook
      sys.__excepthook__(type, value, tb)
   else:
      # we are NOT in interactive mode, print the exception...
      #traceback.print_exception(type, value, tb)
      colorhook(type, value, tb)

      frame = tb.tb_next.tb_frame
      sh = InteractiveShellEmbed(
          banner1=termcolor.colored(
              "Custom Debug IPython Shell:\n", 'red'))
      sh.confirm_exit = False
      sh.mainloop(local_ns=frame.f_locals, global_ns=frame.f_globals)

      # ...then start the debugger in post-mortem mode.
      #import pdb; pdb.pm()

sys.excepthook = debughook
