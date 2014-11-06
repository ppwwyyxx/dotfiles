#!/usr/bin/env python2
# -*- coding: UTF-8 -*-
# File: .startup.py
# Date: Thu Nov 06 20:52:50 2014 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

try:
    import readline
except ImportError:
    print "Module readline not available."
else:
    import rlcompleter
    readline.parse_and_bind("tab: complete")

try:
    import scipy
    import numpy as np
    import cv2
    from cv2 import imread, imwrite, imshow
except:
    pass

import cPickle as pickle
import gzip
import itertools
import operator
from itertools import *
from operator import *

import sys
import os
import shutil

import time
from datetime import datetime
from collections import defaultdict
import re
import string

from pprint import pprint
import __builtin__
def displayhook(value):
    if value is not None:
        __builtin__._ = value
        pprint(value)
sys.displayhook = displayhook
