#!/usr/bin/env python2
# -*- coding: UTF-8 -*-
# File: .startup.py
# Date: Wed Apr 01 16:49:16 2015 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

# stdlib
import sys, os
import shutil
import gzip

import itertools
from itertools import *
import operator
from operator import *
import collections
from collections import defaultdict, Counter

import inspect, functools
import time
from datetime import datetime
import re, string
from pprint import pprint

# python 2/3 compatibility
if sys.version_info.major == 2:
    import readline
    import rlcompleter
    readline.parse_and_bind("tab: complete")
    import cPickle as pickle
else:
    import pickle

try:
    import scipy
    import numpy as np
    import cv2
    from cv2 import imread, imwrite, imshow
    import h5py
except:
    pass

try:
    import matplotlib.pyplot as plt
    def show_img_sync(img):
        if img.shape[0] == 3 and len(img.shape) == 3 and img.shape[2] != 3:
            n = np.zeros((img.shape[1], img.shape[2], 3))
            for k in range(3):
                n[:,:,k] = img[k]
            img = n
        plt.imshow(img)
        plt.show()
except:
    pass
