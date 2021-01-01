#!/usr/bin/env python

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
    import numpy as np
    np.set_printoptions(linewidth=120)
    import cv2
    import scipy
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
