import gdb
import sys
import cv2
import numpy as np

CV_8U = 0
CV_8S = 1
CV_16U = 2
CV_16S = 3
CV_32S = 4
CV_32F = 5
CV_64F = 6
CV_USRTYPE1 = 7
CV_CN_MAX = 512
CV_CN_SHIFT = 3
CV_MAT_CN_MASK = (CV_CN_MAX - 1) << CV_CN_SHIFT
CV_DEPTH_MAX = (1 << CV_CN_SHIFT)
CV_MAT_DEPTH_MASK = CV_DEPTH_MAX - 1
IPL_DEPTH_SIGN = 0x80000000


class ImgShowCommand(gdb.Command):

    def __init__(self):
        super(ImgShowCommand, self).__init__("imshow",
                                             gdb.COMMAND_DATA,
                                             gdb.COMPLETE_SYMBOL)

    def decode_flag(self, flags):
        channel = (((flags) & CV_MAT_CN_MASK) >> CV_CN_SHIFT) + 1
        depth = (flags) & CV_MAT_DEPTH_MASK
        cv_elem_size = (((4 << 28) | 0x8442211) >> depth * 4) & 15
        if (depth == CV_8S or depth == CV_16S or depth == CV_32S):
            mask = IPL_DEPTH_SIGN
        else:
            mask = 0
        ipl_depth = cv_elem_size * 8 | mask
        return channel, depth

    def invoke(self, arg, from_tty):
        args = gdb.string_to_argv(arg)

        v = gdb.parse_and_eval(args[0])
        cols, rows = int(v['cols']), int(v['rows'])
        channel, depth = self.decode_flag(v['flags'])
        if depth != CV_8U:
            print("support CV_8U only")
            return

        # conver the v['data'] type to "char*" type
        char_type = gdb.lookup_type("char")
        char_pointer_type = char_type.pointer()
        buffer_ptr = v['data'].cast(char_pointer_type)

        # read bytes from inferior's memory, because
        # we run the opencv-python module in GDB's own process
        # otherwise, we use memory corss processes
        buf = v['step']['buf']
        bytes_cnt = buf[0] * v['rows']
        inferior = gdb.selected_inferior()
        mem = inferior.read_memory(buffer_ptr, bytes_cnt)

        img = np.frombuffer(mem, dtype='uint8', count=int(bytes_cnt))
        img = img.reshape(rows, cols, channel)

        # cv2.startWindowThread()
        cv2.namedWindow('viewer')
        cv2.imshow('viewer', img)
        cv2.waitKey(0)
        cv2.destroyWindow('viewer')

ImgShowCommand()
