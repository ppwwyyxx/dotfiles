#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
A general curve plotter to create curves such as:
https://github.com/ppwwyyxx/tensorpack/tree/master/examples/ResNet

A simplest example:
$ cat examples/train_log/mnist-convnet/stat.json \
        | jq '.[] | .train_error, .validation_error' \
        | paste - - \
        | plot-point.py --legend 'train,val' --xlabel 'epoch' --ylabel 'error'

For more usage, see `plot-point.py -h` or the code.
"""

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.font_manager as fontm
import argparse
import sys
from collections import defaultdict
from itertools import chain
import six

# http://jonathansoma.com/lede/data-studio/matplotlib/exporting-from-matplotlib-to-open-in-adobe-illustrator/
matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42
# matplotlib.rcParams["font.family"] = "Libre Baskerville"
matplotlib.rcParams["font.family"] = "Arial"
# rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
# rc('font',**{'family':'sans-serif','sans-serif':['Microsoft Yahei']})
# rc('text', usetex=True)

STDIN_FNAME = '-'


def get_args():
    description = "plot points into graph."
    parser = argparse.ArgumentParser(description=description)
    # IO related:
    parser.add_argument('-i', '--input',
                        help='input data file, use "-" for stdin. Default stdin. Input \
            format is many rows of DELIMIETER-separated data',
                        default='-')
    parser.add_argument('-d', '--delimeter',
                        help='column delimeter', default='\t')
    parser.add_argument('-o', '--output',
                        help='output image', default='')
    parser.add_argument('--show',
                        help='show the figure after rendered',
                        action='store_true')

    # Data related:
    parser.add_argument('-c', '--column',
                        help="describe each column in data, for example 'x,y,y'. \
            Default to 'y' for one column and 'x,y' for two columns. \
            Plot attributes can be appended after 'y', like 'ythick;cr'. \
            By default, assume all columns are y. \
            ")
    parser.add_argument('--scale',
                        help='scale of each column (either x or y), separated by comma')
    parser.add_argument('--decay',
                        help='exponential decay rate to smooth Y',
                        type=str, default='0')
    parser.add_argument('--units',
                        help='recognize common units such as K, M, G, T',
                        action='store_true')

    # Text related:
    parser.add_argument('-t', '--title',
                        help='title of the graph',
                        default='')
    parser.add_argument('--xlabel',
                        help='x label', type=six.text_type)
    parser.add_argument('--ylabel',
                        help='y label', type=six.text_type)
    parser.add_argument('--legend',
                        help='legend for each y')
    parser.add_argument('--font-size-large', type=int, default=30,
                        help='font size for title')
    parser.add_argument('--font-size-medium', type=int, default=20,
                        help='font size for legend and labels')
    parser.add_argument('--font-size-small', type=int, default=15,
                        help='font size for ticks')

    # Display:
    parser.add_argument('--aspect-ratio', type=float, default=1.618,
            help='aspect ratio (x / y)')
    parser.add_argument('--xlim',
                        help='x lim', type=float, nargs=2)
    parser.add_argument('--ylim',
                        help='y lim', type=float, nargs=2)
    parser.add_argument('--num-xtick',
                        help='number of x ticks', type=int, default=10)
    parser.add_argument('--xticks', nargs='+')
    parser.add_argument('--annotate-maximum',
                        help='annonate maximum value in graph',
                        action='store_true')
    parser.add_argument('--annotate-minimum',
                        help='annonate minimum value in graph',
                        action='store_true')
    parser.add_argument('--fill-alpha', type=float, default=0.1)
    parser.add_argument('--xkcd',
                        help='xkcd style',
                        action='store_true')

    global args
    args = parser.parse_args()

    if not args.show and not args.output:
        args.show = True


def parse_val(val: str) -> float:
    if not args.units:
        return float(val)
    val = val.lower()
    if val.endswith("k"):
        return float(val[:-1]) * 1e3
    if val.endswith("m"):
        return float(val[:-1]) * 1e6
    if val.endswith("g"):
        return float(val[:-1]) * 1e9
    if val.endswith("t") or val.endswith("b"):
        return float(val[:-1]) * 1e12
    return float(val)


def read_entire_matrix():
    headers = None
    # parse input args
    headers = None
    if args.input == STDIN_FNAME:
        fin = sys.stdin
    else:
        fin = open(args.input)
    all_lines = fin.readlines()
    if args.input != STDIN_FNAME:
        fin.close()

    nr_column = len(all_lines[0].rstrip('\n').split(args.delimeter))
    # read the entire matrix to 'data'
    data = [[] for _ in range(nr_column)]
    ended = defaultdict(bool)
    for lineno, line in enumerate(all_lines):
        line = line.rstrip('\n').split(args.delimeter)
        assert len(line) <= nr_column, \
            """One row have too many columns (separated by {})!
Line: {}""".format(repr(args.delimeter), line)
        for idx, val in enumerate(line):
            if val == '':
                ended[idx] = True
                continue
            else:
                try:
                    val = parse_val(val)
                except Exception:
                    if lineno == 0:
                        # header
                        headers = line
                        break
                    else:
                        raise
                else:
                    assert not ended[idx], "Column {} has hole!".format(idx)
                    data[idx].append(val)
    return data, headers


class Sequence(object):
    def __init__(self, xs, ys, plot_args=None):
        """
        Args:
            xs, ys: a list of floats
        """
        self.xs = np.copy(np.asarray(xs))
        self.ys = np.asarray(ys)

        assert len(xs) >= len(ys), \
            "x column is shorter than y column! {} < {}".format(len(xs), len(ys))
        self.xs = self.xs[:len(ys)]

        if plot_args is None:
            plot_args = {}
        self.plot_args = plot_args
        self.fill_color = self.plot_args.pop("fill", True)
        self.legend = None

        self.drawables = []
        self._legend_line = None

    @property
    def legend_line(self):
        return self._legend_line

    @legend_line.setter
    def legend_line(self, val):
        val.set_picker(5)
        self._legend_line = val

    def exponential_smooth(self, alpha):
        """ smooth data by alpha."""
        data = self.ys
        ret = np.copy(data)
        now = data[0]
        for k in range(len(data)):
            ret[k] = now * alpha + data[k] * (1 - alpha)
            now = ret[k]
        self.ys = ret

    def scale_y(self, scale):
        if scale == 1.0:
            return
        self.ys *= scale
        if self.legend:
            self.legend = "{},scaley={:.2g}".format(self.legend, scale)

    @property
    def xrange(self):
        return np.array([min(self.xs), max(self.xs)])

    def toggle_vis(self):
        assert len(self.drawables), "Called before plot()!"
        vis = not self.drawables[0].get_visible()
        for d in self.drawables:
            d.set_visible(vis)
        self.legend_line.set_alpha(1.0 if vis else 0.2)
        return vis


def annotate_min_max(data_x, data_y, ax):
    """
    Annotate on top of ax, given one sequence of X and Y.
    """

    def filter_valid_range(points, rect):
        """rect = (min_x, max_x, min_y, max_y)"""
        ret = []
        for x, y in points:
            if x >= rect[0] and x <= rect[1] and y >= rect[2] and y <= rect[3]:
                ret.append((x, y))
        if len(ret) == 0:
            ret.append(points[0])
        return ret

    max_x, min_x = max(data_x), min(data_x)
    max_y, min_y = max(data_y), min(data_y)
    x_range = max_x - min_x
    y_range = max_y - min_y
    x_max, y_max = data_y[0], data_y[0]
    x_min, y_min = data_x[0], data_y[0]

    for i in range(1, len(data_x)):
        if data_y[i] > y_max:
            y_max = data_y[i]
            x_max = data_x[i]
        if data_y[i] < y_min:
            y_min = data_y[i]
            x_min = data_x[i]

    rect = ax.axis()
    if args.annotate_maximum:
        text_x, text_y = filter_valid_range([
            (x_max + 0.05 * x_range,
                y_max + 0.025 * y_range),
            (x_max - 0.05 * x_range,
                y_max + 0.025 * y_range),
            (x_max + 0.05 * x_range,
                y_max - 0.025 * y_range),
            (x_max - 0.05 * x_range,
                y_max - 0.025 * y_range)],
            rect)[0]
        ax.annotate('maximum ({:d},{:.3f})' . format(int(x_max), y_max),
                    xy=(x_max, y_max),
                    xytext=(text_x, text_y),
                    arrowprops=dict(arrowstyle='->'))
    if args.annotate_minimum:
        text_x, text_y = filter_valid_range([
            (x_min + 0.05 * x_range,
                y_min - 0.025 * y_range),
            (x_min - 0.05 * x_range,
                y_min - 0.025 * y_range),
            (x_min + 0.05 * x_range,
                y_min + 0.025 * y_range),
            (x_min - 0.05 * x_range,
                y_min + 0.025 * y_range)],
            rect)[0]
        ax.annotate('minimum ({:d},{:.3f})' . format(int(x_min), y_min),
                    xy=(x_min, y_min),
                    xytext=(text_x, text_y),
                    arrowprops=dict(arrowstyle='->'))
        # ax.annotate('{:.3f}' . format(y_min),
        # xy = (x_min, y_min),
        # xytext = (text_x, text_y),
        # arrowprops = dict(arrowstyle = '->'))


def plot_args_from_column_desc(desc):
    if not desc:
        return {}
    ret = {}
    desc = desc.split(';')
    if 'thick' in desc:
        ret['lw'] = 5
    if 'dash' in desc:
        ret['ls'] = (0, (5, 4))
    if 'nofill' in desc:
        ret['fill'] = False
    for v in desc:
        if v.startswith('c'):
            ret['color'] = v[1:]
        if v.startswith('m'):
            ret['marker'] = v[1:]
    return ret


def do_plot(seqs):
    """
    seqs: [Sequence]
    """
    fig = plt.figure(figsize=(8 * args.aspect_ratio, 8))
    if args.output:
        # tight image for output
        ax = fig.add_axes((0,0,1,1))
    else:
        ax = fig.add_axes((0.1, 0.1, 0.8, 0.8))

    for seq in seqs:
        curve_obj = plt.plot(seq.xs, seq.ys, label=seq.legend, **seq.plot_args)[0]
        seq.drawables.append(curve_obj)

        if seq.fill_color:
            c = curve_obj.get_color()
            fill_obj = plt.fill_between(seq.xs, seq.ys, alpha=args.fill_alpha, facecolor=c)
            seq.drawables.append(fill_obj)

        if args.annotate_maximum or args.annotate_minimum:
            annotate_min_max(seq.xs, seq.ys, ax)

    # deal with label and xlim
    if args.xlabel:
        plt.xlabel(args.xlabel, fontsize=args.font_size_medium)
    if args.ylabel:
        plt.ylabel(args.ylabel, fontsize=args.font_size_medium)
    if args.xlim:
        plt.xlim(args.xlim[0], args.xlim[1])
    else:
        # adjust maxx
        all_xrange = np.asarray([s.xrange for s in seqs])
        minx, maxx = min(all_xrange[:, 0]), max(all_xrange[:, 1])
        new_maxx = maxx + (maxx - minx) * 0.05
        plt.xlim(minx, new_maxx)
    if args.xticks:
        # only work for the default xs (1..N). does not work if data contains xs
        plt.xticks(list(range(1, len(args.xticks) + 1)), args.xticks)
    if args.ylim:
        plt.ylim(args.ylim[0], args.ylim[1])

    legend_obj = plt.legend(loc='best', fontsize=args.font_size_medium)

    if legend_obj is not None:  # when legend is disabled, this is None
        for legend_line, seq in zip(legend_obj.get_lines(), seqs):
            seq.legend_line = legend_line

        if len(seqs) > 10:
            for seq in seqs:
                seq.toggle_vis()

        legend_line_to_seq = {seq.legend_line: seq for seq in seqs}

        def onclick(event):
            legline = event.artist
            seq = legend_line_to_seq[legline]
            vis = seq.toggle_vis()
            fig.canvas.draw()

        fig.canvas.mpl_connect('pick_event', onclick)

    for label in chain.from_iterable(
            [ax.get_xticklabels(), ax.get_yticklabels()]):
        label.set_fontproperties(fontm.FontProperties(size=args.font_size_small))

    ax.locator_params(nbins=args.num_xtick, axis='x')
    ax.tick_params(direction='in')

    ax.grid(color='#dfdfdf', linestyle='dashed', axis='y')

    plt.title(args.title, fontdict={'fontsize': args.font_size_large})
    if args.output != '':
        plt.savefig(args.output, bbox_inches='tight', transparent=True)
    if args.show:
        plt.show()


def main():
    get_args()
    data, headers = read_entire_matrix()     # #col x #row

    if args.scale:
        scales = list(map(float, args.scale.split(',')))
        if len(scales) == 1:
            scales = scales * len(data)
        assert len(scales) == len(data)
        for scale, col in zip(scales, data):
            for i, ele in enumerate(col):
                col[i] *= scale

    # parse column format
    nr_column = len(data)
    if args.column is None:
        column = ['y'] * nr_column
    else:
        column = args.column.strip().split(',')
    for k in column:
        assert k[0] in ['x', 'y', 'n']
    assert nr_column == len(column), "Column and data doesn't have same length. {}!={}".format(nr_column, len(column))

    # split data into Xs and Ys
    data_xs, data_ys, desc_ys = [], [], []   # #col x #row
    for column_data, column_desc in zip(data, column):
        if column_desc[0] == 'y':
            data_ys.append(column_data)
            desc_ys.append(column_desc)
        elif column_desc[0] == 'x':
            data_xs.append(column_data)
    num_curve = len(data_ys)
    length_ys = [len(t) for t in data_ys]
    print("Length of each Y column:", length_ys)

    # populate default xs
    if len(data_xs) > 1:
        assert len(data_xs) == num_curve, \
            "If multiple x columns are used, num_x_column must equals to nr_y_column"
    elif len(data_xs) == 1:
        data_xs = data_xs * num_curve
    else:
        data_xs = [list(range(1, max(length_ys) + 1))] * num_curve

    # put into seq
    seqs = []
    assert len(data_xs) == len(data_ys)
    for idx, (X, Y) in enumerate(zip(data_xs, data_ys)):
        col_desc = desc_ys[idx]
        seqs.append(Sequence(
            X, Y,
            plot_args=plot_args_from_column_desc(col_desc[1:])))

    if ',' not in args.decay:
        decay = [float(args.decay)] * len(seqs)
    else:
        decay = [float(x) for x in args.decay.split(',')]
    for dec, s in zip(decay, seqs):
        if dec != 0:
            s.exponential_smooth(dec)

    legends = None
    if args.legend:
        for sep in [',', ';']:
            if args.legend.count(sep) == num_curve - 1:
                legends = args.legend.split(sep)
                break
        else:
            raise ValueError(f"data has {num_curve} columns but legend is {args.legend}")
    elif headers is not None and len(headers):
        legends = headers
    if legends is not None:
        assert len(legends) == num_curve
        for legend, seq in zip(legends, seqs):
            seq.legend = legend

    if args.xkcd:
        with plt.xkcd():
            do_plot(seqs)
    else:
        do_plot(seqs)


if __name__ == '__main__':
    main()
