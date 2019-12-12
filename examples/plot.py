#!/usr/bin/python

import sys, os

import re

from itertools import cycle

import matplotlib
matplotlib.use('PDF')

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.lines import Line2D

text_style = dict(fontsize='xx-large', fontdict={'family': 'monospace'})

from pint import UnitRegistry
ureg = UnitRegistry()

from quantulum import parser

def plotter(ax, data1, list_data2, param_dict):
    """
    Helper function from matplotlib.org/tutorials/introductory/usage.html

    Parameters
    ----------
    ax : Axes
        The axes to draw to

    data1 : array
        The x data

    data2 : array array
        The y data

    param_dict : dict
        Dictionary of kwargs to pass to ax.plot

    Returns
    -------
    out : list
        list of artists added
    """
    out = []
    unfilled_markers = cycle(['x', 'd', 'o', 's'])
    for data2, mm in zip (list_data2, unfilled_markers):
        out.extend(ax.plot(data1, data2, marker=mm, **param_dict))

    return out

def main(argv):
    ninputs = len(argv)
    if ninputs <= 2:
        print "Usage: ", os.path.basename(sys.argv[0]), " <max_cores> <out_file> <time_file>"
        sys.exit(-1)
    in_fp_file = sys.argv[3]
    out_filepath = sys.argv[2]

    in_fp = open(in_fp_file, "r")

    current_K = None
    current_S = None
    search_K  = re.compile('K: (.+)$')
    search_S  = re.compile('size: (.+)')
    search_T  = re.compile('mean: (\d+\.\d+)')
    search_D  = re.compile('stddev: (\d+\.\d+)')

    data = {}
    ## Sequential data
    try:
        for line in in_fp:
            # sK = search_K.search(line)
            # if sK is not None:
            #     current_K = int(sK.group(1))
            #     seq_data[current_K] = {}

            sS = search_S.search(line)
            if sS is not None:
                current_S = int(sS.group(1))
                print "Current size: ", current_S
                data[current_S] = {}

            sK = search_K.search(line)
            if sK is not None:
                current_K = sK.group(1)
                print "Current K: ", current_K
                data[current_S][current_K] = (0, 0)

            sT = search_T.search(line)
            if sT is not None:
                current_T = float(sT.group(1))
                data[current_S][current_K] = (current_T, data[current_S][current_K][1])

            sD = search_D.search(line)
            if sD is not None:
                current_D = float(sD.group(1))
                data[current_S][current_K] = (data[current_S][current_K][0], current_D)
                print "Current T_D: ", data[current_S][current_K]
    except AttributeError:
        print "Invalid input files"
        sys.exit(-1)

    speedups = { int(ss): { int(kk) : data[ss]["seq"][0] / vv[0]
                        for kk, vv in ss_d.items()
                        if kk != "seq"
                        }
                    for ss, ss_d in data.items()
                }

    max_speedup = max ([ max ([ v for _, v in vv.items() ]) for _, vv in speedups.items() ])
    print max_speedup

    k_x = [ int(kk) for kk, _ in speedups[current_S].items() if kk != "seq" ]
    k_x.sort()
    sizes_x = [ ss for ss, _ in speedups.items() ]
    sizes_x.sort()
    nsizes = len(sizes_x) - 1
    sz1 = sizes_x[nsizes-6]
    sz2 = sizes_x[nsizes-3]
    sz3 = sizes_x[nsizes-1]
    sz4 = sizes_x[nsizes]
    k_sz1 = [ speedups[sz1][k] for k in k_x ]
    k_sz2 = [ speedups[sz2][k] for k in k_x ]
    k_sz3 = [ speedups[sz3][k] for k in k_x ]
    k_sz4 = [ speedups[sz4][k] for k in k_x ]
    fig, ax = plt.subplots(1, 1)
    # ax.set_ylabel('Speedup', **text_style)
    ax.set_xlabel('K', **text_style)
    ax.set_xscale('log', basex=2)
    ax.tick_params(axis='both', which='major', labelsize='x-large')

    markers = ['o', 'x', 's', 'h']

    plotter(ax, k_x , [ k_sz1, k_sz2, k_sz3 , k_sz4], {})
    # ax.set_title("+RTS -N8 ", **text_style)
    ax.set_ylim([0, max(k_sz1 + k_sz2 + k_sz3 + k_sz4) + 0.1])
    # ax.legend(["%.0e" % sz1,"%.0e" % sz2,"%.0e" % sz3,"%.0e" % sz4], prop={'size': 'x-large'} )
    ax.legend(["%.0e" % sz1,"%.0e" % sz2,"%.0e" % sz3,"%.0e" % sz4], prop={'size': 'x-large'} )

    fig.savefig(out_filepath + "_k", dpi=300)
    plt.close(fig)

    nks = len(k_x) - 1
    nsizes = len(sizes_x) - 1
    k1 = k_x[0]
    k2 = k_x[nks/3]
    k3 = k_x[nks-1]
    k4 = k_x[nks]
    k_sz1 = [ speedups[sz][k1] for sz in sizes_x ]
    k_sz2 = [ speedups[sz][k2] for sz in sizes_x ]
    k_sz3 = [ speedups[sz][k3] for sz in sizes_x ]
    k_sz4 = [ speedups[sz][k4] for sz in sizes_x ]
    fig, ax = plt.subplots(1, 1)
    ax.set_xscale('log', basex=2)
    # ax.set_ylabel('Speedup', **text_style)
    ax.set_xlabel('Size', **text_style)
    ax.tick_params(axis='both', which='major', labelsize='x-large')

    markers = ['o', 'x', 's', 'h']

    plotter(ax, sizes_x , [ k_sz1, k_sz2, k_sz3 , k_sz4], {})
    # ax.set_title("+RTS -N8 ", **text_style)
    ax.set_ylim([0, max(k_sz1 + k_sz2 + k_sz3 + k_sz4) + 0.1])
    ax.legend(['K ' + str(k1), 'K ' + str(k2), 'K ' + str(k3), 'K ' + str(k4)], prop={'size': 'x-large'} )

    fig.savefig(out_filepath + "_s", dpi=300)
    plt.close(fig)

    # k_x = range(1, 9)
    # sizes_x = [ int(ss) for ss, _ in data.items() ]
    # sizes_x.sort()
    # nsizes = len(sizes_x) - 1
    # sz1 = sizes_x[0]
    # sz4 = sizes_x[nsizes]
    # sz2 = sizes_x[nsizes / 3]
    # sz3 = sizes_x[nsizes / 2]
    # k_sz1 = [ speedups_kns[k][sz1] for k in k_x ]
    # k_sz2 = [ speedups_kns[k][sz2] for k in k_x ]
    # k_sz3 = [ speedups_kns[k][sz3] for k in k_x ]
    # k_sz4 = [ speedups_kns[k][sz4] for k in k_x ]

    # fig, ax = plt.subplots(1, 1)
    # # ax.set_ylabel('Speedup', **text_style)
    # ax.set_xlabel('K', **text_style)
    # ax.tick_params(axis='both', which='major', labelsize='x-large')

    # markers = ['o', 'x', 's', 'h']

    # plotter(ax, k_x , [ k_sz1, k_sz2, k_sz3 , k_sz4], {})
    # ax.set_title("+RTS -N8 ", **text_style)
    # ax.set_ylim([min(k_sz1 + k_sz2 + k_sz3 + k_sz4) - 0.1, max(k_sz1 + k_sz2 + k_sz3 + k_sz4) + 0.1])
    # ax.legend(["%.0e" % sz1,"%.0e" % sz2,"%.0e" % sz3,"%.0e" % sz4], prop={'size': 'x-large'} )

    # fig.savefig(out_filepath + "2", dpi=300)
    # plt.close(fig)
#
#     ## speedups vs size, for 4 different K values, fixed +RTS -N
#
#     rtsn = 8
#     sizes_x = [ ss for ss, _ in seq_data[1].items() ]
#     sizes_x.sort()
#     k1 = 1
#     k2 = 2
#     k3 = 6
#     k4 = 8
#     sz_k1 = [ speedups_kns[k1][rtsn][sz] for sz in sizes_x ]
#     sz_k2 = [ speedups_kns[k2][rtsn][sz] for sz in sizes_x ]
#     sz_k3 = [ speedups_kns[k3][rtsn][sz] for sz in sizes_x ]
#     sz_k4 = [ speedups_kns[k4][rtsn][sz] for sz in sizes_x ]
#
#     fig, ax = plt.subplots(1, 1)
#     # ax.set_ylabel('Speedup', **text_style)
#     ax.set_xlabel('Length', **text_style)
#     ax.set_xscale('log')
#     ax.tick_params(axis='both', which='major', labelsize='x-large')
#
#     markers = ['o', 'x', 's', 'h']
#
#     plotter(ax, sizes_x , [ sz_k1, sz_k2, sz_k3 , sz_k4], {})
#     ax.set_title("+RTS -N" + str(rtsn), **text_style)
#     ax.set_ylim([min(sz_k1 + sz_k2 + sz_k3 + sz_k4) - 0.1, max(sz_k1 + sz_k2 + sz_k3 + sz_k4) + 0.1])
#     ax.legend(['K' + str(k1), 'K'+ str(k2), 'K' + str(k3), 'K' + str(k4)], prop={'size': 'x-large'} )
#
#     fig.savefig(out_filepath + "3", dpi=300)
#     plt.close(fig)
#
#     ## speedups of +RTS -N

# ./plot.py out Base/Measurements/FFT_seq.time K0/Measurements/FFT_par.time
#     K1/Measurements/FFT_par.time K2/Measurements/FFT_par.time
#     K3/Measurements/FFT_par.time K4/Measurements/FFT_par.time
#     K5/Measurements/FFT_par.time K6/Measurements/FFT_par.time
#     K7/Measurements/FFT_par.time K8/Measurements/FFT_par.time
if __name__ == "__main__":
    main(sys.argv)
