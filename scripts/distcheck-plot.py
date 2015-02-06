#!/usr/bin/python
import argparse
import csv
import datetime as dt
import matplotlib.pyplot as plt
from matplotlib import dates
import os.path

# takes as a input a cvs file and a list of distributions and
# creates a plot of the evolution of broken and outdated packages
# vs time.

# cvs format "date suite packages broken (outdated)"
# ex : 20111125 unstable 35056 337 (73)

def initdict(dists,distname) :
    dists[distname] = {'date' : [], 'total' : [], 'broken' : [], 'outdated' : [] }
    return dists

def parse(dataset,outdated=False):
    dists = {}
    csv_reader = csv.reader(dataset,delimiter=' ')
    for line in sorted(csv_reader,key=lambda k: k[0]):
        distname = line[1]
        if distname not in dists :
            dists = initdict(dists,distname)

        dists[distname]['date'].append(dt.datetime.strptime(line[0],'%Y%m%d'))
        dists[distname]['total'].append(int(line[2]))
        dists[distname]['broken'].append(int(line[3]))
        if outdated : 
            dists[distname]['outdated'].append(int(line[4]))

    return dists

def plot(dists,distlist,output,title,outdated=False) :

    fig = plt.figure()
    fig.suptitle(title)
    plotsidx = [311,312,313] if outdated else [211,212]
    ax1 = fig.add_subplot(plotsidx[0],title='Total Packages vs Time')
    ax2 = fig.add_subplot(plotsidx[1],title='Non-Installable Packages vs Time')
    if outdated : 
        ax3 = fig.add_subplot(plotsidx[2],title='Outdated Packages vs Time')
    for k in distlist :
        ax1.plot(dists[k]['date'],dists[k]['total'],',-',label=k.capitalize())
        ax2.plot(dists[k]['date'],dists[k]['broken'],',-',label=k.capitalize())
        if outdated :
            ax3.plot(dists[k]['date'],dists[k]['outdated'],',-',label=k.capitalize())

    if len(distlist) > 1 :
        ax1.legend(loc='upper left')
        ax2.legend()
        if outdated :
            ax3.legend(loc='upper left')

    fig.autofmt_xdate()

    plt.savefig(output)

def multiplot(dists,dist1,dist2,output,title) :

    fig = plt.figure()
    fig.suptitle(title)

    plotsidx = [311,312,313]

    ax1 = fig.add_subplot(plotsidx[0],title='Total Packages vs Time')
    for k in dists :
        ax1.plot(dists[k]['date'],dists[k]['total'],',-',label=k.capitalize())
    ax1.legend(loc='upper left')

    ax2 = fig.add_subplot(plotsidx[1],title='Non-Installable Packages vs Time')
    ax2.plot(dists[dist1]['date'],dists[dist1]['broken'],',-',label=dist1.capitalize())
    ax2.legend(loc='upper right')

    ax3 = fig.add_subplot(plotsidx[2])
    ax3.plot(dists[dist2]['date'],dists[dist2]['broken'],',-',label=dist2.capitalize())
    ax3.legend(loc='upper right')

    fig.autofmt_xdate()

    plt.savefig(output)

# plot two distribution with different scales
def multiscale(dists,dist1,dist2,output,title,outdated=False) :

    fig = plt.figure()
    fig.suptitle(title)
    fig.autofmt_xdate()

    plotsidx = [311,312,313] if outdated else [211,212]
    ax1 = fig.add_subplot(plotsidx[0],title='Total Packages vs Time')
    ax1.plot(dists[dist1]['date'],dists[dist1]['total'],',-',label=dist1.capitalize())
    ax1.plot(dists[dist2]['date'],dists[dist2]['total'],',-',label=dist2.capitalize())
    ax1.legend(loc='upper left')
    ax1.xaxis.set_visible(False)

    ax2 = fig.add_subplot(plotsidx[1],title='Non-Installable Packages vs Time')
    ax2.plot(dists[dist1]['date'],dists[dist1]['broken'],'o-',label=dist1.capitalize())
    ax2.xaxis.set_visible(False)

    ax22 = ax2.twinx() 
    ax22.plot(dists[dist2]['date'],dists[dist2]['broken'],'s-',label=dist2.capitalize())
    ax22.set_ylim(0, 70)

    if outdated : 
        ax3 = fig.add_subplot(plotsidx[2],title='Outdated Packages vs Time')
        ax3.plot(dists[dist1]['date'],dists[dist1]['outdated'],'o-',label=dist1.capitalize())

        ax33 = ax3.twinx() 
        ax33.plot(dists[dist2]['date'],dists[dist2]['outdated'],'gs-',label=dist2.capitalize())
        ax33.set_ylim(0, 10)
        # ax33.set_yticks([-1,0,1])

        plt.setp(ax3.xaxis.get_majorticklabels(), rotation=30)

    plt.savefig(output)

def main():
    parser = argparse.ArgumentParser(description='plot outdated/broken')
    parser.add_argument('-v', '--verbose')
    parser.add_argument('-d', '--debug', action='store_true', default=False)
    parser.add_argument('-o', '--output', action='store')
    parser.add_argument('-t', '--title', action='store')
    parser.add_argument('-s', '--split', action='store_true', default=False)
    parser.add_argument('-m', '--multiscale', action='store_true', default=False)
    parser.add_argument('-p', '--multiplot', action='store_true', default=False)
    parser.add_argument('--outdated', action='store_true', default=False)
    parser.add_argument('dataset', type=str, nargs=1, help="dataset")
    parser.add_argument('distlist', type=str, nargs=2, help="first and second suite")
    args = parser.parse_args()
 
    fname = args.dataset[0]

    dataset = open(fname)
    dists = parse(dataset)
    dataset.close()

    if args.title :
        title = args.title[0]
    else :
        title = ""

    if args.distlist :
        distlist = args.distlist
    else :
        distlist = dists.keys()

    if args.multiscale :
        output = "aggregate-%s-ms.png" % os.path.splitext(fname)[0]
        multiscale(dists, distlist[0], distlist[1], output, title, args.outdated)
    elif args.multiplot :
        output = "aggregate-%s-mp.png" % os.path.splitext(fname)[0]
        multiplot(dists, distlist[0], distlist[1], output, title)
    else :
        if args.split :
            # different graphs, one for each suite
            for d in distlist :
                output = "%s-%s.png" % (d,os.path.splitext(fname)[0])
                plot(dists, [d] , output, title, args.outdated)
        else :
            # all in one graph
            output = "aggregate-%s.png" % os.path.splitext(fname)[0]
            plot(dists, distlist ,output, title, args.outdated)

if __name__ == '__main__':
    main()

