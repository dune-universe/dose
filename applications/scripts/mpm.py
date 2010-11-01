#!/usr/bin/env python

import os
import tempfile
import os.path

import apt_pkg
import apt

from subprocess import Popen, PIPE, STDOUT

cudfConverter="bin/deb-cudf.native"

acquireProgress = apt.progress.base.AcquireProgress()
installProgress = apt.progress.base.InstallProgress()
opProgress = apt.progress.base.OpProgress()

aptroot="/tmp/test"

def setup():
    apt_pkg.init()

    cache = apt.Cache(progress=opProgress)

#XXX here is not necessary to copy all the Packages, we
#just need to write the lock

    apt_pkg.Config.set("Dir::State::lists",aptroot)
    apt_pkg.Config.set("Dir::Cache",aptroot)

    apt_pkg.Config.set("APT::Get::Simulate","true")

#    apt_pkg.Config.set("APT::Get::Show-Versions","true")
#    apt_pkg.Config.set("Debug::pkgProblemResolver","true")
#    apt_pkg.Config.set("Debug::pkgDPkgPM","true")
#    apt_pkg.Config.set("Debug::pkgOrderList","true")

    cache.open(None)
    return cache

def update(cache):
    ''' update the source lists and create cudf universe '''
# XXX regenerate the cudf only if necessary !!!!
    sourceList = cache._list
    packagesList=[]
    cache.update(acquireProgress)
    for l in sourceList.list :
        for f in l.index_files :
            if f.has_packages and f.exists :
                s = f.describe.split(' ')[-1:][0]
                packagesList.append(s.lstrip('(').rstrip(')'))

    status = "/var/lib/dpkg/status"
    packages = ' '.join(packagesList)
    cudf = os.path.join(aptroot,"universe.cudf")
    cmd = "%s --status %s --outfile %s %s" % (cudfConverter, status, cudf, packages)
    #print cmd
    out = Popen(cmd, shell=True, stdout=PIPE).communicate()[0]

def solver(cache, request, solver):
    ''' run here the mancoosi solver '''

    infile = os.path.join(aptroot,"solverinput.fifo")
    os.mkfifo(infile)
    writer = open(infile,'w+')
    universe = os.path.join(aptroot,"universe.cudf")
    cudf = open(universe,'r')
    print >>writer, cudf.readlines()
    print >>writer, "\nrequest:\n"
    os.close(cudf)
    os.close(writer)

    outfile = os.path.join(aptroot,"solveroutput.fifo")
    os.mkfifo(outfile)

    cmd = "solvers/%s/%s %s %s" (solver, infile, outfile)
    print cmd
    out = Popen(cmd, shell=True, stdout=PIPE).communicate()[0]
    print out

    reader = open(outfile,'r+')
    os.close(reader)

#    pkg1 = cache["xwatch"]
#    pkg2 = cache["di"]
#    pkg1.mark_install() #False,False,False)
#    print 'pkg1 is marked for install:', pkg1.marked_install
#    print 'pkg1 is (summary):', pkg1.candidate.summary

#    pkg2.mark_install(False,False,False)
#    print 'pkg2 is marked for install:', pkg2.marked_install
#    print 'pkg2 is (summary):', pkg2.candidate.summary

def simulator():
    ''' run here the model simulator '''
    return

def download(cache):
    depcache = cache._depcache
    pm = apt_pkg.PackageManager(depcache)
    fetcher = apt_pkg.Acquire(acquireProgress)
    pm.get_archives(fetcher, cache._list, cache._records)
    cache._fetch_archives(fetcher,pm)

def install(cache):
    depcache = cache._depcache
    depcache.commit(acquireProgress,installProgress)

def main():
    cache = setup()
    update(cache)
    solver(cache,"","p2cudf-paranoid-1.6")
#    simulator()
#    download(cache)
    #install(cache)
#    cache.commit(acquireProgress,installProgress)

    print "Broken: %s " % cache.broken_count
    print "InstCount: %s " % cache.install_count
    print "DelCount: %s " % cache.delete_count

if __name__ == '__main__':
    main()

