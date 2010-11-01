#!/usr/bin/env python

import os
import sys
import tempfile
import os.path
import shutil
import cStringIO

import apt_pkg
import apt

from subprocess import Popen, PIPE, STDOUT

cudfConverter="bin/deb-cudf.native"
cudfSolDiff="bin/cudf_sol_diff.native"

acquireProgress = apt.progress.base.AcquireProgress()
installProgress = apt.progress.base.InstallProgress()
opProgress = apt.progress.base.OpProgress()

aptroot="/tmp/test"

def update(cache):
    ''' update the source lists and create cudf universe '''
# XXX regenerate the cudf only if necessary !!!!
    packagesList=[]
    sourceList = apt_pkg.SourceList()
    cache.update(acquireProgress)
    for l in sourceList.list :
        for f in l.index_files :
            if f.has_packages and f.exists :
                s = f.describe.split(' ')[-1:][0]
                packagesList.append(s.lstrip('(').rstrip(')'))

    status = apt_pkg.Config.get("Dir::State::status")
    packages = ' '.join(packagesList)
    universe = apt_pkg.Config.get("APT::Cudf::universe")
    cmd = "%s --status %s --outfile %s --map %s" % (cudfConverter, status, universe, packages)
    #print cmd
    out = Popen(cmd, shell=True, stdout=PIPE).communicate()[0]
    print "update"

def build_map():
    from_cudf = {}
    to_cudf = {}
    infile = apt_pkg.Config.get("APT::Cudf::universe") + ".map"
    vmap = open(infile,'r')
    for line in vmap:
        (p,cv,dv) = line.rstrip().split("=")
        from_cudf[(p,dv)] = cv
        to_cudf[(p,cv)] = dv
    vmap.close()
    return (from_cudf,to_cudf)

def make_request(request) :
    l = []
    r = ""
    (from_cudf,to_cudf) = build_map()
    for item in request :
        pv = item.split("=")
        if len(pv) == 0 : continue 
        if len(pv) == 1 :
            r = pv[0]
        else :
            r = "%s = %s" % (pv[0], from_cudf[(pv[0],pv[1])])
        l.append(r)
    return ",".join(l)

def solver(cache, request):
    ''' run here the mancoosi solver '''

    solver = apt_pkg.Config.get("APT::Solver::name")
    infile = tempfile.mktemp()
    writer = open(infile,'w')
# XXX I should have a binding to generate the universe
# and create a deb version <-> cudf version mapping file 
    universe = apt_pkg.Config.get("APT::cudf::universe")
    cudf = open(universe,'r')
# I could spare myself the time of making a copy all the time, and
# reuse a cached request just by truncating the old request part and 
# appending the new request.
    shutil.copyfileobj(cudf, writer)

    print >>writer, "request: mpm"
    print >>writer, "install: %s" % request['install']
    print >>writer, "upgrade: %s" % request['upgrade']
    print >>writer, "remove: %s" % request['remove']

    cudf.close()
    writer.close()

    outfile = tempfile.mktemp()
    cmd = "./%s %s %s" % (solver, infile, outfile)
    swd = os.path.join(apt_pkg.Config.get("APT::Solver::Dir"),"solvers",solver)

    print cmd
    out = Popen(cmd, stdout=PIPE, cwd=swd, shell=True).communicate()[0]
    #print out
    
    if open(outfile,'r').readline().rstrip('\n') == 'FAIL' :
        print "Request unsatisfiable"
        sys.exit(0)

    cmd = "%s -verbose -cudf %s -sol %s" % (cudfSolDiff, infile, outfile)
    out = Popen(cmd, stdout=PIPE, shell=True).communicate()[0]
    #print out
    
    diff = cStringIO.StringIO(out)
    l = diff.readlines()
    diff.close()

    for pkg in l[-1].split(' ') :
        pv = pkg.rstrip('\n').split('=')
        if len(pv) == 0 or len(pv[0]) == 0 : continue
        if len(pv) == 1 :
            p = pv[0]
            if p[-1] == '-' :
                package = cache[p[0:-1]]
                package.mark_delete(False)
            elif p[-1] == '+' :
                package = cache[p[0:-1]]
                package.mark_install(False,False,False)
            else :
                print "Request error : %s" % p
                sys.exit(1)
        else :
            p = pv[0]
            v = pv[1]
            package = cache[p]
            package.candidate = package.versions[v]
            package.mark_install(False,False,False)

def simulator():
    ''' run here the model simulator '''
    return

def download(cache):
    depcache = cache._depcache
    pm = apt_pkg.PackageManager(depcache)
    fetcher = apt_pkg.Acquire(acquireProgress)
    pm.get_archives(fetcher, cache._list, cache._records)
    cache._fetch_archives(fetcher,pm)

def commit(cache):
    depcache = cache._depcache
    depcache.commit(acquireProgress,installProgress)

def main(args):
    apt_pkg.init_config()
    apt_pkg.Config.set("Dir::State::lists",aptroot)
    apt_pkg.Config.set("Dir::Cache",aptroot)
    apt_pkg.Config.set("APT::Get::Simulate","true")
    apt_pkg.Config.set("APT::Solver::name","p2cudf-paranoid-1.6")
    apt_pkg.Config.set("APT::Solver::criteria","+new,-changed,-notuptodate,+recommended,-removed")
    apt_pkg.Config.set("APT::Solver::Dir","/home/abate/Projects/git-svn-repos/dose3/applications/scripts")
    apt_pkg.Config.set("APT::Cudf::universe",os.path.join(aptroot,"universe.cudf"))
    apt_pkg.Config.set("APT::Cudf::map",os.path.join(aptroot,"versions.map"))

    arguments = apt_pkg.parse_commandline(apt_pkg.config, [
            ('s', "solver", "APT::Solver::name", "HasArg"),
            ('d', "solverdir", "APT::Solver::Dir", "HasArg"),
            ('i', "crit", "APT::Solver::criteria", "HasArg"),
            ('c', "config-file", "", "ConfigFile"),
            ('o', "option", "", "ArbItem")], args)

    apt_pkg.init_system()

    cache = apt.Cache(progress=opProgress)
    cache.open(None)
 
    if not arguments:
        sys.exit(0)
    elif arguments[0] == 'update' :
        update(cache)
    else :
        request = {'upgrade' : "", 'install' : "", 'remove': ""}
        if arguments[0] == 'install' :
            request['install'] = make_request(arguments[1:])
            solver(cache,request)
        elif arguments[0] == 'remove' :
            request['remove'] = make_request(arguments[1:])
            solver(cache,request)
        elif arguments[0] == 'upgrade' :
            request['upgrade'] = make_request(arguments[1:])
            solver(cache,request)
        download(cache)
#        commit(cache)
#        simulator()
#        cache.commit(acquireProgress,installProgress)

    print "Broken: %s " % cache.broken_count
    print "InstCount: %s " % cache.install_count
    print "DelCount: %s " % cache.delete_count

if __name__ == '__main__':
    sys.exit(main(sys.argv))
