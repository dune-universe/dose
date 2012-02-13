#!/usr/bin/python 

# needs debian packages: python-yaml

import argparse
import re
import os
import sys
import subprocess
import yaml

argparser=argparse.ArgumentParser(
  description="Find packages with potential file conflicts.")
argparser.add_argument('-c',dest='contentsfile',action='store',required=True,
                       help='set name of the Contents file.')
argparser.add_argument('-o',dest='outdir',action='store',required=True,
                       help='set name of the output directory')
argparser.add_argument('-r',dest='repositories',action='append',required=True,
                       help='add a debian repository (Packages) file')
arguments=argparser.parse_args()

# read the contentsfile into a dictionary
cntsf=open(arguments.contentsfile,'r')
print 'Scanning the contents file ...',
sys.stdout.flush()
# Skip the preamble
while True:
    if re.match('FILE\s*LOCATION\s*',cntsf.readline()): break
# Lines start on a file, then a comma-separated list of packages. We are only
# interested in lines that contain at least two packages. Packages are given
# as suite/section/packagename or section/packagename.
linefilter=re.compile('^(.*\S)\s+(\S*,\S*)\s*$')
filetable = {}
for line in cntsf:
    linematch=re.match(linefilter,line)
    if linematch:
        foundfile=linematch.group(1)
        # get the packages as a sorted list. Drop package prefix consisting
        # of (possibly suite and) section.
        foundpackages=sorted(map(
                lambda s:s[1+s.rfind('/'):],
                re.split(',',linematch.group(2))))
        numberpackages=len(foundpackages)
        for pair in [ (foundpackages[i],foundpackages[j])
                      for i in range(numberpackages-1)
                      for j in range(i+1,numberpackages)]:
            if pair not in filetable:
                filetable[pair]=foundfile+'\n'
            else:
                filetable[pair] += foundfile+'\n'
cntsf.close()
print 'done.'

outdir = arguments.outdir
if os.path.exists(outdir):
    raise('directory'+outdir+'already exists')
else:
    os.mkdir(arguments.outdir)

# write the input file for debcheck, containing a pseudo-package for each
# of the pairs (pa,pb) that we found, depending on pa and on pb
edosin=open(outdir+'/debcheck-input','w')
for packages,files in filetable.iteritems():
    pa=packages[0]
    pb=packages[1]
    edosin.write('Package: '+pa+'---'+pb+'\n')
    edosin.write('Version: 1\nArchitecture: all\n')
    edosin.write('Depends: '+pa+', '+pb+'\n\n')
edosin.close()

# call debcheck
debcheck='/usr/bin/dose-debcheck -s'
for repo in arguments.repositories:
    debcheck += ' --bg=deb://' + repo
debcheck += ' -o'+outdir+'/debcheck-output '+outdir+'/debcheck-input'
print 'Running dose-debcheck ...',
sys.stdout.flush()
edos=subprocess.call(debcheck,shell=True)
print 'done.'

debreport = yaml.load (file(outdir+'/debcheck-output', 'r'))
os.chdir(outdir)
for stanza in debreport['report'] :
    stanzamatch=re.match('^(.*)---(.*)',stanza['package'])
    pa,pb=stanzamatch.group(1),stanzamatch.group(2)
    out=open(pa+','+pb,'w')
    out.write(filetable[(pa,pb)])
    out.close()

#result=edos.communicate()[0]
#print result


