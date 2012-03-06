#!/usr/bin/python

# Copyright (C) 2012 Ralf Treinen <treinen@debian.org>
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

import argparse, re, yaml
import itertools, heapq
import os, sys, subprocess
import debian.debian_support

# sepchar is used when constructing the name of a pseudo-package that
# encodes the pair of two packages. It should be a character that may
# not appear in the name of a debian package, but that is accepted by
# dose-debcheck in package names.
sepchar='_'

argparser=argparse.ArgumentParser(
  description="Find packages with potential file conflicts.")
argparser.add_argument('-c1',dest='contentsfiles1',action='append',
                       required=True,
                       help='add a debian Contents file for universe 1.')
argparser.add_argument('-c2',dest='contentsfiles2',action='append',
                       required=True,
                       help='add a debian Contents file for universe 2.')
argparser.add_argument('-r1',dest='repositories1',action='append',
                       required=True,
                       help='add a debian repository file for universe 1')
argparser.add_argument('-r2',dest='repositories2',action='append',
                       required=True,
                       help='add a debian repository file for universe 2')
argparser.add_argument('-o',dest='outdir',action='store',required=True,
                       help='set name of the output directory')
argparser.add_argument('--separator',dest='sepchar',action='store',
                       required=False,
                       help='separator used in  names of pseudo packages')
arguments=argparser.parse_args()

outdir = arguments.outdir
if os.path.exists(outdir):
    print >> sys.stderr, 'fatal error: directory \'{0}\' already exists'.format(outdir)
    sys.exit(1)
else:
    os.mkdir(arguments.outdir)

if arguments.sepchar is not None:
    sepchar=arguments.sepchar

# variables used for statistics
number_shared_files  = 0  # number of shared files
number_pairs_sharing = 0  # pairs of packages that share at least 1 file
number_pairs_coinst  = 0  # among these pairs that are co-installable

############################################################################
# read the contentsfile into a dictionary
############################################################################

#print('Scanning the contents files ...', end=" ")
print('Scanning the contents files ...'),
sys.stdout.flush()

descriptors = [(open(f,'r'),1) for f in arguments.contentsfiles1] + [(open(f,'r'),2) for f in arguments.contentsfiles2]

# Skip the preambles in all files
for desc,tag in descriptors:
    while True:
        if re.match('FILE\s*LOCATION\s*',desc.readline()): break

# iterator over the pairs in a file, each pair is a line in the file split
# at whitespace.
def lr_split_file(f,tag):
    for line in f: 
        s=line.rsplit(None,1)
        yield (s[0],tag,s[1])

# table that will associate to a pair of packages a list of common files
filetable = {}

for foundfile, pck_it in itertools.groupby(
    heapq.merge(*[lr_split_file(f,tag) for (f,tag) in descriptors]),
    lambda x:x[0]):
    packages=[p for p in itertools.chain(
            *map(lambda pl:map(lambda s:(pl[1],s),re.split(',',pl[2])),pck_it))]
    pcks1=[p[1][1+p[1].rfind('/'):] for p in packages if p[0]==1]
    pcks2=[p[1][1+p[1].rfind('/'):] for p in packages if p[0]==2]
    if len(pcks1) > 1 and len(pcks2) > 1:
        number_shared_files += 1
        for pair in [(x1,x2) for x1 in pcks1 for x2 in pcks2 if x1!=x2]:
            if pair not in filetable:
                filetable[pair]=foundfile+'\n'
                number_pairs_sharing += 1
            else:
                filetable[pair] += foundfile+'\n'
    
for (desc,tag) in descriptors:
    desc.close()
print('done.')

############################################################################
# construct table of latest versions in each universe
############################################################################

print('Scanning the packages files ...'),
sys.stdout.flush()

re_blankline = re.compile('\s*$')

def fill_version_table(repositories,table):
    package, version = '', ''
    for f in repositories:
        for l in open(f):
            if re.match(re_blankline,l):
                if package != '' and version != '' :
                    if package not in table:
                        table[package] = version
                    elif debian.debian_support.version_compare(table[package],
                                                        version) < 1:
                        table[package] = version
                        package = ''
                        version = ''
            elif l.startswith('Package:'):
                package = l.split()[1]
            elif l.startswith('Version:'):
                version = l.split()[1]
                
versiontable1 = {}
versiontable2 = {}
fill_version_table(arguments.repositories1,versiontable1)    
fill_version_table(arguments.repositories2,versiontable2)    

print('done')

###########################################################################
# run debcheck
###########################################################################

#print('Running dose-debcheck ...', end=' ')
print('Running dose-debcheck ...'),

invocation='/usr/bin/dose-debcheck --successes '
for repo in arguments.repositories1:
    invocation += ' --bg=deb://' + repo 
for repo in arguments.repositories2:
    invocation += ' --bg=deb://' + repo 

sys.stdout.flush()
debcheckproc=subprocess.Popen(invocation,shell=True,
                              stdin=subprocess.PIPE,
                              stdout=subprocess.PIPE)

pseudopackage = """
Package: {pa}{sep}{pb}
Version: 1
Architecture: all
Depends: {pa}(={va}), {pb}(={vb})

"""
for (pa,pb) in filetable.keys():
    if pa in versiontable1 and pb in versiontable2:
        debcheckproc.stdin.write(pseudopackage.format(pa=pa,pb=pb,
                                                      sep=sepchar,
                                                      va=versiontable1[pa],
                                                      vb=versiontable2[pb]))
debcheckproc.stdin.close()

# read the report generated by debcheck. Since we called debcheck with
# option --successes we get only stanzas that encode co-installable
# pairs of packages.
debreport = yaml.load(debcheckproc.stdout)

print('done.')

#############################################################################
# Report statistics and generate output
#############################################################################

os.chdir(outdir)
if debreport['report'] is not None:
    for stanza in debreport['report']:
        number_pairs_coinst += 1
        stanzamatch=re.match('^(.*)'+sepchar+'(.*)',stanza['package'])
        pa,pb=stanzamatch.group(1),stanzamatch.group(2)
        out=open(pa+sepchar+pb,'w')
        out.write(filetable[(pa,pb)])
        out.close()

print('Shared files: {0}'.format(number_shared_files))
print('Pairs of packages sharing a file: {0}'.format(number_pairs_sharing))
print('Coinstallable pairs among these: {0}'.format(number_pairs_coinst))


