#!/usr/bin/python

import unittest
from subprocess import Popen
import difflib
import uuid
import os,sys,time
import argparse

# TODO
# apt-cudf.native  ceve.native  challenged.native  deb-buildcheck.native
# dominators-graph.native  
# smallworld.native  strong-deps.native

def diff_files(fromfile,tofile):
    n = 3 #context lines
    fromdate = time.ctime(os.stat(fromfile).st_mtime)
    todate = time.ctime(os.stat(tofile).st_mtime)
    fromlines = open(fromfile, 'U').readlines()
    tolines = open(tofile, 'U').readlines()
    diff = difflib.unified_diff(fromlines, tolines, fromfile, tofile, 
            fromdate, todate, n=n)
    l = list(diff)
    if l :
        #sys.stdout.writelines(l)
        return False
    else :
        return True

def test_application(self,expected_file,cmd):
    uid = uuid.uuid1()
    output_file = "tmp/%s.cudf" % uid
    output = open(output_file,'w')
    #print cmd
    #print expected_file
    p = Popen(cmd, stdout=output)
    p.communicate()
    output.close()
    d = diff_files(output_file,expected_file)
    self.assertTrue(d)

class DoseTests(unittest.TestCase):

    def test_failure_distcheck(self):
        expected_file = "tests/applications/dose-tests/distcheck_test_failure"
        cmd = ["./distcheck.native","-f","-e","deb://tests/DebianPackages/sid.packages.bz2"]
        test_application(self,expected_file,cmd)

    def test_success_distcheck(self):
        expected_file = "tests/applications/dose-tests/distcheck_test_success"
        cmd = ["./distcheck.native","-s","deb://tests/DebianPackages/lenny.packages.bz2"]
        test_application(self,expected_file,cmd)

    def test_ignore_essential_distcheck(self):
        expected_file = "tests/applications/dose-tests/distcheck_test_ignore_essential"
        cmd = ["./distcheck.native","--deb-ignore-essential","-f","-e","deb://tests/DebianPackages/sid.packages.bz2"]
        test_application(self,expected_file,cmd)

    def test_failure_outdated(self):
        expected_file = "tests/applications/dose-tests/outdated_failure"
        cmd = ["./outdated.native","-f","-e","tests/DebianPackages/sid.packages.bz2"]
        test_application(self,expected_file,cmd)

def main():
    parser = argparse.ArgumentParser(description='description of you program')
    parser.add_argument('-v', '--verbose', action='store_const', const=2)
    parser.add_argument('-d', '--debug', action='store_true')
    parser.add_argument('-pwd', type=str, nargs=1, help="dose root directory")
    args = parser.parse_args()

    suite = unittest.TestLoader().loadTestsFromTestCase(DoseTests)
    unittest.TextTestRunner(verbosity=args.verbose).run(suite)

if __name__ == '__main__':
    main()

