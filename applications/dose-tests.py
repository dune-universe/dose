#!/usr/bin/python

import unittest
from subprocess import Popen, PIPE
import difflib
import uuid
import os,sys,time
import argparse
from itertools import groupby, ifilter
import yaml, urllib
import filecmp
import cStringIO
from sets import Set

def convert(d) :
    def aux(e) :
        if isinstance(e, dict) :
            return frozenset(sorted([(k,aux(v)) for (k,v) in e.items()]))
        elif isinstance(e, list) :
            return frozenset(sorted([aux(v) for v in e]))
        else :
            return e
    return sorted([aux(e) for e in d])

def parse822(f) :
    cnf_fields = ['conflict','depends','provides','recommends']

    def cnf(k,s) :
        if k in cnf_fields :
            l = s.split(',')
            ll = map(lambda s : s.split('|'), l)
            return ll
        else :
            return s

    records = []
    for empty, record in groupby(ifilter(lambda s: not s.startswith('#'),open(f)), key=str.isspace):
        if not empty:
            l = map(lambda s : tuple(s.split(': ')), record)
            l = map(lambda (k,v) : (k,v.rstrip()), l)
            # we ignore the preamble here ...
            if ('preamble' not in l[0]) and (len(l) > 0):
                pairs = ((k, cnf(k,v.strip())) for k,v in l)
                records.append((pairs))

    l = sorted([sorted(e) for e in records])
    return [frozenset(e) for e in l]

def parseyaml(f) :
    print "yaml %s" % f
    data = yaml.load(open(f))
    report = data.get('report', [])
    l = convert(report)
    return l

def parsetext(f) :
    l = sorted(open(f).readlines())
    return [frozenset(sorted(e)) for e in l]

def diff_aux(expectedfile,resultfile,parser):
    if filecmp.cmp(expectedfile,resultfile) :
        return True
    else :
        expected = parser(expectedfile)
        result = parser(resultfile)
        matcher = difflib.SequenceMatcher(None, expected, result)
        if matcher.ratio() == 1.0 :
            print "Warning ! Expected result and actual result are not identical."
            print "The order is not the same."
            return True
        else :
            if verbose > 2 :
                diff = difflib.unified_diff(open(expectedfile).readlines(),open(resultfile).readlines())
                sys.stdout.writelines(list(diff))
            return False

def diff_yaml(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile, parseyaml)

def diff_822(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile,parse822)

def diff_text(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile,parsetext)
    
def test_application(self,expected_file,cmd,diff):
    uid = uuid.uuid1()
    if not os.path.exists("tmp"):
        os.makedirs("tmp")

    output_file = "tmp/%s.cudf" % uid
    output = open(output_file,'w')
    p = Popen(cmd, stdout=output)
    p.communicate()
    d = diff(expected_file,output_file)
    output.close()
    os.remove(output_file)
    self.assertTrue(d)

class DoseTests(unittest.TestCase):
    def __init__(self, test):
        super(DoseTests, self).__init__()
        self.name = test['Name'] 
        self.comment = test['Comment'] if 'Comment' in test else None
        self.expected = test['Expected'] 
        self.cmd = test['Cmd'].split(' ') + test['Input'].split(' ')
        if test['Type'] == '822' :
            self.difftype = diff_822
        elif test['Type']  == 'yaml' :
            self.difftype = diff_yaml
        elif test['Type']  == 'text' :
            self.difftype = diff_text
        else :
            self.difftype = diff_text
    def shortDescription(self):
        if self.comment :
            return "Description = " + self.comment + "\n" + ("Cmd = ") + " ".join(self.cmd) + "\nExpected file = %s" % self.expected + "\n"
        else :
            return "Test = %s" % self.name + "\n" + ("Cmd = ") + " ".join(self.cmd) + "\nExpected file = %s" % self.expected + "\n"
    def runTest(self):
        test_application(self,self.expected,self.cmd,self.difftype)

def suite(f,runtest,rungroup):
    suite = unittest.TestSuite()
    groups = Set()
    tests = Set()
    groupFound=False
    testsFound=False
    for stanza in parse822(f):
        s = dict(stanza)
        groups.add(s['Group'])
        if (len(runtest) == 0 and len(rungroup) == 0) :
            suite.addTest(DoseTests(s))
        elif s['Name'] in runtest :
            testFound=True
            suite.addTest(DoseTests(s))
        elif len(rungroup) > 0 and s['Group'] in rungroup :
            groupFound=True
            suite.addTest(DoseTests(s))
    if len(runtest) != 0 and testFound == False :
        print "Test(s) [%s] Not found" % (','.join(str(p) for p in runtest)) 
        print "Tests available [%s]" % (','.join(str(p) for p in tests))
    if len(rungroup) != 0 and groupFound == False :
        print "Group(s) [%s] Not found" % (','.join(str(p) for p in rungroup))
        print "Groups available [%s]" % (','.join(str(p) for p in groups))

    return suite

def main():
    global verbose
    parser = argparse.ArgumentParser(description='Unit test for Dose applications')
    parser.add_argument('-v', '--verbose', type=int, nargs=1, default=2) 
    parser.add_argument('--runtest', nargs='+', default=[]) 
    parser.add_argument('--rungroup', nargs=1, default=[]) 
    parser.add_argument('inputfile', type=str, nargs=1, help="test file")
    args = parser.parse_args()

    verbose = args.verbose

    unittest.TextTestRunner(verbosity=args.verbose).run(suite(args.inputfile[0],args.runtest,args.rungroup))

if __name__ == '__main__':
    main()

