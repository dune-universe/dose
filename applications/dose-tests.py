#!/usr/bin/python

import unittest
from subprocess import Popen, PIPE
import difflib
import uuid
import os,sys,time
import argparse
from itertools import groupby, ifilter
import yaml

try :
    from yaml import CBaseLoader as yamlLoader
except ImportError:
    from yaml import BaseLoader as yamlLoader
    warning('YAML C-library not available, falling back to python')

import filecmp
import cStringIO
from sets import Set

class Ignore(Exception):
    pass

def convert(d) :
    def aux(e) :
        if isinstance(e, dict) :
            return frozenset(sorted([(k,aux(v)) for (k,v) in e.items()]))
        elif isinstance(e, list) :
            return frozenset(sorted([aux(v) for v in e]))
        else :
            return e
    return sorted([aux(e) for e in d])

def parse822(f,filter) :
    records = []
    for empty, record in groupby(ifilter(lambda s: not s.startswith('#'),open(f)), key=str.isspace):
        if not empty:
            l = map(lambda s : tuple(s.split(': ')), record)
            l = map(lambda (k,v) : (k,v.rstrip()), l)
            if (len(l) > 0):
                try :
                    pairs = ((k, filter((k,v.strip()))) for k,v in l)
                    records.append((pairs))
                except Ignore :
                    continue

    l = sorted([sorted(e) for e in records])
    return [frozenset(e) for e in l]

def parseedsp(f):
    fields = ['Package','Architecture']

    def filter(k,s,fields) :
        if k in fields :
            return s

    return parse822(f,filter)

def parsedistcheck(f) :
    cnf_fields = ['conflict','depends','provides','recommends']

    def cnf(k,s) :
        if k == "preamble" : raise Ignore
        if k in cnf_fields :
            l = s.split(',')
            ll = map(lambda s : s.split('|'), l)
            return ll
        else :
            return s

    return parse822(f,filter)

def parseyaml(f) :
    print "yaml %s" % f
    l = []
    if os.path.getsize(f) > 0 :
        data = yaml.load(open(f), Loader=yamlLoader)
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
    return diff_aux(expectedfile,resultfile,parseyaml)

def diff_822(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile,parsedistcheck)

def diff_text(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile,parsetext)

def diff_edsp(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile,parseedsp)
    
def test_application(self,expected_file,cmd,diff,exitcode):
    uid = uuid.uuid1()
    if not os.path.exists("tmp"):
        os.makedirs("tmp")

    output_file = "tmp/%s.cudf" % uid
    output = open(output_file,'w')
    p = Popen(cmd, stdout=output)
    p.communicate()
    rc = p.returncode if exitcode is not None else None
    if rc == exitcode :
        ec = True
    else :
        print "ExitCode = %d" % rc
        ec = False
    d = diff(expected_file,output_file)
    output.close()
    os.remove(output_file)
    self.assertTrue(d)
    self.assertTrue(ec)

class DoseTests(unittest.TestCase):
    def __init__(self, test):
        super(DoseTests, self).__init__()
        self.name = test['Name'] 
        self.comment = test['Comment'] if 'Comment' in test else None
        self.expected = test['Expected'] 
        self.cmd = test['Cmd'].split(' ') + test['Input'].split(' ')
        self.exitcode = int(test['ExitCode']) if 'ExitCode' in test else None
        if test['Type'] == '822' :
            self.difftype = diff_822
        elif test['Type']  == 'yaml' :
            self.difftype = diff_yaml
        elif test['Type']  == 'text' :
            self.difftype = diff_text
        elif test['Type']  == 'edsp' :
            self.difftype = diff_text
        else :
            self.difftype = diff_text
    def shortDescription(self):
        if self.comment :
            return "Description : " + self.comment + "\n" + ("Cmd : ") + " ".join(self.cmd) + "\nExpected file : %s" % self.expected + "\n"
        else :
            s =     "Test : %s" % self.name
            s = s + "\n" + ("Cmd : ") + " ".join(self.cmd)
            s = s + "\nExpected file : %s" % self.expected
            s = s + "\nExpected exitcode : %d" % self.exitcode if self.exitcode is not None else s
            return s + "\n"
    def runTest(self):
        test_application(self,self.expected,self.cmd,self.difftype, self.exitcode)

def suite(f,runtest,rungroup):
    suite = unittest.TestSuite()
    groups = Set()
    tests = Set()
    groupFound=False
    testsFound=False
    for stanza in parse822(f,lambda s: s[1]):
        s = dict(stanza)
        if s['Name'] not in runtest and 'Ignore' in s and s['Ignore'] == 'yes' :
            continue
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

def fixtest(expected_file,cmd):
    output = open(expected_file,'w')
    p = Popen(cmd, stdout=output)
    p.communicate()
    output.close()

def main():
    global verbose
    parser = argparse.ArgumentParser(description='Unit test for Dose applications')
    parser.add_argument('-v', '--verbose', type=int, nargs=1, default=2) 
    parser.add_argument('--runtest', nargs=1, default=[]) 
    parser.add_argument('--rungroup', nargs=1, default=[]) 
    parser.add_argument('--fixtest', nargs=1, default=[]) 
    parser.add_argument('inputfile', type=str, nargs=1, help="test file")
    args = parser.parse_args()

    verbose = args.verbose

    if len(args.fixtest) > 0 :
        f = args.inputfile[0]
        for stanza in parse822(f,lambda s: s[1]):
            s = dict(stanza)
            if args.fixtest[0] in s['Name'] :
                cmd = s['Cmd'].split(' ') + s['Input'].split(' ')
                expected = s['Expected']
                print "Overwriting expected file: %s" % expected
                fixtest(expected,cmd)
    else :
        unittest.TextTestRunner(verbosity=args.verbose).run(suite(args.inputfile[0],args.runtest,args.rungroup))

if __name__ == '__main__':
    main()

