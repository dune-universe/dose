#!/usr/bin/python

import os,sys,time
import argparse

def extract(html_doc,man=False):
    from bs4 import BeautifulSoup
    soup = BeautifulSoup(html_doc)
    if man :
        description = soup('h1',id='DESCRIPTION',limit=1)[0].find_next_sibling()
        print description

    if man is False :
        for a in soup.findAll('a'):
            l = a['href'][:-5]
            a['href'] = "../" + l + "/" + "index.html"
     
    print soup.body

def main():
    parser = argparse.ArgumentParser(description='Ocamldoc body extractor')
    parser.add_argument('inputfile', type=str, nargs=1, help="test file")
    parser.add_argument('-m', '--man', action='store_true', default=False)
    args = parser.parse_args()
 
    f = args.inputfile[0]
    extract(open(f),args.man)

if __name__ == '__main__':
    main()
