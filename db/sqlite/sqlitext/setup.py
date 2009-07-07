#!/usr/bin/env python

"""
setup.py file for SWIG example
"""

from distutils.core import setup, Extension


sqlitext_module = Extension('_sqlitext',
                           sources=['sqlitext_wrap.c', 'sqlitext.c'],
			   libraries=['sqlite3'],
                           )

setup (name = 'sqlitext',
       version = '0.1',
       author      = "SWIG Docs",
       description = """Simple swig example from docs""",
       ext_modules = [sqlitext_module],
       py_modules = ["sqlitext"],
       )
