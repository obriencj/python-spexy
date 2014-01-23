#! /usr/bin/env python2


# This library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, see
# <http://www.gnu.org/licenses/>.


"""
Spexy - a sexp-based Python repl and encoding

Hacky, obtuse, and ridiculous. Written originally in 2007 while on
Ambien (or with the help of Space Lord Zolpidem)

Not to be taken too seriously.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from setuptools import setup


setup( name = "spexy",
       version = "0.9.0",
       
       packages = [ "spexy" ],
       
       test_suite = "tests",

       # PyPI information
       author = "Christopher O'Brien",
       author_email = "obriencj@gmail.com",
       url = "https://github.com/obriencj/python-spexy",
       license = "GNU Lesser General Public License",

       description = "S-Expression, LISP-like Python repl and encoding",
       
       provides = [ "spexy" ],
       requires = [],
       platforms = [ "python2 >= 2.6" ],

       classifiers = ["Intended Audience :: Developers",
                      "Programming Language :: Python :: 2",
                      "Topic :: Software Development"] )


#
# The end.
