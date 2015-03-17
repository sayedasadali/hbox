#!/usr/bin/env python
'''A converter between lhs and hs files.

converts based on file extension. see Usage for details
'''

__version__ = '0.9'
__author__  = 'Sayed Asad Ali'

import sys

# This code is subject to the Python licence, as can be read on
# http://www.python.org/download/releases/2.5.2/license/
#
# For those without an internet connection, here is a summary. When this
# summary clashes with the Python licence, the latter will be applied.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


def lhs2hs(filename):
	# open both files, target files named based on source file
	with open(filename, 'r') as lhs, open(filename[:-4]+'.hs', 'w') as hs:
		for line in lhs:
			# these are code lines
			if line[0] == '>':
				line = line[2:]
			# these should be commented
			else:
				line = '-- ' + line
			hs.write(line)
	print '[lhs2hs] ' + filename[:-4]+'.hs' + ' created'

def hs2lhs(filename):
	with open(filename, 'r') as hs, open(filename[:-3]+'.lhs', 'w') as lhs:
		for line in hs:
			# these are comments and the '--' should just be removed
			if line[:2] == '--':
				line = line[3:]
			#these are code lines and should be appended with '>'
			else:
				line = '> ' + line
			lhs.write(line)
	print '[hs2lhs] ' + filename[:-3]+'.lhs' + ' created'

def printUsage():
    print "Usage:"
    print "python convert.py <filename> - convert from lhs to hs  file"
    print "python convert.py <filename> - convert from hs  to lhs file"

if __name__ == '__main__':
    if len(sys.argv) == 2:
	    if sys.argv[1][-3:] == '.hs':
	    	hs2lhs(sys.argv[1])
	    elif sys.argv[1][-4:] == '.lhs':
	    	lhs2hs(sys.argv[1])
	    else:
	    	printUsage()
    else:
    	printUsage()
