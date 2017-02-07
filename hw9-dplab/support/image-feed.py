#!/usr/bin/env python

## parse a graphic file (of most known formats) and feed into Standard ML

import os, sys
from PIL import Image
from struct import *

fileName = sys.argv[1]
fileOut = sys.argv[2]
try:
    myImg = Image.open(fileName)
    myOutput = open (fileOut,'wb')
    width,height = myImg.size
    myOutput.write(pack("ii",width,height)) ## 4-byte ints for width and height
    rgbImg = myImg.convert("RGB")
    pixels = rgbImg.getdata()
    for (r,g,b) in pixels:
        myOutput.write(pack("BBB", r,g,b)) ## 1-byte each for r,g,b
except IOError, e:
    print >> sys.stderr, "%s: %s\n\nCannot open/understand %s" % (sys.argv[0], str(e), fileName)

