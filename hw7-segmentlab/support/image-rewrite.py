#!/usr/bin/env python

## unpack raw-file format from Standard ML into a JPEG output file

import os, sys
from PIL import Image
from struct import *

fileName = sys.argv[1]
fileIn = sys.argv[2]
try:
    ## Read/unpack 4-byte ints for width and height

    myInput = open(fileIn,'rb')
    dimensions = myInput.read(2*4)
    width,height = unpack("ii", dimensions)

    pixels = myInput.read(3*width*height)
    myImg = Image.frombytes("RGB", (width, height), pixels, "raw", "RGB", 0, 1)
    myImg.save(fileName, "PNG")
except IOError, e:
    print >> sys.stderr, "%s: %s\n\nCannot open/write to %s" % (sys.argv[0], str(e), fileName)

