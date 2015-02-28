#!/usr/bin/env python2

import os

for filename in os.listdir('.'):
  outfilename = '%s.clean' % filename
  with open(filename) as infile:
    with open(outfilename, 'w') as outfile:
      for line in infile:
        parts = line.strip().split('\t')
        if len(parts) < 2:
          continue
        if parts[0] != 'g':
          continue
        print >>outfile, parts[1]
