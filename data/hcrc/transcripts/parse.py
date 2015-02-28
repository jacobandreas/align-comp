#!/usr/bin/env python2

import os
import re

for filename in os.listdir('.'):
  m = re.match(r'(.*)\.txt\.clean$', filename)
  if not m:
    continue
  outfilename = '%s.ptb' % m.group(1)

  os.system("""\
java -Xmx1G -jar /Users/jda/code/3p/berkeleyparser/BerkeleyParser-1.7.jar \
-gr /Users/jda/code/3p/berkeleyparser/eng_sm6.gr \
-inputFile %s \
> %s""" % (filename, outfilename))
