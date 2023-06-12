from os import listdir
from os.path import isfile, join
import time
import sys
volume = '/app'
delay = 10
while True:
  files = []
  files = [filename for filename in listdir(volume) if isfile(join(volume, filename))]
  print(files, file=sys.stderr)
  sys.stderr.flush()
  time.sleep(delay)
