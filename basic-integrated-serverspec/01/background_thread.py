#Copyright (c) 2020 Serguei Kouzmine
#
# origin: https://pymotw.com/2/socket/tcp.html
# see also: http://www.java2s.com/Tutorial/Python/0420__Network/EchoServer.htm
# based on: http://sebastiandahlgren.se/2014/06/27/running-a-method-as-a-background-thread-in-python/

from threading import Thread
from time import sleep
from os import getenv
import socket
import sys
import re

class BackgroundThread(object):

  def __init__(self, bound, interval = 30):
    self.interval = interval

    thread = Thread(target = self.run, args = ())
    thread.daemon = bound
    thread.start()

  def run(self):
    while True:
      print('Doing something imporant in the background')
      sleep(self.interval)

if getenv('BOUND').lower() in ['true', '1', 't', 'y', 'yes']:
  bound = True
else:   
  bound = False

example = BackgroundThread(bound,3)
sleep(3)
print('Checkpoint')
sleep(2)
print('Main exit')
