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

class BackgroundServer(object):

  def __init__(self, bound, port = 10000, debug = False):
    self.port = port
    self.debug = debug

    thread = Thread(target = self.run, args = ())
    thread.daemon = bound
    thread.start()

  def run(self):
    server = Server(self.port, self.debug)

class Server(object):

  def __init__(self, port , debug = False):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    server_address = ('0.0.0.0', port )
    if debug:
      print ('starting up on {} port {}'.format(server_address[0],server_address[1]))

    sock.bind(server_address)
    sock.listen(1)

    while True:
      connection, client_address = sock.accept()
      if debug:
        print('accepted a connection from {}'.format(client_address))

      data = connection.recv(4096)
      # echo to caller message
      if len(data):
        connection.sendall(data)
        text = data.decode('utf-8')
        if debug:
          print('received {}'.format(text))
        if re.match(r'QUIT', text):
          break
      connection.close()

if getenv('BOUND')!= None and getenv('BOUND').lower() in ['true', '1', 't', 'y', 'yes']:
  bound = True
else:
  bound = False

if getenv('DEBUG')!= None and getenv('DEBUG').lower() in ['true', '1', 't', 'y', 'yes']:
  debug = True
else:
  debug = False

port = getenv('SERVICE_PORT')

if port == None :
  port = 10000

example = BackgroundServer(bound, int(port), debug)
print('Main exit')
