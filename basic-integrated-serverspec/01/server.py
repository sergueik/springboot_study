#Copyright (c) 2020 Serguei Kouzmine
#
# origin: https://pymotw.com/2/socket/tcp.html
# see also: http://www.java2s.com/Tutorial/Python/0420__Network/EchoServer.htm
import socket
import sys
from os import getenv
import re

class Server(object):

  def __init__(self, port , debug = False):
    # Create a TCP/IP socket
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
      if len(data):
        connection.sendall(data)
        text = data.decode('utf-8')
        if debug:
          print('received {}'.format(text))
        if re.match(r'QUIT', text):
          break
      # echo arriving message indicating accepted connection
      connection.close()

if getenv('DEBUG')!= None and getenv('DEBUG').lower() in ['true', '1', 't', 'y', 'yes']:
  debug = True
else:
  debug = False

port = getenv('SERVICE_PORT')

if port == None :
  port = 10000

server = Server(int(port), debug)
