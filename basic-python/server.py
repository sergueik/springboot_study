#Copyright (c) 2020 Serguei Kouzmine
#
# origin: https://pymotw.com/2/socket/tcp.html
# see also: http://www.java2s.com/Tutorial/Python/0420__Network/EchoServer.htm
import socket
import sys
from os import getenv
import re

# Create a TCP/IP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
port = getenv('PORT')

if port == None :
  port = 10000

server_address = ('0.0.0.0', int(port) )

print ('starting up on {} port {}'.format(server_address[0],server_address[1]))
sock.bind(server_address)
sock.listen(1)

while True:
  connection, client_address = sock.accept()
  print('accepted a connection from {}'.format(client_address))
  data = connection.recv(4096)
  if len(data):
    connection.sendall(data)
    text = data.decode('utf-8')
    print('received {}'.format(text))
    if re.match(r'QUIT', text):
      break
  # echo arriving message indicating accepted connection
  connection.close()
 
