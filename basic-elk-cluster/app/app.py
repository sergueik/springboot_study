# -*- coding: utf-8 -*-
import socket
from flask import Flask
from urllib.parse import unquote
import sys
from elasticapm.contrib.flask import ElasticAPM
	
def show_character_ord(text):
  result = []
  for character in range(0, len(text)):
    result.append(ord(text[character]))
  return result

app = Flask(__name__)
app.config['ELASTIC_APM'] = {
          'SERVICE_NAME': 'FlaskApp',
          'SECRET_TOKEN': '',
          'SERVER_URL': 'http://apm-server:8200'
}
apm = ElasticAPM(app)
@app.route('/')
def hello_world():
  return 'Hello йцукен'

@app.route('/hello/<name>', methods=['GET'])
def hello_name(name):
   name_check = 'абв'
   return 'Hello {} {} {} {}!'.format(name, show_character_ord(name), name_check, show_character_ord(name_check))
if __name__ == '__main__':
  s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
  s.connect(('8.8.8.8', 80))
  ip = (s.getsockname()[0])
  # print([l for l in ([ip for ip in socket.gethostbyname_ex(socket.gethostname())[2] if not ip.startswith("127.")][:1], [[(s.connect(('8.8.8.8', 53)), s.getsockname()[0], s.close()) for s in [socket.socket(socket.AF_INET, socket.SOCK_DGRAM)]][0][1]]) if l][0][0])
  app.run(host = ip, port = 6000)

