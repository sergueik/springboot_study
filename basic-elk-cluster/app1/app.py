# -*- coding: utf-8 -*-
import sys,os,socket
import requests
from flask import Flask,jsonify
from urllib.parse import unquote
from elasticapm.contrib.flask import ElasticAPM
	
def show_character_ord(text):
  result = []
  for character in range(0, len(text)):
    result.append(ord(text[character]))
  return result

app = Flask(__name__)
app.config['ELASTIC_APM'] = {
          'SERVICE_NAME': 'App 1',
          'SECRET_TOKEN': '',
          'SERVER_URL': 'http://apm-server:8200'
}
apm = ElasticAPM(app)
@app.route('/')
def hello_world():
  return 'Hello йцукен'

@app.route('/call')
def call_request():
  URL = 'http://app2:7000/books/all'
  value = 'parameter value'
  PARAMS = {'parameter':value}
  response = requests.get(url = URL, params = PARAMS)
  # https://www.geeksforgeeks.org/get-post-requests-using-python/
  # response is already jsonified, but load it as JSON and jsonify again pretending there is some processing to happen
  data = response.json()
  return jsonify(data)

@app.route('/hello/<name>', methods=['GET'])
def hello_name(name):
   name_check = 'абв'
   return 'Hello {} {} {} {}!'.format(name, show_character_ord(name), name_check, show_character_ord(name_check))
if __name__ == '__main__':
  s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
  s.connect(('8.8.8.8', 80))
  ip = (s.getsockname()[0])
  if os.environ.get('PORT') is not None:
    app.run( host = ip, port = os.environ.get('PORT'))
  else:
    app.run( host = ip, port = 6000) 
