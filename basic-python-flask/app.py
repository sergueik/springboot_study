# -*- coding: utf-8 -*-
from flask import Flask, jsonify, request, redirect, url_for
from urllib.parse import unquote

from flask_restful import Resource, Api
from flask_swagger_ui import get_swaggerui_blueprint
from flask_cors import CORS, cross_origin

import json
import sys,os,socket
	
app = Flask(__name__)
cors = CORS(app, resources={ r'/*': {'origins': '*'}})
# NOTE: this is probably incorrect too
app.config['CORS_HEADERS'] = 'Access-Control-Allow-Origin: *'

api = Api(app)

def show_character_ord(text):
  result = []
  for character in range(0, len(text)):
    result.append(ord(text[character]))
  return result

@app.route('/')
def hello_world():
  return redirect(url_for('hello'), code=302)
  
@app.route('/hello', methods=['GET'])
def hello():
  return 'Hello йцукен'

@app.route('/hello/<name>', methods=['GET'])
def hello_name(name: str) -> str:
   name_check = 'абв'
   return 'Hello {} {} {} {}!'.format(name, show_character_ord(name), name_check, show_character_ord(name_check))

@app.route('/call', methods = ['POST'])
def update_text():
  data = request.form 
  return ''

@app.route('/swagger.json')
@cross_origin()
def swagger():
  with open('swagger.json', 'r') as f:
    return jsonify(json.load(f))

if __name__ == '__main__':

  host_ip = '0.0.0.0'
  s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
  s.connect(('8.8.8.8', 80))
  host_ip = (s.getsockname()[0])

  # Configure Swagger UI
  # Fetch error
  # Failed to fetch http://172.17.0.2:5000/swagger.json
  # Fetch error
  # Possible cross-origin (CORS) issue? The URL origin (http://172.17.0.2:5000) does not match the page (http://192.168.99.100:5000). Check the server returns the correct 'Access-Control-Allow-*' headers.
  swaggerui_blueprint = get_swaggerui_blueprint(
    '/swagger',
    ('http://{}:5000/swagger.json'.format(host_ip)),
    config={
      'app_name': 'Sample API'
    }
  )
  app.register_blueprint(swaggerui_blueprint, url_prefix='/swagger')
  
  app.run(host=host_ip)

