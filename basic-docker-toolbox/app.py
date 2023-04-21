# -*- coding: utf-8 -*-
from flask import Flask,jsonify, request
from urllib.parse import unquote
import sys
	
app = Flask(__name__)

@app.route('/')
def hello_world():
  return 'Hello'

@app.route('/hello/<name>', methods=['GET'])
def hello_name(name):
   return 'Hello {}!'.format(name)

if __name__ == '__main__':
  app.run(host= '0.0.0.0')

