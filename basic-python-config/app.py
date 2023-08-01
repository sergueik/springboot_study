# -*- coding: utf-8 -*-
import sys,os,socket
import requests
from flask import Flask,jsonify, request
from urllib.parse import unquote

app = Flask(__name__)
@app.route('/config')
def call_request():
  data = { 'sergueik53': {'PORTS': [22,443,3306]}, 'sergueik71': {'PORTS': [5432]}, 'sergueik119': {}}
  return jsonify(data)

if __name__ == '__main__':
  if os.environ.get('PORT') is not None:
    app.run( host = '0.0.0.0', port = os.environ.get('PORT'))
  else:
    app.run( host = '0.0.0.0', port = 6000)
