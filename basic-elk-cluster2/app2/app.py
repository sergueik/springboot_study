# -*- coding: utf-8 -*-
import sys,os,socket
from flask import Flask,request,jsonify
import json
import sqlite3 as sqlite
from urllib.parse import unquote
from elasticapm.contrib.flask import ElasticAPM
	
def show_character_ord(text):
  result = []
  for character in range(0, len(text)):
    result.append(ord(text[character]))
  return result

app = Flask(__name__)
app.config['ELASTIC_APM'] = {
          'SERVICE_NAME': 'App 2',
          'SECRET_TOKEN': '',
          'SERVER_URL': 'http://apm-server:8200'
}

if os.environ.get('MONITOR') is not None:
  apm = ElasticAPM(app)

def dict_factory(cursor, row):
  d = {}
  for idx, col in enumerate(cursor.description):
    d[col[0]] = row[idx]
  return d


@app.route("/books/json", methods=['GET'])
def api_filter_json():
  data = request.get_json()
  results = []
  id = data['id']
  query = 'SELECT * FROM books WHERE id=?;'
  conn = sqlite.connect('books.db')
  conn.row_factory = dict_factory
  cur = conn.cursor()
  result = cur.execute(query, (id,),).fetchall()
  return jsonify(result)


@app.route('/books/all')
def hello_world():

  conn = sqlite.connect('books.db')
  conn.row_factory = dict_factory
  cur = conn.cursor()
  result = cur.execute('SELECT * FROM books;').fetchall()
  payload = json.dumps(result)
  return payload


if __name__ == '__main__':
  s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
  s.connect(('8.8.8.8', 80))
  ip = (s.getsockname()[0])
  if os.environ.get('PORT') is not None:
    app.run( host = ip, port = os.environ.get('PORT'))
  else:
    app.run( host = ip, port = 6000) 
