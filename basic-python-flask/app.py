# -*- coding: utf-8 -*-
from flask import Flask
from urllib.parse import unquote
import sys
	
app = Flask(__name__)
def show_character_ord(text):
  result = []
  for character in range(0, len(text)):
    result.append(ord(text[character]))
  return result

@app.route('/')
def hello_world():
  return 'Hello йцукен'

@app.route('/hello/<name>', methods=['GET'])
def hello_name(name):
   name_check = 'абв'
   return 'Hello {} {} {} {}!'.format(name, show_character_ord(name), name_check, show_character_ord(name_check))
if __name__ == '__main__':
  app.run(host= '0.0.0.0')

