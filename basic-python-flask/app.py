# -*- coding: utf-8 -*-
from flask import Flask, jsonify, request, redirect, url_for
from urllib.parse import unquote

from flask_restful import Resource, Api
from flask_swagger_ui import get_swaggerui_blueprint
from flask_cors import CORS, cross_origin

import json
import sys, os, socket

app = Flask(__name__)
cors = CORS(app, resources={r'/*': {'origins': '*'}})
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

@app.route('/call', methods=['POST'])
def update_text():
    data = request.form
    return ''

# Configure Swagger UI

# NOTE - avod using hostname in api_url to prevent
# Failed to fetch http://172.17.0.2:5000/swagger.json
# Fetch error Possible cross-origin (CORS) issue

# https://github.com/sveint/flask-swagger-ui/blob/master/flask_swagger_ui/flask_swagger_ui.py#L6
swaggerui_blueprint = get_swaggerui_blueprint(
    base_url='/swagger',
    api_url='/swagger.json',
    config={
        'app_name': 'Sample API'
    }
)
app.register_blueprint(swaggerui_blueprint, url_prefix='/swagger')

@app.route('/swagger.json')
@cross_origin()
def swagger():
    with open('swagger.json', 'r') as f:
        return jsonify(json.load(f))

if __name__ == '__main__':
    host_ip = '0.0.0.0'
    app.run(host=host_ip)
