# -*- coding: utf-8 -*-
import sys,os,socket
import zeep
from lxml import etree
from flask import Flask, render_template, jsonify, request
from urllib.parse import unquote
from elasticapm.contrib.flask import ElasticAPM
import elasticapm
global default_port
default_port = 8000

app = Flask(__name__, template_folder = 'Templates')
# the envornment is not
if os.environ.get('APM_SERVER') is not None:
  apm_server = os.environ.get('APM_SERVER')
else:
  apm_server = 'apm-server'   
apm_server_url = 'http://{}:8200'.format(apm_server)
# NOTE: default apm agent configuration is to look for apm server at localhost:8200
app.config['ELASTIC_APM'] = { 'SERVICE_NAME': 'App 4', 'SECRET_TOKEN': '', 'SERVER_URL': apm_server_url }
apm = ElasticAPM(app)

# NOTE: some code duplication
@app.route('/', methods = ['GET'])
def home():
  if os.getenv('SOAP_SERVER') != None :
    soap_server = os.getenv('SOAP_SERVER')
  else:
    soap_server = 'localhost'
  
  client = zeep.Client(wsdl = 'http://{}:8888/CurrencyConversionWebService?wsdl'.format(soap_server))
  currencyList = client.service.getCurrencyList()
  
  return render_template('index.html', currencyList = currencyList, len = len(currencyList))

@app.route('/', methods = ['POST'])
def currencyConvert():
  currency1 = request.form['currencyName1']
  currency2 = request.form['currencyName2']
  value1 = float(request.form['value1'])
  print('currencyName1={} currencyName2={} value1={}'.format(currency1,currency2,str(value1)))
  # NOTE: in runtime, complains:
  # No handlers could be found for logger "elasticapm.transport"
  # probably will need containerization to function
  if os.getenv('SOAP_SERVER') != None :
    soap_server = os.getenv('SOAP_SERVER')
  else:
    soap_server = 'localhost'
  
  client = zeep.Client(wsdl = 'http://{}:8888/CurrencyConversionWebService?wsdl'.format(soap_server))
  currencyList = client.service.getCurrencyList()
  
  answer = ''
  node = client.create_message(client.service, 'convert', value1, currency1, currency2)
  print(etree.tostring(node, pretty_print = True))
  # update the page
  answer = str(value1) + ' ' + currency1 + ' => ' + str(round(float(client.service.convert(value1, currency1, currency2)),2)) + ' ' + currency2
  
  return render_template('index.html', currencyList = currencyList, len = len(currencyList), answer = answer)


if __name__ == '__main__':
  s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
  s.connect(('8.8.8.8', 80))
  ip = (s.getsockname()[0])
  # NOTE: connecting specifically to the ip address of the container hurts the docker-compose healthcheck
  if os.environ.get('PORT') is not None:
    # app.run( host = ip, port = os.environ.get('PORT'))
    app.run( host = '0.0.0.0', port = os.environ.get('PORT'))
  else:
    # app.run( host = ip, port = default_port) 
    app.run( host = '0.0.0.0', port = default_port)
