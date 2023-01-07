from flask import Flask, render_template, request
import zeep
import requests
from lxml import etree
import os

app = Flask('CurrencyConverter', template_folder = 'Templates')
# NOTE: in runtime, complains:
# No handlers could be found for logger "elasticapm.transport"
# probably will need containerization to function
if os.getenv('SERVER') != None :
  server = os.getenv('SERVER')
else:
  server = 'localhost'

client = zeep.Client(wsdl = 'http://{}:8888/CurrencyConversionWebService?wsdl'.format(server))
currencyList = client.service.getCurrencyList()

answer = ''

@app.route('/', methods = ['GET'])
def home():
  return render_template('index.html', currencyList = currencyList, len = len(currencyList))

@app.route('/', methods = ['POST'])
def currencyConvert():
  currency1 = request.form['currencyName1']
  currency2 = request.form['currencyName2']
  value1 = float(request.form['value1'])
  print('currencyName1={} currencyName2={} value1={}'.format(currency1,currency2,str(value1)))
  node = client.create_message(client.service, 'convert', value1, currency1, currency2)
  print(etree.tostring(node, pretty_print = True))
  # update the page
  transport = zeep.Transport()
  response = transport.post_xml('http://{}:8888/CurrencyConversionWebService'.format(server), node, {"SOAPAction": '"convert"',"Content-Type": "text/xml; charset=utf-8"})
  print(response.content)
  # this will be raw SOAP server response XML document, which one has to parse to get the result
  # NOTE: can 
  # set_default_soapheaders({"SOAPAction": '"convert"',"Content-Type": "text/xml; charset=utf-8"})
  # perform the high level call
  result =   client.service.convert(value1, currency1, currency2)
  answer = str(value1) + ' ' + currency1 + ' => ' + str(round(float(result),2)) + ' ' + currency2
 
  return render_template('index.html', currencyList = currencyList, len = len(currencyList), answer = answer)


if __name__ == '__main__':
  app.run( host = '0.0.0.0', port = 5000, debug = True)
