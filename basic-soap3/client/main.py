from flask import Flask, render_template, request
import zeep
from lxml import etree
import os

app = Flask('CurrencyConverter', template_folder = 'Templates')


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
  answer = str(value1) + ' ' + currency1 + ' => ' + str(round(float(client.service.convert(value1, currency1, currency2)),2)) + ' ' + currency2
  
  return render_template('index.html', currencyList = currencyList, len = len(currencyList), answer = answer)


if __name__ == '__main__':
  app.run( host = '0.0.0.0', port = 5000, debug = True)
