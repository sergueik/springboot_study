from flask import Flask, render_template, request
import zeep
from lxml import etree

templatesLocation = 'Templates'
app = Flask('CurrencyConverter', template_folder= templatesLocation)

server = 'localhost'
server = '192.168.0.25'

client = zeep.Client(wsdl =  'http://{}:8888/CurrencyConversionWebService?wsdl'.format(server))
currencyList = client.service.getCurrencyList()

answer = ''

@app.route('/', methods=['GET'])
def home():
  return render_template('index.html', currencyList = currencyList, len = len(currencyList))

@app.route('/', methods=['POST'])
def currencyConvert():
  currency1 = request.form['currencyName1']
  currency2 = request.form['currencyName2']
  value1 = request.form['value1']

  node = client.create_message(client.service, 'convert', value1, currency1, currency2)
  print(etree.tostring(node, pretty_print = True))
  answer = value1 + ' ' + currency1 + ' => ' + str(round(client.service.convert(value1, currency1, currency2),2)) + ' ' + currency2
  
  return render_template('index.html', currencyList = currencyList, len = len(currencyList), answer = answer)


if __name__ == '__main__':
  app.run(debug=True)