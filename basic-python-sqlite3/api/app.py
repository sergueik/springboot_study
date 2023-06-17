from flask import Flask, request, jsonify, render_template
import sqlite3 as sqlite
import sys
import datetime
import os

app = Flask(__name__)
global databasefile
databasefile = '../data/books.db'
# with statement in Python
class DbWrapper(object):
  def __init__(self, db):
    self.db = db
   
  def __enter__(self):
    self.conn = sqlite.connect(self.db)
    return self.conn
 
  def __exit__(self, *args):
    self.conn.close()
 
def dict_factory(cursor, row):
  d = {}
  for idx, col in enumerate(cursor.description):
    d[col[0]] = row[idx]
  return d
    
@app.route('/', methods=['GET'])
def home():
  return """<h1>Distant Reading Archive</h1>
  <p>A prototype API for distant reading of science fiction novels</p>
  """

@app.errorhandler(404)
def page_not_found(e):
  return render_template('404.html'), 404

# A route to return all of the available entries in our catalog.
@app.route('/books', methods=['GET'])
def api_all():
  results = []  
  with DbWrapper(databasefile ) as conn:
    conn.row_factory = dict_factory
    cur = conn.cursor()
    results = cur.execute('SELECT * FROM books;').fetchall()
  return jsonify(results)

@app.route('/book', methods=['GET'])
def api_filter():
  query_parameters = request.args
  
  id = query_parameters.get('id')
  published = query_parameters.get('published')
  author = query_parameters.get('author')

  to_filter = []
  query = build_select_books_query(author, id, published, to_filter)
  results = []
  
  with DbWrapper(databasefile ) as conn:
    cur = conn.cursor()
    results = cur.execute(query, to_filter).fetchall()
  
  return jsonify(results)

@app.route('/book/json', methods=['GET'])
def api_filter_json():
  books = request.get_json()
  results = []
  for book in books['books']:
    to_filter = []

    id = book['id']
    published = book['published']
    author = book['author']
    query = build_select_books_query(author, id, published, to_filter)

    resuts = []
    with DbWrapper(databasefile ) as conn:
      conn.row_factory = dict_factory
      cur = conn.cursor()
      results.append(cur.execute(query, to_filter).fetchall()[0])

  return jsonify(results)


def build_select_books_query(author, id, published, to_filter):
  query = 'SELECT * FROM books WHERE'
  if id:
    query += ' id=? AND'
    to_filter.append(id)
  if published:
    query += ' published=? AND'
    to_filter.append(published)
  if author:
    query += ' author=? AND'
    to_filter.append(author)
  if not (id or published or author):
    return page_not_found(404)
  query = query[:-4] + ';'
  return query

@app.route('/books', methods = ['POST'])
def currencyConvert():
  hostname = request.form['hostname']
  info = request.form['info']
  value = float(request.form['value'])
  timestamp = datetime.datetime.now().strftime('%s')
  # datetime.datetime.now() - datetime.timedelta(seconds=60)
  response = 'hostname={} info={} value={} timestamp={}'.format(hostname,info,str(value),timestamp)
  print(response, file=sys.stderr)
  sys.stderr.flush()
  # update the page: not returning anything will raise TypeError
  return render_template('index.html', hostname = hostname, value = str(value), timestamp = timestamp )


if __name__ == '__main__':
  if os.environ.get('PORT') is not None:
    app.run(debug=True, host='0.0.0.0', port=os.environ.get('PORT'))
  else:
    app.run(debug=True, host='0.0.0.0') 

