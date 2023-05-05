from fastapi import FastAPI
from dotenv import dotenv_values
from pymongo import MongoClient

config = dotenv_values('.env')

app = FastAPI()
@app.on_event('startup')
def startup_db_client():
  app.mongodb_client = MongoClient(config['DATABASE_URL'])
  app.database = app.mongodb_client[config['MONGO_INITDB_DATABASE']]
  print('Connected to the MongoDB database {} {}'.format(app.mongodb_client, app.database))


@app.on_event('shutdown')
def shutdown_db_client():
  app.mongodb_client.close()
  print('MongoDB database connection were closed.')


@app.get('/ping')
def ping():
  result = []
  # NOTE: cannot transmit objects:
  # UnicodeDecodeError: 'utf-8' codec can't decode byte 0x98 in position 1: invalid start byte
  for doc in app.database.list_collections():
    result.append(doc['name'])
  return {'Test': result }

