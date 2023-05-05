from dotenv import dotenv_values
from pymongo import MongoClient
if __name__ == '__main__':

  config = dotenv_values('.env')

  mongodb_client = MongoClient(config['DATABASE_URL'])
  database = mongodb_client[config['MONGO_INITDB_DATABASE']]
  print('Connected to the MongoDB database via connection string {} {} {}'.format(config['DATABASE_URL'],mongodb_client, database))
  for doc in database.list_collections():
    print('Test:{}'.format(doc))
  mongodb_client.close()
  print('MongoDB database connection were closed.')

