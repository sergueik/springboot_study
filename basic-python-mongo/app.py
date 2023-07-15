from dotenv import dotenv_values
from pymongo import MongoClient

if __name__ == '__main__':

  config = dotenv_values('.env')

  mongo_client = MongoClient(config['DATABASE_URL'])
  database = mongo_client[config['MONGO_INITDB_DATABASE']]
  print('Connected to the MongoDB database via connection string {} {} {}'.format(config['DATABASE_URL'],mongo_client, database))
  collection = database['bar']
  for cnt in range(0,1000):
    item = 'bar{0}'.format(cnt)
    data = { 'item': item, 'qty': 100, 'tags': ['cotton'], 'size': {'h': 28,'w': 35.5,'uom':'cm' } }
    oid = collection.insert_one(data)
    print('oid:{}'.format(oid))
  mongo_client.close()
  print('MongoDB database connection were closed.')

