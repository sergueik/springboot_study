import csv
import requests
import sys
def GetMetrixNames(url):
    response = requests.get('{0}/api/v1/label/__name__/values'.format(url))
    names = response.json()['data']
    #Return metrix names
    return names
"""
Prometheus hourly data as csv.
"""
writer = csv.writer(sys.stdout)
if len(sys.argv) != 2:
    print('Usage: {0} http://localhost:9090'.format(sys.argv[0]))
    sys.exit(1)
metrixNames=GetMetrixNames(sys.argv[1])
writeHeader=True
for metrixName in metrixNames:
     #now its hardcoded for hourly
     response = requests.get('{0}/api/v1/query'.format(sys.argv[1]),
     params={'query': metrixName+'[1h]'})
     results = response.json()['data']['result']
     # Build a list of all labelnames used.
     #gets all keys and discard __name__
     labelnames = set()
     for result in results:
         labelnames.update(result['metric'].keys())
     # Canonicalize
     labelnames.discard('__name__')
     labelnames = sorted(labelnames)
     # Write the samples.
     if writeHeader:
          writer.writerow(['name', 'timestamp', 'value'] + labelnames)
          writeHeader=False
     for result in results:
          l = [result['metric'].get('__name__', '')] + result['values']
          for label in labelnames:
              l.append(result['metric'].get(label, ''))
              writer.writerow(l)
