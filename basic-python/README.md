## Info 

Beam server

### Usage
```sh
IMAGE=basic-cron
docker build -t $IMAGE -f Dockerfile .
```

```sh
NAME=basic-cron
docker run --name $NAME -it $IMAGE
```

Small addition to basic `Dockerfile` 
```sh
python3 server.py 
starting up on 0.0.0.0 port 10000
accepted a connection from ('127.0.0.1', 43166)
accepted a connection from ('127.0.0.1', 43168)
received QUIT
```

```sh
sh delayed_start.sh
```
### See Also

  * https://qna.habr.com/q/1339110
  * [Essential Parts of Multiprocessing in Python ](https://towardsdatascience.com/multiprocessing-in-python-9d498b1029ca)
```python
from multiprocessing import Lock, Pool

lock = Lock()

with lock:
    driver = uc.Chrome(seleniumwire_options=wire_options, options=options)
```
  * [Multiprocessing Manager Example in Python](https://superfastpython.com/multiprocessing-manager-example/
```python
from multiprocessing import Manager, Pool

manager = Manager()

def get_whoer(proxy: str):
    # ...

if __name__ == '__main__':
    """ip:port:login:password"""
    with Pool(processes=2) as p:
        p.map(get_whoer, proxy_list)
```
  * [ How Python Keeps Your Queues Thread-Safe] (https://jonbleiberg.medium.com/how-python-keeps-your-queues-thread-safe-4b66a2f3e692)
```python
from queue import Queue
from multiprocessing import Pool

queue = Queue()

def get_whoer(proxy: str):
    # ...

if __name__ == '__main__':
    """ip:port:login:password"""
    proxy_list = list(map(str.rstrip, open('proxy.txt').readlines()))
    for proxy in proxy_list:
        queue.put(proxy)
    with Pool(processes=2) as p:
        p.map(get_whoer, range(len(proxy_list)))
```		
		


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
