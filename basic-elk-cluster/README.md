### Info

clone on [Elastic APM-Server Lab](https://github.com/SMin1620/Elastic_APM_Lab) ELK applications cluster for APM learning  testing (aftera failed to vanilla install of `apm-server` on a a custom Vagrant box with other components installed)

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-cluster.png)

### Usage

* pull the latest __7.x__ images

```sh
ELK_VERSION=7.17.7
docker pull kibana:$ELK_VERSION
docker pull elasticsearch:$ELK_VERSION
docker pull docker.elastic.co/apm/apm-server:$ELK_VERSION
```
* can run in foreground

```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up --build
```
the Kibana web UI is on [http://localhost:5601](http://localhost:5601) 
the APM indes is auto created:
```sh
curl -XGET 'localhost:9200/_cat/indices?v&pretty' | grep apm
```
```text
green  open   .apm-agent-configuration         lTdECYbVQeWchgMt_EyOTw   1   0          0            0       226b           226b
yellow open   apm-7.17.7-error-000001          T8LCy1CbSlur9BhzWosXYQ   1   1          0            0       226b           226b
yellow open   apm-7.17.7-span-000001           yo4MxOkPTyysB3inaeK5sA   1   1          0            0       226b           226b
green  open   .apm-custom-link                 BEiqFEjFSEuIvYYIxaGhyw   1   0          0            0       226b           226b
yellow open   apm-7.17.7-onboarding-2022.11.30 Px1s3L3fSv64W_zIRAqgJw   1   1          1            0      7.9kb          7.9kb
yellow open   apm-7.17.7-profile-000001        3v8VQPW-SXumL-fLxMlryw   1   1          0            0       226b           226b
yellow open   apm-7.17.7-metric-000001         4NVXn4_sSGuR2AshSJqMtA   1   1          4            0     70.8kb         70.8kb
yellow open   apm-7.17.7-transaction-000001    pTVGSy6GRPapi3OjIi_qvA   1   1          0            0       226b           226b
```
Run explicitly "loud" the cluster member health checks docker-compose does when buildin the cluster:

```sh
curl -s --write-out 'HTTP %{http_code}' --fail  http://localhost:8200/
curl -s http://localhost:9200 | jq '.tagline'
curl -s http://localhost:5601/api/status | jq '.status.overall.state'
```
this will show:

```json
{
  "build_date": "2022-10-13T16:18:51Z",
  "build_sha": "441d2c2d115da97caaab8bbdd343d527da85fe47",
  "publish_ready": true,
  "version": "7.17.7"
}
```
```text
"You Know, for Search"
```
```text

"green"
```
 and for `app`:

```sh
curl -s  http://localhost:6000/
```
```text
<h1>Distant Reading Archive</h1>
    <p>A prototype API for distant reading of science fiction novels</p>
```


check if can not connect to `apm-server`
```sh
docker-compose exec apm-server sh
```
if the error is:
```text
Error response from daemon: Container 8fb3761cee5c83adbf650fd371fe7ef6c7adafcf645a57cfd5b06e057d40c1bc is restarting, wait until the container is running
```

check if the `apm-server` container status is unstable
```
docker container ls | grep apm-server
```
```text

CONTAINER ID   IMAGE                             COMMAND                  CREATED          STATUS                          PORTS                                                 NAMES
bd60a5144d04   basic-elk-cluster_apm-server      "/usr/local/bin/dock…"   33 minutes ago   Restarting (1) 19 seconds ago                                                         apm-server

```
inspect the logs:
```sh
docker logs apm-server
```
```text
apm-server       | Exiting: error loading config file: config file ("apm-server.yml") can only be writable by the owner but the permissions are "-rw-rw-r--" (to fix the permissions use: 'chmod go-w /usr/share/apm-server/apm-server.yml')
```

and correct the permissions:
```sh
chmod 644 apm-server/config/apm-server.yml
```

* proceed with a hello world application example on `app` server
```sh
curl -s http://localhost:6000
```
![APM Example](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-example.png)

* NOTE: if run in [Docker Toolbox](https://github.com/docker-archive/toolbox) on Windows,
use the ip adddress of the network card connected to Host-only adapter in the VM running Docker:

```sh
ifconfig eth1
```
```text
eth1      link encap:Ethernet HWaddr 08:0027:B9:31:8B
          inet addr:192.168.99.100 Bcast: 192.168.99.255 Mask:255.255.255.0
```

### Configuration

By default, the stack exposes the following ports:

* 5000: Logstash TCP input.
* 9200: Elasticsearch HTTP
* 9300: Elasticsearch TCP transport
* 5601: Kibana
* 8200: APM

NOTE: the images are relatively heavy

```text
basic-elk-cluster_apm-server           latest                 c4ef445c0412   20 minutes ago   258MB
basic-elk-cluster_kibana               latest                 3a414cdc79d3   20 minutes ago   799MB
basic-elk-cluster_elasticsearch        latest                 a3f9ff0db620   23 minutes ago   619MB
basic-elk-cluster_app                  latest                 2902ce4b8a5c   2 days ago       133MB
```
### TODO


* occasionally after the start of the cluster, the Elastic Server reports APM Server issue on initial setup page `http://192.168.0.92:5601/app/home#/tutorial/apm`:


No APM Server detected. Please make sure it is running and you have updated to 7.0 or higher.

  * the `docker-compose` container cluster build fails under [Docker Toolbox](https://github.com/docker-archive/toolbox) on Windows, with
```text
ERROR: for elasticsearch  Cannot start service elasticsearch: OCI runtime create failed: container_linux.go:349: starting container process caused "process_linux.go:449: container init caused \"rootfs_linux.go:58: mounting \\\"/c/developer/sergueik/springboot_study/basic-elk-cluster/elasticsearch/config/elasticsearch.yml\\\" to rootfs \\\"/mnt/sda1/var/lib/docker/overlay2/39b30c8076f811570ce79d4f29d44bd7398ead2b5bc85e5070a556952bd8fe92/merged\\\" at \\\"/mnt/sda1/var/lib/docker/overlay2/39b30c8076f811570ce79d4f29d44bd7398ead2b5bc85e5070a556952bd8fe92/merged/usr/share/elasticsearch/config/elasticsearch.yml\\\" caused \\\"not a directory\\\"\"": unknown: Are you trying to mount a directory onto a file (or vice-versa)? Check if the specified host path exists and is the expected type
ERROR: Encountered errors while bringing up the project.
```

the workaround would be to add the `ADD` instructions to node `Dockerfile`
 
  * after fixing the volume issue, still observing `elasticsearch` fail to initialize:
```text
ERROR: for kibana  Container "abed361f9948" is unhealthy.

ERROR: for apm-server  Container "abed361f9948" is unhealthy.
ERROR: Encountered errors while bringing up the project.
```
the id of the unhealthy container is `elasticsearch`
```text
abed361f9948        basic-elk-cluster_elasticsearch   "/usr/local/bin/dock"   33
 minutes ago      Up 33 minutes (unhealthy)   0.0.0.0:9200->9200/tcp, 9300/tcp
 elasticsearch
```
with the logs:
```text

{"type": "server", "timestamp": "2022-11-27T14:23:00,138+0900", "level": "ERROR", "component": "o.e.x.m.c.i.IndexStatsCollector", "cluster.name": "docker-cluster", "node.name": "elasticsearch",  "message": "collector [index-stats] failed to collect data" ,
"stacktrace": ["java.lang.NullPointerException: null",
"at org.elasticsearch.xpack.monitoring.collector.Collector.collect(Collector.java:85) [x-pack-monitoring-7.1.0.jar:7.1.0]",
"at org.elasticsearch.xpack.monitoring.MonitoringService$MonitoringExecution$1.doRun(MonitoringService.java:242) [x-pack-monitoring-7.1.0.jar:7.1.0]",
"at org.elasticsearch.common.util.concurrent.AbstractRunnable.run(AbstractRunnable.java:37) [elasticsearch-7.1.0.jar:7.1.0]",
"at java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:515) [?:?]",
"at java.util.concurrent.FutureTask.run(FutureTask.java:264) [?:?]",
"at org.elasticsearch.common.util.concurrent.ThreadContext$ContextPreservingRunnable.run(ThreadContext.java:681) [elasticsearch-7.1.0.jar:7.1.0]",
"at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1128) [?:?]",
"at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:628) [?:?]",
"at java.lang.Thread.run(Thread.java:835) [?:?]"] }
```

  * remove the `logstash` node from the cluster (not needed for APM exercise), also it makes the cluster unstable in long run possibly due to memory leak:
```sh
docker container ls
```
```text
CONTAINER ID   IMAGE                             COMMAND                  CREATED        STATUS                            PORTS                              NAMES
7c151f60e5a3   baic-aspnetapp                    "./aspnetapp"            22 hours ago   Up 22 hours                       0.0.0.0:8000->80/tcp               basic-aspnetapp
6f2396c64de1   basic-elk-cluster_app             "python /app/app.py"     28 hours ago   Up 23 hours (unhealthy)           0.0.0.0:6000->6000/tcp, 8080/tcp   app
6ad327b93b37   basic-elk-cluster_apm-server      "/usr/local/bin/dock…"   28 hours ago   Up 23 hours (healthy)             0.0.0.0:8200->8200/tcp             apm-server
88722a866d04   basic-elk-cluster_kibana          "/usr/local/bin/kiba…"   28 hours ago   Up 23 hours (healthy)             0.0.0.0:5601->5601/tcp             kibana
eb7149f3e2e8   basic-elk-cluster_logstash        "/usr/local/bin/dock…"   28 hours ago   Up 23 hours                       0.0.0.0:5044->5044/tcp, 9600/tcp   logstash
d23974a3444c   basic-elk-cluster_elasticsearch   "/usr/local/bin/dock…"   28 hours ago   Up 8 seconds (health: starting)   0.0.0.0:9200->9200/tcp, 9300/tcp   elasticsearch
```
one can not stop 
```sh
docker-compose stop
```

```text
Stopping app           ... 
Stopping apm-server    ... 
Stopping apm-server    ... error
Stopping kibana        ... error
Stopping elasticsearch ... 
Stopping elasticsearch ... error
```
- repeated attempts fix this

* add [elastic filebeat](https://hub.docker.com/layers/elastic/filebeat/7.17.7) node forpracticing Kibana/Lucene queries on the same cluster

* exercise [log Correlation](https://www.elastic.co/guide/en/apm/agent/python/current/log-correlation.html)

### Testing Failure to Collect the Metrics

Add another application, using [python flask api running in a docker container](https://github.com/deparkes/docker_flask_example)
integrated with Elastic APM:


```sh
docker-compose ps
```
```text
    Name               Command               State                Ports
--------------------------------------------------------------------------------
apm-server      /usr/bin/tini --         Up (healthy)     0.0.0.0:8200->8200/tcp
                /usr/loca ...                             ,:::8200->8200/tcp
app1            python /app/app.py       Up (unhealthy)   0.0.0.0:6000->6000/tcp
                                                          ,:::6000->6000/tcp,
                                                          8080/tcp
app2            python app.py            Up               0.0.0.0:7000->7000/tcp
                                                          ,:::7000->7000/tcp
elasticsearch   /bin/tini --             Up (healthy)     0.0.0.0:9200->9200/tcp
                /usr/local/bi ...                         ,:::9200->9200/tcp,
                                                          9300/tcp
kibana          /bin/tini --             Up (healthy)     0.0.0.0:5601->5601/tcp
                /usr/local/bi ...                         ,:::5601->5601/tcp

```
* interact with the `app1`:

```sh
curl http://localhost:6000/hello/12345
```

it will instantly show in the Observability Servies summary page 

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-two-services.png)

* interact with the `app2`:

```sh
curl http://localhost:7000/
```
```text
<h1>Distant Reading Archive</h1>
    <p>A prototype API for distant reading of science fiction novels</p>

```
also test a failing call:
```sh
curl http://localhost:7000/api/v1/resources/books/json
```
```text
<!doctype html>
<html lang=en>
<title>400 Bad Request</title>
<h1>Bad Request</h1>
<p>Failed to decode JSON object: None</p>
```

* check the  `apm-server` console log, notice the application name:
```sh
docker container logs -f apm-server
```
(will have to scan visually)
```text
{"log.level":"info","@timestamp":"2022-12-01T18:24:18.003Z","log.logger":"request","log.origin":{"file.name":"middleware/log_middleware.go","file.line":63},"message":"request accepted","service.name":"apm-server","url.original":"/intake/v2/events","http.request.method":"POST","user_agent.original":"apm-agent-python/6.13.2 (Flask SQLite REST App)","source.address":"172.18.0.6","http.request.body.bytes":478,"http.request.id":"f272c310-13e3-4ea8-8137-a0bd590279e4","event.duration":228619,"http.response.status_code":202,"ecs.version":"1.6.0"}

{"log.level":"info","@timestamp":"2022-12-01T18:27:47.818Z","log.logger":"requt","log.origin":{"file.name":"middleware/log_middleware.go","file.line":63},"msage":"request accepted","service.name":"apm-server","url.original":"/intake/vevents","http.request.method":"POST","user_agent.original":"apm-agent-python/63.2 (Flask SQLite REST App)","source.address":"172.18.0.6","http.request.body.tes":472,"http.request.id":"a7581f77-9d31-4740-9cd7-65fa7c5614cd","event.duratn":354388,"http.response.status_code":202,"ecs.version":"1.6.0"}

```

however no monitoring data is observed

* bad payload leading to 400 bad resest
```sh
curl -X GET -s -H "Content-Type: application/json" -d '{id:100}'  http://172.17.0.2:6000/books/json 
```

```text
<!doctype html>
<html lang=en>
<title>400 Bad Request</title>
<h1>Bad Request</h1>
<p>The browser (or proxy) sent a request that this server could not understand.</p>

```
* wrong method
```sh
curl -X POST -s -H "Content-Type: application/json" -d '{"id":1}'  http://172.17.0.2:6000/books/json 
```

```text
<!doctype html>
<html lang=en>
<title>405 Method Not Allowed</title>
<h1>Method Not Allowed</h1>
<p>The method is not allowed for the requested URL.</p>
```
* regular processing, should create `sqlite` transactions
```sh
curl -X GET -s -H "Content-Type: application/json" -d '{"id":100}'  http://172.17.0.2:6000/books/json
```
```text
[]
```


![APM Transactions](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-transactions.png)

NOTE, the Python APM code is not shown in stack trace of the SQLite `connect` transaction:

```python
/app/app.py in api_filter_json at line 38
```
```python
38 conn = sqlite.connect('books.db')
```
```text
+ 15 library frames
```
```python
flask/app.py in dispatch_request at line 1502
flask/app.py in full_dispatch_request at line 1516
flask/app.py in wsgi_app at line 2073
flask/app.py in __call__ at line 2091
werkzeug/serving.py in execute at line 322
werkzeug/serving.py in run_wsgi at line 335
http/server.py in handle_one_request at line 415
http/server.py in handle at line 427
werkzeug/serving.py in handle at line 363
python3.8/socketserver.py in __init__ at line 720
python3.8/socketserver.py in finish_request at line 360
python3.8/socketserver.py in process_request_thread at line 650
python3.8/threading.py in run at line 870
python3.8/threading.py in _bootstrap_inner at line 932
python3.8/threading.py in _bootstrap at line 890
```

nor the `query` transaction
```text
database statement
```
```SQL
SELECT * FROM books WHERE id=?;
```
```python
flask/app.py in dispatch_request at line 1502
```
```python
1408    def dispatch_request(self) -> ResponseReturnValue:                   
...
1502        return self.ensure_sync(self.view_functions[rule.endpoint])(**req.view_a
```
```python
flask/app.py in full_dispatch_request at line 1516
flask/app.py in wsgi_app at line 2073
flask/app.py in __call__ at line 2091
werkzeug/serving.py in execute at line 322
werkzeug/serving.py in run_wsgi at line 335
http/server.py in handle_one_request at line 415
http/server.py in handle at line 427
werkzeug/serving.py in handle at line 363
python3.8/socketserver.py in __init__ at line 720
python3.8/socketserver.py in finish_request at line 360
python3.8/socketserver.py in process_request_thread at line 650
python3.8/threading.py in run at line 870
python3.8/threading.py in _bootstrap_inner at line 932
python3.8/threading.py in _bootstrap at line 890
```
* to inspect the call stack, connect to the `app2` container:

```sh
docker exec -it app2 sh
```
```sh
vi /usr/local/lib/python3.8/site-packages/flask/app.py 
```
* pass bad payload
```sh
curl -X GET -s -H "Content-Type: application/json" -d '{}'  http://172.17.0.2:6000/books/json
```
to trigger `KeyError: 'id'` Exception on /books/json and 500 Internal Server Error
```text
<!doctype html>
<html lang=en>
<title>500 Internal Server Error</title>
<h1>Internal Server Error</h1>
<p>The server encountered an internal error and was unable to complete your request. Either the server is overloaded or there is an error in the application.</p>
```

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-two-services2.png)

### Trace Sample

The REST call from `app1` to `app2`:

```python
@app.route('/call')
def call_request():
  URL = 'http://app2:7000/books/all'
  value = 'parameter value'
  PARAMS = {'parameter':value}
  response = requests.get(url = URL, params = PARAMS)
  # https://www.geeksforgeeks.org/get-post-requests-using-python/
  # response is already jsonified, but load it as JSON and jsonify again pretending there is some processing to happen
  data = response.json()
  return jsonify(data)
``` 

and subsequent SQLite `connect` and `query` transactions

```python
@app.route('/books/all')
def hello_world():

  conn = sqlite.connect('books.db')
  conn.row_factory = dict_factory
  cur = conn.cursor()
  result = cur.execute('SELECT * FROM books;').fetchall()
  payload = json.dumps(result)
  return payload
```
were confirmed to have identical `tracei.id` metadata value generated by APM.

![Trace Sample](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-trace-sample.png)

```sh
curl -H "Custom_Request_Header: value123" http://localhost:6000/call
curl -H "Custom_Request_Header: value2" http://localhost:6000/call
```
```sh
curl -H "AppendedField: value1" http://localhost:6000/call
curl -H "AppendedField: value2" http://localhost:6000/call
```

```sh
curl -XGET 'localhost:9200/apm-7.17.7-transaction-000001/_search?_source=false'
```

see both 

`http.request.headers.Custom-Request-Header`
and `http.request.headers.Appendedfield`:

![Custom Appended Field](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-transaction-appended_field.png)


![Custom Request Header](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-transaction-custom-request-header.png)

show in Metadata


#### Modifying the APM Transaction Name

* Update directly

```sh
curl -H 'Content-Type: application/json' -X PUT  '192.168.0.64:9200/apm-7.17.7-transaction-000001/_doc/eNptHIUBtUxcsrGvfl0g' -d '{
  "container": {
    "id": "7dbe0a84978e4f3e8b148c25515cd14056abf067e64ccf344ee6b087a7a8f1a9"
  },
  "agent": {
    "name": "python",
    "version": "6.13.2"
  },
  "process": {
    "args": [
      "/app/app.py"
    ],
    "pid": 1,
    "ppid": 0
  },
  "source": {
    "ip": "192.168.0.25"
  },
  "processor": {
    "name": "transaction",
    "event": "transaction"
  },
  "url": {
    "path": "/call",
    "scheme": "http",
    "port": 6000,
    "domain": "192.168.0.64",
    "full": "http://192.168.0.64:6000/call"
  },
  "labels": {
    "dollar_value": 47.12,
    "ecommerce": true
  },
  "observer": {
    "hostname": "047a4ac9306a",
    "id": "821fab6f-a265-42af-9f93-54b98c8eb165",
    "type": "apm-server",
    "ephemeral_id": "84da4704-9a5a-4ae4-b414-a3f01b7c5e6c",
    "version": "7.17.7",
    "version_major": 7
  },
  "trace": {
    "id": "f999b9190f2c4254811fd90648619097"
  },
  "@timestamp": "2022-12-16T19:33:29.695Z",
  "ecs": {
    "version": "1.12.0"
  },
  "service": {
    "node": {
      "name": "7dbe0a84978e4f3e8b148c25515cd14056abf067e64ccf344ee6b087a7a8f1a9"
    },
    "framework": {
      "name": "flask",
      "version": "2.0.3"
    },
    "name": "App 1",
    "runtime": {
      "name": "CPython",
      "version": "3.8.2"
    },
    "language": {
      "name": "python",
      "version": "3.8.2"
    }
  },
  "host": {
    "hostname": "7dbe0a84978e",
    "os": {
      "platform": "linux"
    },
    "ip": "172.19.0.6",
    "name": "7dbe0a84978e",
    "architecture": "x86_64"
  },
  "http": {
    "request": {
      "headers": {
        "Authorization": [
          "[REDACTED]"
        ],
        "Accept": [
          "*/*"
        ],
        "User-Agent": [
          "curl/7.74.0"
        ],
        "Host": [
          "192.168.0.64:6000"
        ]
      },
      "method": "GET",
      "env": {
        "SERVER_PORT": "6000",
        "REMOTE_ADDR": "192.168.0.25",
        "SERVER_NAME": "172.19.0.6"
      }
    },
    "response": {
      "headers": {
        "Content-Length": [
          "13688"
        ],
        "Content-Type": [
          "application/json"
        ]
      },
      "status_code": 200
    }
  },
  "client": {
    "ip": "192.168.0.25"
  },
  "event": {
    "ingested": "2022-12-16T19:33:35.892163533Z",
    "outcome": "success"
  },
  "user_agent": {
    "original": "curl/7.74.0",
    "name": "curl",
    "device": {
      "name": "Other"
    },
    "version": "7.74.0"
  },
  "transaction": {
    "result": "HTTP 2xx",
    "duration": {
      "us": 62058
    },
    "name": "GET /call/XXX ",
    "span_count": {
      "dropped": 0,
      "started": 1
    },
    "id": "7d74d5a5237a67a5",
    "type": "request",
    "sampled": true
  },
  "timestamp": {
    "us": 1671219209695904
  }
}'

* this will print to console
```json
{
    "_index": "apm-7.17.7-transaction-000001",
    "_type": "_doc",
    "_id": "eNptHIUBtUxcsrGvfl0g",
    "_version": 2,
    "result": "updated",
    "_shards": {
        "total": 2,
        "successful": 1,
        "failed": 0
    },
    "_seq_no": 2,
    "_primary_term": 1
}
```

* and the Dev Tools query

```
GET /apm-7.17.7-transaction-000001/_doc/eNptHIUBtUxcsrGvfl0g?_source=transaction.name
```

will confirm:

```json
{
    "_index": "apm-7.17.7-transaction-000001",
    "_type": "_doc",
    "_id": "eNptHIUBtUxcsrGvfl0g",
    "_version": 2,
    "_seq_no": 2,
    "_primary_term": 1,
    "found": true,
    "_source": {
        "transaction": {
            "name": "GET /call/XXX "
        }
    }
}
```

and so will do the APM Transactions screen in Kibana:

![Custom Appended Field](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-transaction-renamed.png)


* one [can](https://stackoverflow.com/questions/19563215/update-only-specific-field-value-in-elasticsearch) update just the selected field


* the correct way is
```sh

INDEX=apm-7.17.7-transaction-000001
TYPE=_doc
ID=eNptHIUBtUxcsrGvfl0g
curl -H 'Content-Type: application/json' -X POST "192.168.0.64:9200/$INDEX/$TYPE/$ID/_update" -d '{
    "doc" : {
        "transaction": {"name":  "GET /call/YYY"}
    }
}'
```
```sh

INDEX=apm-7.17.7-transaction-000001
TYPE=_doc
ID=ZgddHoUBLFUYqZyXJEff
curl -H 'Content-Type: application/json' -X POST "192.168.0.64:9200/$INDEX/$TYPE/$ID/_update" -d '{
    "doc" : {
        "http": {"request": {"headers": {"Appendedfield": [ "http://xmlme.com/WebServices/GetSpeech"] }}}
    }
}'

NOTE: this will be incorrect syntax
```sh
INDEX=apm-7.17.7-transaction-000001
TYPE=_doc
ID=eNptHIUBtUxcsrGvfl0g
curl -H 'Content-Type: application/json' -X POST "192.168.0.64:9200/$INDEX/$TYPE/$ID/_update" -d '{
    "doc" : {
        "transaction.name": "GET /call/YYY"
    }
}'

```

it will return new version
```json
{
    "_index": "apm-7.17.7-transaction-000001",
    "_type": "_doc",
    "_id": "eNptHIUBtUxcsrGvfl0g",
    "_version": 4,
    "result": "updated",
    "_shards": {
        "total": 2,
        "successful": 1,
        "failed": 0
    },
    "_seq_no": 4,
    "_primary_term": 1
}
```

which will duplicate the entry in APM Observability Transactions	
 
and the Dev Tools query 

```sh
GET /apm-7.17.7-transaction-000001/_doc/eNptHIUBtUxcsrGvfl0g?_source=transaction
```
show the reason:

```json

{
    "_index": "apm-7.17.7-transaction-000001",
    "_type": "_doc",
    "_id": "eNptHIUBtUxcsrGvfl0g",
    "_version": 4,
    "_seq_no": 4,
    "_primary_term": 1,
    "found": true,
    "_source": {
        "transaction.name": "GET /call/YYY",
        "transaction": {
            "name": "GET /call/XXX "
        }
    }
}
```


* do a REST call with `Custom_Request_Header` custom header
```sh
curl -H "Custom_Request_Header: value123" http://192.168.0.64:6000/call
```
query

```
GET /apm-7.17.7-transaction-000001/_search
```

 * focus on the entry with the header "Custom_Request_Header": "value123"
 
  it will be renamed as
  
```json

 "http" : {
      "request" : {
        "headers" : {
          "Custom-Request-Header" : [
            "value123"
          ],
  
```
 
 
```sh
GET /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx?_source=transaction.name,http.request.headers.Custom-Request-Header
```

```json
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "D9qeHIUBtUxcsrGvCHtx",
  "_version" : 1,
  "_seq_no" : 7,
  "_primary_term" : 1,
  "found" : true,
  "_source" : {
    "http" : {
      "request" : {
        "headers" : {
          "Custom-Request-Header" : [
            "value123"
          ]
        }
      }
    },
    "transaction" : {
      "name" : "GET /call"
    }
  }
}
```




Run dynamic update
```sh
POST /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx/_update 
{
  "script" : {
    "source": "ctx._source.transaction.name = 'GET /call4'",
    "lang": "painless",
    "params" : {
      "count" : 4
    }
  }
}

```

it will succeed:

```json
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "D9qeHIUBtUxcsrGvCHtx",
  "_version" : 2,
  "result" : "updated",
  "_shards" : {
    "total" : 2,
    "successful" : 1,
    "failed" : 0
  },
  "_seq_no" : 8,
  "_primary_term" : 1
}

```

```sh
GET /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx?_source=transaction.name,http.request.headers.Custom-Request-Header
```

will show

```json
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "D9qeHIUBtUxcsrGvCHtx",
  "_version" : 2,
  "_seq_no" : 8,
  "_primary_term" : 1,
  "found" : true,
  "_source" : {
    "http" : {
      "request" : {
        "headers" : {
          "Custom-Request-Header" : [
            "value123"
          ]
        }
      }
    },
    "transaction" : {
      "name" : "GET /call4"
    }
  }
}

```

```sh
POST /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx/_update 
{
  "script" : {
    "source": "ctx._source.transaction.name += ctx._source.http.request.headers.Custom-Request-Header",
    "lang": "painless",
    "params" : {
      "count" : 4
    }
  }
}

```


this will fail with:

```json
{
  "error" : {
    "root_cause" : [
      {
        "type" : "illegal_argument_exception",
        "reason" : "failed to execute script"
      }
    ],
    "type" : "illegal_argument_exception",
    "reason" : "failed to execute script",
    "caused_by" : {
      "type" : "script_exception",
      "reason" : "compile error",
      "script_stack" : [
        "... p.request.headers.Custom-Request-Header",
        "                             ^---- HERE"
      ],
      "script" : "ctx._source.transaction.name += ctx._source.http.request.headers.Custom-Request-Header",
      "lang" : "painless",
      "position" : {
        "offset" : 72,
        "start" : 47,
        "end" : 86
      },
      "caused_by" : {
        "type" : "illegal_argument_exception",
        "reason" : "cannot resolve symbol [Request]"
      }
    }
  },
  "status" : 400
}

```

but the
```sh
POST /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx/_update 
{
  "script" : {
    "source": "ctx._source.transaction.name += ctx._source.http.request.headers['Custom-Request-Header']",
    "lang": "painless",
    "params" : {
      "count" : 4
    }
  }
}

```

it  will succeed, but the format will not be quite correct:
```sh
GET /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx?_source=transaction.name,http.request.headers.Custom-Request-Header
```

```JSON
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "D9qeHIUBtUxcsrGvCHtx",
  "_version" : 3,
  "_seq_no" : 9,
  "_primary_term" : 1,
  "found" : true,
  "_source" : {
    "http" : {
      "request" : {
        "headers" : {
          "Custom-Request-Header" : [
            "value123"
          ]
        }
      }
    },
    "transaction" : {
      "name" : "GET /call4[value123]"
    }
  }
}

```

the correct one is 

```sh

POST /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx/_update 
{
  "script" : {
    "source": "ctx._source.transaction.name += '/' + ctx._source.http.request.headers['Custom-Request-Header'][0]",
    "lang": "painless"
  }
}
```

this will produce:

```sh
GET /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx?_source=transaction.name,http.request.headers.Custom-Request-Header

```
```JSON
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "D9qeHIUBtUxcsrGvCHtx",
  "_version" : 11,
  "_seq_no" : 17,
  "_primary_term" : 1,
  "found" : true,
  "_source" : {
    "http" : {
      "request" : {
        "headers" : {
          "Custom-Request-Header" : [
            "value123"
          ]
        }
      }
    },
    "transaction" : {
      "name" : "GET /call4/value123"
    }
  }
}
```

* do a REST call with `AppendedField` custom header
```
curl -H "AppendedField: value456" http://192.168.0.64:6000/call
```


query

```
GET /apm-7.17.7-transaction-000001/_search
```

 * focus on the entry with the header "Custom_Request_Header": "value456"

```
GET /apm-7.17.7-transaction-000001/_search?_source=transaction.name,http.request.headers.Appendedfield
``` 
use the `id` of the  document that contains the header
```sh
GET /apm-7.17.7-transaction-000001/_doc/s9rBHIUBtUxcsrGvZpAB?_source=transaction.name,http.request.headers.Appendedfield
```

```json

{
    "_index": "apm-7.17.7-transaction-000001",
    "_type": "_doc",
    "_id": "s9rBHIUBtUxcsrGvZpAB",
    "_version": 1,
    "_seq_no": 19,
    "_primary_term": 1,
    "found": true,
    "_source": {
        "http": {
            "request": {
                "headers": {
                    "Appendedfield": [
                        "value456"
                    ]
                }
            }
        },
        "transaction": {
            "name": "GET /call"
        }
    }
}
```

* update that document
```
POST /apm-7.17.7-transaction-000001/_doc/s9rBHIUBtUxcsrGvZpAB/_update 
{
  "script" : {
    "source": "ctx._source.transaction.name += '/' + ctx._source.http.request.headers['Appendedfield'][0]",
    "lang": "painless"
  }
}
```

* verify
```sh

GET /apm-7.17.7-transaction-000001/_doc/s9rBHIUBtUxcsrGvZpAB?_source=transaction.name,http.request.headers.Appendedfield
```
```json
{
    "_index": "apm-7.17.7-transaction-000001",
    "_type": "_doc",
    "_id": "s9rBHIUBtUxcsrGvZpAB",
    "_version": 2,
    "_seq_no": 22,
    "_primary_term": 1,
    "found": true,
    "_source": {
        "http": {
            "request": {
                "headers": {
                    "Appendedfield": [
                        "value456"
                    ]
                }
            }
        },
        "transaction": {
            "name": "GET /call/value456"
        }
    }
}
```

* update that document idempotently
```sh
POST /apm-7.17.7-transaction-000001/_doc/s9rBHIUBtUxcsrGvZpAB/_update 

{
    "script": {
        "source": "String data = ctx._source.http.request.headers['Appendedfield'][0]; if (ctx._source.transaction.name.indexOf(data ) == -1 ) ctx._source.transaction.name += '/' + data;",
        "lang": "painless"
    }
}
```

alternatively

```sh
POST /apm-7.17.7-transaction-000001/_update_by_query

{
  "script": {
      "source": """

      String data1 = ctx._source.transaction.name;
      String data2 = ctx._source.http.request.headers['Appendedfield'][0];
      if (data1.indexOf(data2) == -1)
          ctx._source.transaction.name = data1 + '/' + data2;
      else
          ctx.op = 'noop'
      """,
      "lang": "painless"
  },
  "query": {
      "terms": {
          "_id": [
              "s9rBHIUBtUxcsrGvZpAB"
          ]
      }
  }
}
```
NOTE: can query by presence of the SOAP API  which would be the only one in need of amening with `SOAPAction` header:

```sh 
GET /apm-7.17.7-transaction-000001/_search?_source=_id,transaction.name,http.request.headers.Appendedfield

{
    "query": {
        "terms": {
            "transaction.name": [
                "GET /call"
            ]
        }
    }
}
```
or
```sh 
GET /apm-7.17.7-transaction-000001/_search?_source=_id,transaction.name,http.request.headers.Appendedfield

{
    "query": {
        "match": {
            "transaction.name": [
                "GET /call"
            ]
        }
    }
}
```


these work:

```JSON

{
    "took": 1,
    "timed_out": false,
    "_shards": {
        "total": 1,
        "successful": 1,
        "skipped": 0,
        "failed": 0
    },
    "hits": {
        "total": {
            "value": 1,
            "relation": "eq"
        },
        "max_score": 1.0,
        "hits": [{
            "_index": "apm-7.17.7-transaction-000001",
            "_type": "_doc",
            "_id": "6drBHIUBtUxcsrGvtZBL",
            "_score": 1.0,
            "_source": {
                "http": {
                    "request": {
                        "headers": {
                            "Appendedfield": [
                                "value456"
                            ]
                        }
                    }
                },
                "transaction": {
                    "name": "GET /call"
                }
            }
        }]
    }
}
```


this: 


```sh
GET /apm-7.17.7-transaction-000001/_search?_source=_id,transaction.name

{
    "query": {
        "terms": {
            "http.request.headers.Appendedfield": [
                "value456"
            ]
        }
    }
}
```
does not work:
```JSON
{
    "took": 0,
    "timed_out": false,
    "_shards": {
        "total": 1,
        "successful": 1,
        "skipped": 0,
        "failed": 0
    },
    "hits": {
        "total": {
            "value": 0,
            "relation": "eq"
        },
        "max_score": null,
        "hits": []
    }
}
```

the variant
```
GET /apm-7.17.7-transaction-000001/_search?_source=_id,transaction.name
{
    "query": {
        "bool": {
            "must": [{
                "match": {
                    "http.request.headers.Appendedfield": "value456"
                }
            }]
        }

    }
}
```
does not find any documents 


the variant:
```sh
GET /apm-7.17.7-transaction-000001/_search?_source=_id,http.request.headers

{
    "query": {
        "wildcard": {
            "http.request.headers": {
                "value": "*"
            }
        }

    }
}
```
does not find any documents 

neither does
```sh
GET /apm-7.17.7-transaction-000001/_search?_source=_id,http.request.headers
{
    "query": {
        "exists": {
            "field": "http.request.headers"
        }

    }
}
```
NOTE:  is there a document field chain depth limit? the following does work:

```sh
GET /apm-7.17.7-transaction-000001/_search?_source=_id,http.request.headers

{
    "query": {

        "exists": {
            "field": "http.request"
        }

    }
}
```
### Adding Pipeline in console call

This update script can be also saved as a ingestion pipeline

```sh
  
  {    "script": {
      "source": "String data1 = ctx._source.transaction.name;String data2 = ctx._source.http.request.headers['Appendedfield'][0]; if (data1.indexOf(data2) == -1) ctx._source.transaction.name = data1 + '/' + data2; else ctx.op = 'noop'"
    }
  }




```

NOTE:  currently struggling with:
```
Invalid pipeline
Please ensure the JSON is a valid pipeline object.
```
when  editing the Pipeline



* add the pipeline via a curl call:
```sh
curl -XPUT -H 'Content-Type: application/json' "http://192.168.0.64:9200/_ingest/pipeline/apm_renametransaction_pipeline" -d "
{
  \"description\": \"appends the header to transaction name\",
\"processors\": [
    {
    \"script\": {
      \"source\": \"String data = ctx._source.http.request.headers['Appendedfield'][0]; if (ctx._source.transaction.name.indexOf(data ) == -1 ) ctx._source.transaction.name += '/' + data;\"
    }
  }
  ]
}"
```
this responds with
```json
{"acknowledged":true}
```

* inspect via Kibana GUI

![Custom Ingestion Pipeline](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-custom-ingestion-pieline.png)


* Added a `Set` filter:
```json
 {
    "set": {
      "field": "transaction.name",
      "value": "{{transaction.name}} / {{ http.request.headers.Appendedfield.0 }}"
    }
  }
```
![Custom Ingestion Pipeline](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-ingest-filter.png)

* Observed the SOAP-action-amended transaction names after doing a prepated REST calls:
```sh
curl -H "AppendedField: value25" http://192.168.0.64:6000/call
```


 ![Custom Ingestion Pipeline](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-transactions-with-names.png)



#### Processing the SOAPACtion Header before appending

the working update payload code example

```sh

POST /apm-7.17.7-transaction-000001/_doc/ZgddHoUBLFUYqZyXJEff/_update 
{
    "script": {
          "lang": "painless",
          "source": """
            // String data2 = 'http://xmlme.com/WebServices/GetSpeech';
            String data1 = ctx._source.transaction.name;
            String data2 = ctx._source.http.request.headers.Appendedfield[0];
            String[] data = data2.splitOnToken(params['delimiter']);
            String data3 = data[params['position']];
            if (data1.indexOf(data3) == -1)
              ctx._source.transaction.name = data1 + '/' + data3;
            else
              ctx.op = 'noop'
          """,
          "params": {
            "delimiter": "/",
            "position": -1
          }
    }
}

```

this leads to ingestion step attempt that does not work:
```javascript
  {
    "set": {
      "field": "transaction.name",
      "value": "{{transaction.name}} / {{ (http.request.headers.Appendedfield.0).splitOnToken('/')[-1] }}"
    }
  }
```

- nothing gets appended

the solution is to combine

GSUB 
```javascript
{
      "gsub": {
        "field": "http.request.headers.Appendedfield.0",
        "pattern": "^.*/",
        "replacement": "",
        "target_field": "http.request.headers.Appendedfield.0"
      }
    }
```
and 

SET 

```
{
      "gsub": {
        "field": "http.request.headers.Appendedfield.0",
        "pattern": "^.*/",
        "replacement": "",
        "target_field": "http.request.headers.Appendedfield.0"
      }
    }
```
pipelines. This way the request with a legitimate SOAPAction-like header:

```sh
for I in $(seq 1 1 10 );do J=$(expr $I % 2); curl -H "AppendedField: http://xmlme.com/WebServices/API${J}" http://localhost:6000/call ; sleep 30 ; done
```
produces the mix '/call/API1' and '/call/API2' "SOAP Call" transactions 

![Custom Ingestion Pipeline](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-transaction-renamed2.png)


### See Also

  * [monitoring python flask application with elastic apm](https://medium.com/analytics-vidhya/monitoring-python-flask-application-with-elastic-apm-bb0853f056ff)

  * [get application performance metrics on python flask witheElastic APM on kibana and elasticsearch](https://ruanbekker.medium.com/get-application-performance-metrics-on-python-flask-with-elastic-apm-on-kibana-and-elasticsearch-2859ea02ae30)

  * [setup APM Server on Ubuntu for Your Elastic Stack to Get Insights in Your Application Performance Metrics]( https://blog.ruanbekker.com/blog/2018/11/11/setup-apm-server-on-ubuntu-for-your-elastic-stack-to-get-insights-in-your-application-performance-metrics)

  * [finding local IP addresses using Python's stdlib](https://stackoverflow.com/questions/166506/finding-local-ip-addresses-using-pythons-stdlib)

  * ElasticSearch [Enrich APIs](https://www.elastic.co/guide/en/elasticsearch/reference/current/enrich-apis.html)

  * ElasticSearch [Ingest pipelines](https://www.elastic.co/guide/en/elasticsearch/reference/current/ingest.html)

  * ElasticSearch document Partial Update [documentation](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html)

  * ElasticSearch [Tune Data Ingestion](https://www.elastic.co/guide/en/apm/guide/master/tune-data-ingestion.html) on APM Server and ElasticSearch server

  * Elastic APM Server [Ingest node pipeline](https://www.elastic.co/guide/en/apm/server/7.15/elasticsearch-output.html#pipeline-option-es) to write events to  
   
  * Elastic APM Server [Custom Pipelines](https://www.elastic.co/guide/en/apm/server/7.15/configuring-ingest-node.html#custom-pipelines

  * [Using Ingest Pipelines to Enhance Elastic Observability Data](https://dzone.com/articles/using-ingest-pipelines-to-improve-elastic-observab)

  * ElasticSearch [Enrich Processor](https://www.elastic.co/guide/en/elasticsearch/reference/current/enrich-processor.html#enrich-processor)
  
  * [call SOAP Server from the command line](https://www.baeldung.com/call-soap-command-line)

  * [call SOAP Server from Postman] (https://www.baeldung.com/postman-soap-request)

  * building `SOAPAction` header [SOAP client app documentation](http://www.herongyang.com/Web-Services/SAAJ-addHeader-Set-SOAPAction-Header-Line.html) for java - uses `headers.addHeader("SOAPAction", uri+"/GetSpeech");` which leads to the header `SOAPAction: http://xmlme.com/WebServices/GetSpeech`

  * [how java code to set an attribute like SOAPAction in the Header?](https://coderanch.com/t/220498/java/set-attribute-SOAPAction-Header) shows example `https://coderanch.com/t/220498/java/set-attribute-SOAPAction-Header`

  * [predefined grok patterns](https://github.com/hpcugent/logstash-patterns/blob/master/files/grok-patterns)

  * `_Update` [examples](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html#docs-update

  * [Script processor](https://www.elastic.co/guide/en/elasticsearch/reference/7.17/script-processor.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
