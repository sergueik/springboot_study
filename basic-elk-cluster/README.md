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
if only fe indices are dislayed:

```text
green  open   .apm-agent-configuration         fFmjfMpPRWSnO9_dOR8zUQ   1   0          0            0       226b           226b
green  open   .apm-custom-link                 FjALsnNuRwujJMKq0lVB7g   1   0          0            0       226b           226b
```
this indicates the `apm-server` is misconfigured, check the container log of `apm-server`, and check its status in Kibana

```sh
docker logs apm-server
```
for error messages that look like:
```JSON
{
  "log.level": "error",
  "@timestamp": "2022-12-31T18:29:23.907Z",
  "log.logger": "publisher_pipeline_output",
  "log.origin": {
    "file.name": "pipeline/output.go",
    "file.line": 154
  },
  "message": "Failed to connect to backoff(elasticsearch(http://elasticsearch:9200)): Connection marked as failed because the onConnect callback failed: error loading Elasticsearch template: error creating template from file /usr/share/apm-server/fields.yml: yaml: line 10356: did not find expected key",
  "service.name": "apm-server",
  "ecs.version": "1.6.0"
}

```
and similar kind of log messages


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

#### Add ASP.Net Core node

run the command:
```sh
docker-compose -f docker-compose.yml -f docker-compose-including-aspnetcoreapp.yml up --build
```
#### Add Java SOAP node
build jar
```sh
cd app5;  mvn clean package
```
run the command:
```sh
docker-compose -f docker-compose.yml -f docker-compose-including-javasoap.yml up --build
```
#### Add NPM Express Blog page node

```sh
docker-compose -f docker-compose.yml -f docker-compose-including-express.yml up --build
```
```sh
docker container ls
```
will show
```text
CONTAINER ID   IMAGE                             COMMAND                  CREATED              STATUS                        PORTS                                                 NAMES
5d19262080d0   basic-elk-cluster-app6            "docker-entrypoint.s…"   About a minute ago   Up About a minute (healthy)   0.0.0.0:3000->3000/tcp, :::3000->3000/tcp             app6
4b4ac2988007   basic-elk-cluster-app1            "python /app/app.py"     19 minutes ago       Up 16 minutes (healthy)       0.0.0.0:6000->6000/tcp, :::6000->6000/tcp, 8080/tcp   app1
58c59e8c2b8a   basic-elk-cluster-app2            "python /app/app.py"     19 minutes ago       Up 16 minutes (healthy)       0.0.0.0:7000->7000/tcp, :::7000->7000/tcp, 8080/tcp   app2
35a04836e9d3   basic-elk-cluster-apm-server      "/usr/bin/tini -- /u…"   19 minutes ago       Up 16 minutes (healthy)       0.0.0.0:8200->8200/tcp, :::8200->8200/tcp             apm-server
eb571689c9d8   basic-elk-cluster-kibana          "/bin/tini -- /usr/l…"   19 minutes ago       Up 18 minutes (healthy)       0.0.0.0:5601->5601/tcp, :::5601->5601/tcp             kibana
e72ba232c224   basic-elk-cluster-elasticsearch   "/bin/tini -- /usr/l…"   19 minutes ago       Up 19 minutes (healthy)       0.0.0.0:9200->9200/tcp, :::9200->9200/tcp, 9300/tcp   elasticsearch

```

```sh
docker-compose -f docker-compose.yml -f docker-compose-including-express.yml ps
```
 will show
```text	
NAME                IMAGE                             COMMAND                  SERVICE             CREATED             STATUS                    PORTS
apm-server          basic-elk-cluster-apm-server      "/usr/bin/tini -- /u…"   apm-server          20 minutes ago      Up 17 minutes (healthy)   0.0.0.0:8200->8200/tcp, :::8200->8200/tcp
app1                basic-elk-cluster-app1            "python /app/app.py"     app1                20 minutes ago      Up 17 minutes (healthy)   0.0.0.0:6000->6000/tcp, :::6000->6000/tcp, 8080/tcp
app2                basic-elk-cluster-app2            "python /app/app.py"     app2                20 minutes ago      Up 17 minutes (healthy)   0.0.0.0:7000->7000/tcp, :::7000->7000/tcp, 8080/tcp
app6                basic-elk-cluster-app6            "docker-entrypoint.s…"   app6                3 minutes ago       Up 3 minutes (healthy)    0.0.0.0:3000->3000/tcp, :::3000->3000/tcp
elasticsearch       basic-elk-cluster-elasticsearch   "/bin/tini -- /usr/l…"   elasticsearch       20 minutes ago      Up 20 minutes (healthy)   0.0.0.0:9200->9200/tcp, :::9200->9200/tcp, 9300/tcp
kibana              basic-elk-cluster-kibana          "/bin/tini -- /usr/l…"   kibana              20 minutes ago      Up 19 minutes (healthy)   0.0.0.0:5601->5601/tcp, :::5601->5601/tcp

```
![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/captrue-cluster-with-node-express.png)

Calling `http://localhost:3000/getAPIResponse` will trigger an `express` to `flask` to `sqlite3` call which will be recored in distributed tracing:

![Distributed Tracing Call Example](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-fulltrace-express-python.png)

### Adding Desktop Browser to Test Distributed Tracing Invoked from Client-side Javascript

```sh
docker-compose -f docker-compose.yml -f docker-compose-including-express.yml -f docker-compose-client.yml up --build app7
```
```sh
ID=$(docker container ls |grep app7 |awk '{print $1}')
docker inspect $ID | jq '.[]|.NetworkSettings.Networks'
```
```JSON
{
  "basic-elk-cluster_elastic": {
    "IPAMConfig": null,
    "Links": null,
    "Aliases": [
      "app7",
      "app7",
      "8213fbed68aa"
    ],
    "NetworkID": "698818da7c2371668e5bfe3d164c350e01e9d5e65155a5227f5e89b2a7096fd8",
    "EndpointID": "601e25f4042022ed928d24891e2b2e6bec6515cfa6cde50799a2d9a3adfec3bd",
    "Gateway": "172.20.0.1",
    "IPAddress": "172.20.0.2",
    "IPPrefixLen": 16,
    "IPv6Gateway": "",
    "GlobalIPv6Address": "",
    "GlobalIPv6PrefixLen": 0,
    "MacAddress": "02:42:ac:14:00:08",
    "DriverOpts": null
  }
}
```
NOTE: for jq query to traverse node names with dash, [use double quotes](https://stackoverflow.com/questions/37344329/jq-not-working-on-tag-name-with-dashes-and-numbers):
```sh
IPADDRESS=$(docker inspect $ID | jq -cr '.[]|.NetworkSettings.Networks."basic-elk-cluster_elastic".IPAddress')
echo "IPADDRESS=$IPADDRESS"
```
```text
IPADDRESS=172.20.0.2
```
update remmina configuration
```sh
sed -i "s|^server=.*|server=${IPADDRESS}:0|g" app7/connection.remmina 
```
Connect to XVGFB server like to VNC server 
```sh
remmina -c app7/connection.remmina
```
you will be running as root, but can launch firefox browser from `st` shell:

![Xvfb Fluxbox Firefox](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-xvfb-firefox-remmina.png)

### Configuration

By default, the stack exposes the following ports:

  * 5000: Logstash TCP input.
  * 9200: Elasticsearch HTTP
  * 9300: Elasticsearch TCP transport
  * 5601: Kibana
  * 8200: APM

The monitored Apps run on the following ports:

  * 3000: Node.js Express
  * 6000: Flask #1
  * 7000: Flask #2 
  * 8000: Flask #3 
  * 5000: ASP.Net Core REST
  * 8888: Java SOAP

 
NOTE: the images are relatively heavy

```text
basic-elk-cluster_apm-server           latest                 c4ef445c0412   20 minutes ago   258MB
basic-elk-cluster_kibana               latest                 3a414cdc79d3   20 minutes ago   799MB
basic-elk-cluster_elasticsearch        latest                 a3f9ff0db620   23 minutes ago   619MB
basic-elk-cluster_app                  latest                 2902ce4b8a5c   2 days ago       133MB
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

* check if the APM documents get indexed

```sh
curl -XGET 'localhost:9200/apm-7.17.7-transaction-000001/_search?_source=false' | jq '.'
```

see both 

`http.request.headers.Custom-Request-Header` and `http.request.headers.Appendedfield`:

![Custom Appended Field](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-transaction-appended_field.png)


![Custom Request Header](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-transaction-custom-request-header.png)

show in Metadata


#### Modifying the APM Transaction Name


you will use APM document to when testing the ingestion pipelines

To reset document, update [just the selected field ](https://stackoverflow.com/questions/19563215/update-only-specific-field-value-in-elasticsearch) 
in console:
```sh

INDEX=apm-7.17.7-transaction-000001
TYPE=_doc
ID=eNptHIUBtUxcsrGvfl0g
curl -H 'Content-Type: application/json' -X POST "192.168.0.64:9200/$INDEX/$TYPE/$ID/_update" -d '{
    "doc" : {
        "transaction": {"name":  "GET /call"}
    }
}'
```

```sh
INDEX=apm-7.17.7-transaction-000001
TYPE=_doc
ID=ZgddHoUBLFUYqZyXJEff
curl -H 'Content-Type: application/json' -X POST "192.168.0.64:9200/$INDEX/$TYPE/$ID/_update" -d '{
    "doc": {
        "http": {
            "request": {
                "headers": {
                    "Appendedfield": ["http://xmlme.com/WebServices/GetSpeech"]
                }
            }
        }
    }
}
'
```
this will increment the document version
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

To find the documents with specific headers, exedcute the verbose __Dev Tools__ query 

```sh
GET /apm-7.17.7-transaction-000001/_doc/eNptHIUBtUxcsrGvfl0g?_source=transaction
```

can specify individual fields:
```sh
GET /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx?_source=transaction.name,http.request.headers.Custom-Request-Header
```
the most basic update is to concatenate. One have to use "index" 

```sh
POST /apm-7.17.7-transaction-000001/_doc/D9qeHIUBtUxcsrGvCHtx/_update 
{
  "script" : {
    "source": "ctx._source.transaction.name += ctx._source.http.request.headers['Custom-Request-Header'][0]",
    "lang": "painless",
    "params" : {
      "count" : 4
    }
  }
}

```

instead of "property" syntax

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


the latter will fail with:

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
NOTE: queries against nested fields in the documents are not reliable:

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


works, but this: 


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

#### Processing the SOAPACtion Header before appending

the canonical SOAPAction HTTP Header  looks like an url;
```text
SOAPAction: http://my.organization.org/webservices/WSSERVICE
```
To only keep the `WSSERVICE` part, the working update payload code example is

```sh

POST /apm-7.17.7-transaction-000001/_doc/ZgddHoUBLFUYqZyXJEff/_update 
{
    "script": {
          "lang": "painless",
          "source": """
            
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

but ingestion step attempt that does not work:
```javascript
  {
    "set": {
      "field": "transaction.name",
      "value": "{{transaction.name}} / {{ (http.request.headers.Appendedfield.0).splitOnToken('/')[-1] }}"
    }
  }
```

- nothing gets appended

the solution is to combine three pipelines (__SET__, __GSUB__, __SET__):


__SET__:

![Processor 1](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-processor1.png)

```JSON
{
  "set": {
    "field": "transaction.soapaction",
    "copy_from": "http.request.headers.Appendedfield.0"
  }
}
```

__GSUB__:


![Processor 2](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-processor2.png)

```JSON
{
  "gsub": {
    "field": "http.request.headers.Appendedfield.0",
    "pattern": "^.*/",
    "replacement": "",
    "target_field": "transaction.soapaction"
  }
}
```
(note the deep nested schema of the http request headers appended field)

and  __SET__ :



![Processor 3](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-processor3.png)

```JSON
{
  "set": {
    "field": "transaction.name",
    "value": "{{ transaction.name }}/{{ transaction.soapaction }}"
  }
}
```
This copies the extracted Action token into the new field `transaction.soapaction` at the `transaction` level (one can do the same and keep the original `transaction.name` as an alias. For illustration simplicity instead of sending / extracting the [fragment identifier of the URI](https://www.w3.org/2002/11/dbooth-names/rfc2396-numbered_clean.htm) - the part after the `#` hash symbol the example just grabs the last path element in the URI.

alternatively one can perform __GROK__, to create an `tansaction.soapaction` field in one step:
```JSON
 {
    "grok": {
      "field": "http.request.headers.Appendedfield.0",
      "patterns": [
        "%{GREEDYDATA}/%{GREEDYDATA:transaction.soapaction}"
      ],
      "ignore_missing": true
    }
  }
```
![Grok Debugger](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-grok-debugger.png)


and __SET__:

```JSON
{
  "set": {
    "field": "transaction.name",
    "value": "{{ transaction.name }}/{{ transaction.soapaction }}"
  }
}
```
The Grok filters are somehat easier to test via __Grok Debuger__ in  __Dev Tools__
This way the request with a legitimate SOAPAction-like header:

```sh
for I in $(seq 1 1 10 );do J=$(expr $I % 2); curl -H "AppendedField: http://xmlme.com/WebServices/API${J}" http://localhost:6000/call ; sleep 30 ; done
```
produces the mix '/call/API1' and '/call/API2' "SOAP Call" transactions 

![Custom Ingestion Pipeline](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-transaction-renamed2.png)

### History

The [Wikipedia page](https://en.wikipedia.org/wiki/SOAP) mentions
the original SOAP with its intentions to become the fundament of complex higher level services like 
[UDDI](https://en.wikipedia.org/wiki/Web_Services_Discovery#Universal_Description_Discovery_and_Integration)
(which was never widely accepted and support of which was eventually fully removed by major vendors by 2007) 
has failed to foresee the actual direction of evolution of Web Services.


The __SOAP 1.1__ specification was not approved by W3C to even 'W3C Recommendation' level, and it technically can not be considered a "web standard", defines `SOAPAction` HTTP header.

```text
SOAPAction: "http://electrocommerce.org/abc#MyMessage"
```
The stable release __SOAP 1.2__ [spec](https://www.w3.org/TR/2001/WD-soap12-20010709/#_Toc478383528) (which did became a W3C recommendation) also contains a section describing `SOAPAction` Header and the definition looks similar to that of __SOAP 1.1__ [spec](https://www.w3.org/TR/2000/NOTE-SOAP-20000508/#_Toc478383528)
but from training on the subject is known that __SOAP 1.2__ no longer enforces the `SOAPAction` Header to be present (there are far deeper differences between 1.2 and 1.1 than just that)

In fact the example SOAP message shown in Wikipedia page
```text
POST /InStock HTTP/1.1
Host: www.example.org
Content-Type: application/soap+xml; charset=utf-8
Content-Length: 299
SOAPAction: "http://www.w3.org/2003/05/soap-envelope"

<?xml version="1.0"?>
<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:m="http://www.example.org">
  <soap:Header>
  </soap:Header>
  <soap:Body>
    <m:GetStockPrice>
      <m:StockName>T</m:StockName>
    </m:GetStockPrice>
  </soap:Body>
</soap:Envelope>
```
apparently does contain a `SOAHeader` but in the exampleshown that header value which is of little use, since it is identically matches to the namespace of the undelying SOAP envelope, and not of the message element and
one would not be able to get any information concerning the actual API method call being serialized in the SOAP message from such header

The [SOAP 1.1 example payload](https://www.w3.org/TR/2000/NOTE-SOAP-20000508/#_Toc478383490) using the `SOAPAction` header example above would be
```text

POST /StockQuote HTTP/1.1
Host: www.stockquoteserver.com
Content-Type: text/xml; charset="utf-8"
Content-Length: nnnn
SOAPAction: "http://electrocommerce.org/abc#MyMessage"

<SOAP-ENV:Envelope
  xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
  SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
   <SOAP-ENV:Body>
       <m:MyMessage xmlns:m="http://electrocommerce.org/abc">
           <m:MyArgument>Hello</m:MyArgument>
       </m:MyMessagee>
   </SOAP-ENV:Body>
</SOAP-ENV:Envelope>
```
better illustrates the intent how the `SOAPAction` header must be constructed (it matches the XML namespace of the action element and presumably would also contain the name of the element after the hash symbol: `#GetLastTradePrice`).

Also, the [SOAP 1.2 example document](https://www.w3.org/TR/soap12-part1/#firstexample)
appears to be placethe namespace right in that element but there is no full  message example

```xml
<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope">
 <env:Header>
  <n:alertcontrol xmlns:n="http://example.org/alertcontrol">
   <n:priority>1</n:priority>
   <n:expires>2001-06-22T14:00:00-05:00</n:expires>
  </n:alertcontrol>
 </env:Header>
 <env:Body>
  <m:alert xmlns:m="http://example.org/alert">
   <m:msg>Pick up Mary at school at 2pm</m:msg>
  </m:alert>
 </env:Body>
</env:Envelope>
```

Why __SOAP 1.2__ the `SOAPAction` header no longer being required.

This is because the `SOAP-Envelope` includes its own specific `SOAP-Header` which makes the "Header" technically a part of the HTTP message body
and as such is not easily accessible to Elastic APM at the transaction level. 

Also due to several reasons SOAP document is very complex to parse. __SOAP 1.2__ does no longer expect the HTTP Header to be present precisely because with SOA protocol evolution it puts more and more wight in the Envelope parts to be self-describing


The SOAP protocol tendency to reimplement several existing features of HTTP is quoted among its disadvantages



### Processing Custom Header

* observe APM indices:

```sh
curl -s 'http://localhost:9200/_cat/indices?v&pretty'|grep 'apm'
```
```text
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  1920  100  1920    0     0   144k      0 --:--:-- --:--:-- --:--:--  144k
green  open   .apm-agent-configuration         bsEi1dBORbC1oZ8BXMwQxg   1   0          0            0       226b           226b
yellow open   apm-7.17.7-error-000001          WeUug1n-R1CBfI3Qzph5Qg   1   1          0            0       226b           226b
yellow open   apm-7.17.7-span-000001           1JtE97CPRwK3k7uU9JfidA   1   1          0            0       226b           226b
yellow open   apm-7.17.7-onboarding-2022.12.16 zfcQtDRbSoKIYMXuui13AQ   1   1          1            0      7.9kb          7.9kb
green  open   .apm-custom-link                 P6LEn3TqTmqyyOo5zuXHWA   1   0          0            0       226b           226b
yellow open   apm-7.17.7-profile-000001        KZYMLpLjQIO0zpIVwXs14w   1   1          0            0       226b           226b
yellow open   apm-7.17.7-metric-000001         weyOmQ_GT4CRz8i1LoJPng   1   1          2            0       19kb           19kb
yellow open   apm-7.17.7-transaction-000001    dVuxfasZQ-i8vvGYm16dSA   1   1          0            0       226b           226b
```


inspect the index
```
curl -s -X GET http://localhost:9200/apm-7.17.7-transaction-000001/_mappings
```
```JSON
{
  "apm-7.17.7-transaction-000001" : {
    "mappings" : {
      "_meta" : {
        "beat" : "apm",
        "version" : "7.17.7"
      },
      "dynamic_templates" : [
        {
          "labels" : {
            "path_match" : "labels.*",
... truncated ~ 7K lines of JSON
```

Repeat with search

```
curl -s -X GET http://localhost:9200/apm-7.17.7-transaction-000001/_mappings | jq '."apm-7.17.7-transaction-000001".mappings.properties.http.properties.request.properties.headers' | less
```
this will show

```JSON
{
  "properties": {
    "Custom_header": {
      "type": "keyword",
      "ignore_above": 1024
    }
  }
}
```

perform a few API calls with  header

Search
```
curl -s -X GET http://localhost:9200/apm-7.17.7-transaction-000001/_search
```

```sh
curl -H 'Content-Type: application/json' -s -d '{ "fields": ["http.request.headers"] }' -X GET http://localhost:9200/apm-7.17.7-transaction-000001/_search 
```


with filtering

```sh
curl -H 'Content-Type: application/json' -s -d '{ "fields": ["http.request.headers"] }' -X GET http://localhost:9200/apm-7.17.7-transaction-000001/_search  | jq '.hits.hits[]._source.http.request.headers."Custom-Header"'
```
```text
null
null
["value 3"]
["value 2"]
null
["value"]
```
alternatively do in Dev Tools console:
```
GET /apm-7.17.7-transaction-000001/_search { "fields": ["http.request.headers"] }
```


NOTE: actually both `fields.yml` and `apm-server.yml` work:

```sh
curl -s -X GET http://localhost:9200/apm-7.17.7-transaction-000001/_mappings | jq '."apm-7.17.7-transaction-000001".mappings'| grep -i custom
```
```text
                "Custom_header": {
                "Custom-Header": {
```
but it appears only `Custom-Header` has data

```
PUT /apm-7.17.7-transaction-000001/_mappings
{
  "runtime": {
    "action": {
      "type": "keyword",
      "script": """
        String data=grok('%{GREEDYDATA:filename}').extract(doc["http.request.headers.Custom-Header"].value)?.filename;
        if (data != null) emit(data); else  emit("unknown"); 
      """
    }
  }
}

```
```
GET /apm-7.17.7-transaction-000001/_search { "fields": ["http.request.headers.Custom-Header", "action"] }

```

```sh
GET /apm-7.17.7-transaction-000001/_doc/glAgGYUBS-3qJRiahbvM?_source=http.request.headers.Custom-Header

```

#### Extracting the SOAPAction

perform the valid XML element processing, keep the application-specific tag name
```sh
POST /apm-7.17.7-transaction-000001/_doc/PisIUYUBfKtxXCdrzw64/_update
{
  "script": {
    "lang": "painless",
    "source": """
      String parseSOAPXMLElement(def data) {
        def result = "";
         def elementMatcher = /<([\w]+):([\w]+)\s*(xmlns:)([\w]+)\s*=\s*"([^"]+)"\s*(.*)>/;
          if (data =~ elementMatcher){
            // NOTE: also need to inspect the namespace
            result = elementMatcher.matcher(data).replaceAll('$2');
        }  
        return trim(result);
      }
      String trim(def data){
        return / /.matcher(/\n|\t|\r/.matcher(data).replaceAll(' ')).replaceAll('');
      }

      String data = params['data'];
      def result  = parseSOAPXMLElement(data);

      ctx._source.transaction.name = result;
    """,
    "params": {
      "data": """
        <m:GetPrice xmlns:m="https://www.w3schools.com/prices">
      """,
      "position": -1
    }
  }
}
```
review the result
```sh
GET /apm-7.17.7-transaction-000001/_doc/PisIUYUBfKtxXCdrzw64?_source=transaction
```
see
```json
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "PisIUYUBfKtxXCdrzw64",
  "_version" : 5,
  "_seq_no" : 5,
  "_primary_term" : 2,
  "found" : true,
  "_source" : {
    "transaction" : {
      "result" : "HTTP 2xx",
      "duration" : {
        "us" : 58503
      },
      "name" : "GetPrice",
      "span_count" : {
        "dropped" : 0,
        "started" : 1
      },
      "id" : "003933126330c6d3",
      "type" : "request",
      "sampled" : true
    }
  }
}
```
* process the full SOAP XML payload

```sh
POST /apm-7.17.7-transaction-000001/_doc/PisIUYUBfKtxXCdrzw64/_update
{
  "script": {
    "lang": "painless",
    "source": """
      String parseSOAPXMLDocument(def data) {
        def result = "";
        // handle whitespace
        String[] elelentList = (/\n|\t|\r/.matcher(data).replaceAll(' ')).splitOnToken('<');
        // paginate to have one XML element per page
        for (def element : elelentList) {
          // extract tag name from relevant elements
          def tmp = parseSOAPXMLElement('<' + element);
           if (tmp != "") {
            result = tmp;
          }
        }
        return result;
      }
      String parseSOAPXMLElement(def data) {
        def result = "";
         def elementMatcher = /<([\w]+):([\w]+)\s*(xmlns:)([\w]+)\s*=\s*"([^"]+)"\s*(.*)>/;
          if (data =~ elementMatcher){
            def namespace = elementMatcher.matcher(data).replaceAll('$1');
            def tmp = elementMatcher.matcher(data).replaceAll('$2');
            // NOTE: there is no !~ expression
            if (!(namespace =~ /soap/)){
              // NOTE: one cannot have an empty if ... else block: "illegal_argument_exception", "extraneous if block"
              result = tmp;
            }
        }
        return trim(result);
      }
      String trim(def data){
        return / /.matcher(/\n|\t|\r/.matcher(data).replaceAll(' ')).replaceAll('');
      }
      String data = params['data'];
      def result = parseSOAPXMLDocument(data);

      ctx._source.transaction.name = result;
  """,
  "params": {
    "delimiter": "<",
    "data": """
      <?xml version="1.0"?>


      <soap:Envelope
      xmlns:soap="http://www.w3.org/2003/05/soap-envelope/"
      soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">

      <soap:Body>
        <m:UpdatePrice
        xmlns:m="https://www.w3schools.com/prices"
        soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding"
        ><m:Item>
          Apples</m:Item>
        </m:UpdatePrice>
      </soap:Body>
    """
    }
  }
}

```
the status is success:
```text
#! [types removal] Specifying types in document update requests is deprecated, use the endpoint /{index}/_update/{id} instead.
```
```json
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "PisIUYUBfKtxXCdrzw64",
  "_version" : 10,
  "result" : "updated",
  "_shards" : {
    "total" : 2,
    "successful" : 1,
    "failed" : 0
  },
  "_seq_no" : 10,
  "_primary_term" : 2
}
```
review the document:
```sh
GET /apm-7.17.7-transaction-000001/_doc/PisIUYUBfKtxXCdrzw64?_source=transaction

```

see

```json
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "PisIUYUBfKtxXCdrzw64",
  "_version" : 9,
  "_seq_no" : 9,
  "_primary_term" : 2,
  "found" : true,
  "_source" : {
    "transaction" : {
      "result" : "HTTP 2xx",
      "duration" : {
        "us" : 58503
      },
      "name" : "UpdatePrice",
      "span_count" : {
        "dropped" : 0,
        "started" : 1
      },
      "id" : "003933126330c6d3",
      "type" : "request",
      "sampled" : true
    }
  }
}

```


### APM Transaction Fields
* this section is restored from commit `23f4d0969cd05e38ca62dc26b1e3f4c76ff2e632`*
The most recent version of ELK 
__APM Transaction fields__ (Transaction-specific data for __APM__) [documentation](https://www.elastic.co/guide/en/apm/guide/master/exported-fields-apm-transaction.html)
says about `http.request.headers` and `http.response.headers` that <font color="red">Object is not enabled</font>. 
In fact these are the only propertes with this status.

a similar [request from customer](https://discuss.elastic.co/t/searching-for-a-custom-made-header/200403) concerning availability of some headers was advices i DIY fashion through
programmatically  adding the value of interest 
using APM agent public APIs []() and set the values of these headers as labels in the transaction that corresponds the request handling

* APM Java Agent Public API [reference](https://www.elastic.co/guide/en/apm/agent/java/current/public-api.html)
* APM .Net Agent Public API [reference](https://www.elastic.co/guide/en/apm/agent/dotnet/current/public-api.html)

say

`Transaction` `setLabel(String key, value)` (Added in 1.5.0 as addLabel) in Java and
`void SetLabel(string key, T value)` [1.7.0] in .Net

Labels are used to add *indexed information* (searchable and aggregatable) to __transactions__, __spans__, and __errors__. 

The __APM Transaction Field__ [documentation](https://www.elastic.co/guide/en/apm/guide/master/exported-fields-apm-transaction.html) defines

--- | --- 
`labels` | A flat mapping of user-defined labels with string, boolean or number values
type |object
Yes  | ECS field

for Python there  is no such document as "public-api.html" and the full [method reference](https://www.elastic.co/guide/en/apm/agent/python/current/index.html)



### Changes in Elastic Search Release 8.x

* Looking for the references to the default configuration
```YAML
setup.template.enabled: true
#setup.template.name: "apm-%{[observer.version]}"
#setup.template.pattern: "apm-%{[observer.version]}-*"
setup.template.fields: "${path.config}/fields.yml"
setup.template.overwrite: true
```

those can be found in Elastic Search release __7.x__ in [https://github.com/elastic/apm-server/blob/v7.17.7/apm-server.yml#L557](https://github.com/elastic/apm-server/blob/v7.17.7/apm-server.yml#L557)

however *the experimental config option* suggested in __Searching for a custom made heade__
[customer discussion resolution by Elastic](https://discuss.elastic.co/t/searching-for-a-custom-made-header/200403/2)
```YAML
setup.template.append_fields:
  - name: http.response.headers
    type: object
  - name: http.response.headers.Custom-Header
    type: keyword

```

was nowhere found in scanning various revisions of https://github.com/elastic/apm-server/ repository

Also the 8.x no longer has `setup.template.fields` not any other configurations in `setup.template`


and removed in the release __v8.1.1__ in the commit
[01d75ec0e231832487e739f2d4c174c9534208d3](https://github.com/elastic/apm-server/commit/01d75ec0e231832487e739f2d4c174c9534208d3)

```text
Remove index management, always use data streams (#6606)
* Remove index management, always use data streams

- Remove `apm-server.ilm.*` config
- Remove `apm-server.data_streams.enabled` config
- Remove `setup.*` config
- Remove `output.elasticsearch.{index,indices}` config
- Stop building, including, and using libbeat fields

* Update notice

A bunch of transitive dependencies are removed, because we're
no longer including the x-pack/libbeat processors.

* Update README

* Fix merge
```


it was not tested thoroughfully if the feature was restored in later 8.x releases - most likely it is compeltely gone.
in the [main branch](https://github.com/elastic/apm-server/blob/main/apm-server.yml) 
the `setup.template.*` configuration no longer exist in `apm-server.yml`

### APM Server 8.x Differences

* To inspect differences

* pull image
```sh
APM_SERVER_VERSION=8.5.2
docker pull elastic/apm-server:$APM_SERVER_VERSION
```
* run the shell in the container
```sh
docker run -it elastic/apm-server:$APM_SERVER_VERSION sh
```
* in the container inspect the `apm-server.yml`
```sh
ls /usr/share/apm-server
cd /usr/share/apm-server
more apm-server.yml
```

* note that in the `Elasticsearch output` section there is nothing related to templates

*  cleanup
```sh
docker image ls |grep apm-server | grep $APM_SERVER_VERSION | awk '{print $3}'| xargs -IX docker image rm -f X
```
NOTE: the following will not do it
```sh
docker image rm apm-server:$APM_SERVER_VERSION
```

### APM Agent SOAP Call processing

* add two more nodes:
    + `soap-server` SOAP Server (Java) with APM agent
    + `app4` Python Flask app with [zeep - Python SOAP client](https://docs.python-zeep.org/en/master/) with APM agent


![Cluster With SOAP Agent](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/captrue-cluster-with-soapserver.png)

![SOAP Agent](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-cluster-with-soap.png)

* configure the APM agents to collect header and body

![Agent Settings 1](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-agent-settings-1.png)

![Agent Settings 2](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-agent-settings-2.png)

* invoke the `Currency Converter` by `app4` 

![Currency Converter](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-currency-converter.png)

* observe the distributed transaction timeline and metadata collected:

![SOAP Call](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-soap-call.png)


* ispect details of HTTP, see body in one call

![SOAP HTTP Call Transaction 1](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-transaction-details-1.png)

* ispect details of HTTP, see no body and empty `SOAPAction` header in chained call

![SOAP HTTP Call Transaction 2](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-transaction-details-2.png)


### Switch to APM 8.5

* pull images
```sh
VERSION=8.5.2
```
```sh
docker pull elasticsearch:$VERSION
docker pull kibana:$VERSION
docker pull elastic/apm-server:$VERSION
```
* inspact configurations
```sh
NAME=apm-server.$VERSION
docker run --name $NAME --rm -it elastic/apm-server:$VERSION sh
docker cp $NAME:/usr/share/apm-server/apm-server.yml .
```

### Script Pipeline Step

Posted the sample XML using shell script `event2.sh`:
```sh
#!/bin/sh
DATA=$(cat <<EOF)
<?xml version="1.0"?>
<soap:Envelope
xmlns:soap="http://www.w3.org/2003/05/soap-envelope/"
soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">

<soap:Body>
  <m:UpdatePrice
  xmlns:m="https://www.w3schools.com/prices"
  soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding"
  ><m:Item>
    Apples</m:Item>
  </m:UpdatePrice>
</soap:Body>
EOF
curl -X POST -d "$DATA" http://localhost:6000/call2

```
the format of the data (note the quotes etc.):

```text
{'<?xml version': '"1.0"?>\n<soap:Envelope\nxmlns:soap="http://www.w3.org/2003/05/soap-envelope/"\nsoap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">\n\n<soap:Body>\n <m:UpdatePrice\n xmlns:m="https://www.w3schools.com/prices"\n soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding"\n ><m:Item>\n Apples</m:Item>\n </m:UpdatePrice>\n</soap:Body>'}
```


Found the document which was added (visual scan, need a better way):

```sh
GET /apm-7.17.7-transaction-000001/_doc/FlzJmIUBVsWXF39TlJmq?_source=http,transaction
```

Added pipelines

__SET__ (copying from one field `http.request.body.original` to the other `transaction.body`):
```JSON
{
    "set": {
      "field": "transaction.body",
      "copy_from": "http.request.body.original"
}
```    
result of pipeline test with specific document with `index`:`apm-7.17.7-transaction-000001` `id`:`FlzJmIUBVsWXF39TlJmq`

```JSON 
{
   "_id": "FlzJmIUBVsWXF39TlJmq",
    "_index": "apm-7.17.7-transaction-000001"
}
```
```JSON
{
  "docs": [
    {
      "doc": {
        "_index": "apm-7.17.7-transaction-000001",
        "_type": "_doc",
        "_id": "FlzJmIUBVsWXF39TlJmq",
        "_source": {
          "container": {
            "id": "017c4a89f4db685c1310325dca8141b39c95bffb1c8dba1b560e81a74e680c47"
          },
          "process": {
            "args": [
              "/app/app.py"
            ],
            "pid": 1,
            "ppid": 0
          },
          "agent": {
            "name": "python",
            "version": "6.13.2"
          },
          "source": {
            "ip": "172.20.0.1"
          },
          "processor": {
            "name": "transaction",
            "event": "transaction"
          },
          "url": {
            "path": "/call2",
            "scheme": "http",
            "port": 6000,
            "domain": "localhost",
            "full": "http://localhost:6000/call2"
          },
          "observer": {
            "hostname": "5b8943718007",
            "id": "b6b46367-453c-4504-8dd7-3724181b8f77",
            "ephemeral_id": "af20d66f-dac5-4277-97cf-7511d8cea79c",
            "type": "apm-server",
            "version": "7.17.7",
            "version_major": 7
          },
          "trace": {
            "id": "02dc95aa29c5f3311f2a233a0044eb76"
          },
          "@timestamp": "2023-01-09T23:06:57.534Z",
          "ecs": {
            "version": "1.12.0"
          },
          "service": {
            "name": "App 1",
            "node": {
              "name": "017c4a89f4db685c1310325dca8141b39c95bffb1c8dba1b560e81a74e680c47"
            },
            "runtime": {
              "name": "CPython",
              "version": "3.8.2"
            },
            "language": {
              "name": "python",
              "version": "3.8.2"
            },
            "framework": {
              "name": "flask",
              "version": "2.0.3"
            }
          },
          "host": {
            "name": "017c4a89f4db",
            "hostname": "017c4a89f4db",
            "os": {
              "platform": "linux"
            },
            "ip": "172.20.0.6",
            "architecture": "x86_64"
          },
          "client": {
            "ip": "172.20.0.1"
          },
          "http": {
            "request": {
              "headers": {
                "User-Agent": [
                  "curl/7.58.0"
                ],
                "Host": [
                  "localhost:6000"
                ],
                "Accept": [
                  "*/*"
                ],
                "Content-Length": [
                  "354"
                ],
                "Content-Type": [
                  "application/x-www-form-urlencoded"
                ]
              },
              "method": "POST",
              "env": {
                "SERVER_PORT": "6000",
                "REMOTE_ADDR": "172.20.0.1",
                "SERVER_NAME": "0.0.0.0"
              },
              "body": {
                "original": "{'<?xml version': '\"1.0\"?>\\n<soap:Envelope\\nxmlns:soap=\"http://www.w3.org/2003/05/soap-envelope/\"\\nsoap:encodingStyle=\"http://www.w3.org/2003/05/soap-encoding\">\\n\\n<soap:Body>\\n  <m:UpdatePrice\\n  xmlns:m=\"https://www.w3schools.com/prices\"\\n  soap:encodingStyle=\"http://www.w3.org/2003/05/soap-encoding\"\\n  ><m:Item>\\n    Apples</m:Item>\\n  </m:UpdatePrice>\\n</soap:Body>'}"
              }
            },
            "response": {
              "headers": {
                "Content-Length": [
                  "0"
                ],
                "Content-Type": [
                  "text/html; charset=utf-8"
                ]
              },
              "status_code": 200
            }
          },
          "event": {
            "ingested": "2023-01-09T23:11:13.196783895Z",
            "outcome": "success"
          },
          "transaction": {
            "result": "HTTP 2xx",
            "duration": {
              "us": 1092
            },
            "name": "POST /call2",
            "span_count": {
              "dropped": 0,
              "started": 0
            },
            "id": "cf8a4c1a183bfa14",
            "type": "request",
            "body": "{'<?xml version': '\"1.0\"?>\\n<soap:Envelope\\nxmlns:soap=\"http://www.w3.org/2003/05/soap-envelope/\"\\nsoap:encodingStyle=\"http://www.w3.org/2003/05/soap-encoding\">\\n\\n<soap:Body>\\n  <m:UpdatePrice\\n  xmlns:m=\"https://www.w3schools.com/prices\"\\n  soap:encodingStyle=\"http://www.w3.org/2003/05/soap-encoding\"\\n  ><m:Item>\\n    Apples</m:Item>\\n  </m:UpdatePrice>\\n</soap:Body>'}",
            "sampled": true
          },
          "user_agent": {
            "name": "curl",
            "original": "curl/7.58.0",
            "device": {
              "name": "Other"
            },
            "version": "7.58.0"
          },
          "timestamp": {
            "us": 1673305617534056
          }
        },
        "_ingest": {
          "timestamp": "2023-01-09T23:11:13.196783895Z"
        }
      }
    }
  ]
}
```

while not yet figured how to read and write the index being ingested, use `params`: for input and TBD how access `docs` for output.
 + elementary
```JSON

  {
    "script": {
      "lang": "painless",
      "\r\nString data = \"x\"+ ctx['transaction']['body'] + \"x\"; \r\n\r\nctx['transaction']['body'] = data;"
      "params": {
        "data": "\r\n      <?xml version=\"1.0\"?>\r\n\r\n\r\n      <soap:Envelope\r\n      xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope/\"\r\n      soap:encodingStyle=\"http://www.w3.org/2003/05/soap-encoding\">\r\n\r\n      <soap:Body>\r\n        <m:UpdatePrice\r\n        xmlns:m=\"https://www.w3schools.com/prices\"\r\n        soap:encodingStyle=\"http://www.w3.org/2003/05/soap-encoding\"\r\n        ><m:Item>\r\n          Apples</m:Item>\r\n        </m:UpdatePrice>\r\n      </soap:Body>\r\n    "
      }
    }
```
+ basic

```JSON

  {
    "script": {
      "lang": "painless",
      "source": "      String parseSOAPXMLDocument(def data) {\r\n        def result = \"\";\r\n        String[] elelentList = (/\\n|\\t|\\r/.matcher(data).replaceAll(' ')).splitOnToken('<');\r\n        for (def element : elelentList) {\r\n          def tmp = parseSOAPXMLElement('<' + element);\r\n           if (tmp != \"\") {\r\n            result = tmp;\r\n          }\r\n        }\r\n        return result;\r\n      }\r\n      String parseSOAPXMLElement(def data) {\r\n        def result = \"\";\r\n         def elementMatcher = /<([\\w]+):([\\w]+)\\s*(xmlns:)([\\w]+)\\s*=\\s*\"([^\"]+)\"\\s*(.*)>/;\r\n          if (data =~ elementMatcher){\r\n            def namespace = elementMatcher.matcher(data).replaceAll('$1');\r\n            def tmp = elementMatcher.matcher(data).replaceAll('$2');\r\n            if (!(namespace =~ /soap/)){\r\n              result = tmp;\r\n            }\r\n        }\r\n        return trim(result);\r\n      }\r\n      String trim(def data){\r\n        return / /.matcher(/\\n|\\t|\\r/.matcher(data).replaceAll(' ')).replaceAll('');\r\n      }\r\n\r\n\r\nString data = ctx['transaction']['body'];\r\nString  result = parseSOAPXMLElement(params['data']);\r\nctx['transaction']['body2'] = result;",
      "params": {
        "data": "\r\n      <m:GetPrice xmlns:m=\"https://www.w3schools.com/prices\">\r\n    "
```
 + semi-final
```JSON
  {
    "script": {
      "lang": "painless",
      "source": "      String parseSOAPXMLDocument(def data) {\r\n        def result = \"\";\r\n        String[] elelentList = (/\\n|\\t|\\r/.matcher(data).replaceAll(' ')).splitOnToken('<');\r\n        for (def element : elelentList) {\r\n          def tmp = parseSOAPXMLElement('<' + element);\r\n           if (tmp != \"\") {\r\n            result = tmp;\r\n          }\r\n        }\r\n        return result;\r\n      }\r\n      String parseSOAPXMLElement(def data) {\r\n        def result = \"\";\r\n         def elementMatcher = /<([\\w]+):([\\w]+)\\s*(xmlns:)([\\w]+)\\s*=\\s*\"([^\"]+)\"\\s*(.*)>/;\r\n          if (data =~ elementMatcher){\r\n            def namespace = elementMatcher.matcher(data).replaceAll('$1');\r\n            def tmp = elementMatcher.matcher(data).replaceAll('$2');\r\n            if (!(namespace =~ /soap/)){\r\n              result = tmp;\r\n            }\r\n        }\r\n        return trim(result);\r\n      }\r\n      String trim(def data){\r\n        return / /.matcher(/\\n|\\t|\\r/.matcher(data).replaceAll(' ')).replaceAll('');\r\n      }\r\n\r\n\r\nString data = ctx['transaction']['body'];\r\nString  result = parseSOAPXMLElement(params['data']);\r\nresult = parseSOAPXMLDocument(data);\r\nctx['transaction']['body2'] = result;",
      "params": {
        "data": "\r\n      <m:GetPrice xmlns:m=\"https://www.w3schools.com/prices\">\r\n    "
      }
    }
```

 + fixed

```JSON

  {
    "script": {
      "lang": "painless",
      "source": "      String parseSOAPXMLDocument(def data) {\r\n        def result = \"\";\r\n        String[] elelentList = (/\\n|\\t|\\r|\\\\r/.matcher(data).replaceAll(' ')).splitOnToken('<');\r\n        for (def element : elelentList) {\r\n          def tmp = parseSOAPXMLElement('<' + element);\r\n           if (tmp != \"\") {\r\n            result = tmp;\r\n          }\r\n        }\r\n        return result;\r\n      }\r\n      String parseSOAPXMLElement(def data) {\r\n        def result = \"\";\r\n         def elementMatcher = /<([\\w]+):([\\w]+)\\s*(xmlns:)([\\w]+)\\s*=\\s*\"([^\"]+)\"\\s*(.*)>/;\r\n          if (data =~ elementMatcher){\r\n            def namespace = elementMatcher.matcher(data).replaceAll('$1');\r\n            def tmp = elementMatcher.matcher(data).replaceAll('$2');\r\n            if (!(namespace =~ /soap/)){\r\n              result = tmp;\r\n            }\r\n        }\r\n        return trim(result);\r\n      }\r\n      String trim(def data){\r\n        return / /.matcher(/\\n|\\t|\\\\r/.matcher(data).replaceAll(' ')).replaceAll('');\r\n      }\r\n\r\n\r\nString data = ctx['transaction']['body'];\r\nString  result = parseSOAPXMLElement(params['data']);\r\nctx['transaction']['body2'] = result;",
      "params": {
        "data": "\r\n      <m:GetPrice xmlns:m=\"https://www.w3schools.com/prices\">\r\n    "
      }
    }
  }
```

![Capture Pipeline Script Design](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-pipeline-script-design.png)

![Capture Pipeline Test](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-pipeline-script-testing.png)

### Capture Body Quirks


![Capture Body 1](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-body-fields.png)


![Capture Body 3](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-body-raw-misplaced.png)

![Capture Body 2](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-body-workaround.png)

![Capture Body 4](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-body-workaround-unneeded.png)

![Capture Body 5](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-body-final-example.png)

### Testing Script Processor Step in Dev Tools

* it is quicker to run the `POST` `_update` followed by `GET` in Dev Tools than to repeatedly enter the document id and index in Ingestion Pipeline menu
```text
POST /apm-7.17.7-transaction-000001/_doc/FlzJmIUBVsWXF39TlJmq/_update 
{
  "script": {
    "lang": "painless",
    "source": """
    String parseSOAPXMLDocument(def data) {
      def result = "";
      String[] elelentList = (/\n|\t|\r|\\r|\\n/.matcher(data).replaceAll(' ')).splitOnToken('<');

      for (def item: elelentList) {
        String element = '<' + trim(item);
        def tmp = parseSOAPXMLElement(element);
        if (tmp != "") {
          result = tmp;
        }
      }
      return result;
    }
    String parseSOAPXMLElement(def data) {
      def result = "";
      def elementMatcher = /<([\w]+):([\w]+)\s*(xmlns:)([\w]+)\s*=\s*"([^"]+)"\s*(.*)>/;
      // beautify JS  breaks the =~ into =<space>~ which leads to compile error
      if (data =~ elementMatcher) {
        def namespace = elementMatcher.matcher(data).replaceAll('$1');
        def tmp = elementMatcher.matcher(data).replaceAll('$2');
        if (!(namespace =~/soap/)) {
          result = tmp;
        }
      }
      return trim(result);
    }
    String trim(def data) {
      return / /.matcher(/\n|\t|\\r/.matcher(data).replaceAll(' ')).replaceAll('');
    }



    // String data = ctx['transaction']['body'];
    String data = ctx['_source']['transaction']['body'];

    if (data != null) {
      String result = parseSOAPXMLDocument(data);
      ctx._source.transaction.action = result;
    } else { 
      ctx._source.transaction.action = 'undefined';
    }
    """
  }
}
```

* for simplicity put the `body` into `transaction.body` and only query the transaction`:

```text
GET /apm-7.17.7-transaction-000001/_doc/FlzJmIUBVsWXF39TlJmq?_source=transaction
```
* result:
```JSON
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "FlzJmIUBVsWXF39TlJmq",
  "_version" : 24,
  "_seq_no" : 5746,
  "_primary_term" : 3,
  "found" : true,
  "_source" : {
    "transaction" : {
      "result" : "HTTP 2xx",
      "duration" : {
        "us" : 1092
      },
      "name" : "POST /call2",
      "span_count" : {
        "dropped" : 0,
        "started" : 0
      },
      "id" : "cf8a4c1a183bfa14",
      "type" : "request",
      "body" : """{'<?xml version': '"1.0"?>\n<soap:Envelope\nxmlns:soap="http://www.w3.org/2003/05/soap-envelope/"\nsoap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">\n\n<soap:Body>\n  <m:UpdatePrice\n  xmlns:m="https://www.w3schools.com/prices"\n  soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding"\n  ><m:Item>\n    Apples</m:Item>\n  </m:UpdatePrice>\n</soap:Body>'}""",
      "sampled" : true,
      "body2" : "UpdatePrice",
      "action" : "UpdatePrice"
    }
  }
}

```
* when  finished convert ad-hoc script to Ingeston Pipeline. The only needed change is to replace

```javascript
    String data = ctx['_source']['transaction']['body'];
```

with
```javascript
    String data = ctx['transaction']['body'];
```
thus dropping or adding the key `_source` which is only needed for ad-hoc.

*  validating more complex SOAP document

the elementary parser successfully handles the data extraction from significantly more complex payload because it so happens that the tag of interest was the last encountered and waa carried. A number of optimizations is certainly possible:

* result:

```JSON
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "FlzJmIUBVsWXF39TlJmq",
  "_version" : 32,
  "_seq_no" : 6372,
  "_primary_term" : 3,
  "found" : true,
  "_source" : {
    "transaction" : {
      "result" : "HTTP 2xx",
      "duration" : {
        "us" : 1092
      },
      "name" : "POST /call2",
      "span_count" : {
        "dropped" : 0,
        "started" : 0
      },
      "id" : "cf8a4c1a183bfa14",
      "type" : "request",
      "body" : """ 
      {'
       <?xml version="1.0"?>
<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope/" soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">
  <soap:Header>
    <appId>12345</appId>
    <n1:context xmlns:n1="http://www.w3.org/soap-envelope/n1"/>
  </soap:Header>
  <soap:Body>
    <m3:UpdatePriceComplex xmlns:m1="https://www.w3schools.com/m1" xmlns:m2="https://www.w3schools.com/m2" xmlns:m3="https://www.w3schools.com/m3" xmlns:m4="https://www.w3schools.com/m4" xmlns:m5="https://www.w3schools.com/m5" soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">
      <m1:Item>
    Banana</m1:Item>
    </m3:UpdatePriceComplex>
  </soap:Body>
</soap:Envelope>
    '}

      """,
      "sampled" : true,
      "action" : "UpdatePriceComplex"
    }
  }
}

```
### TODO


  * add [elastic filebeat](https://hub.docker.com/layers/elastic/filebeat/7.17.7) node for practicing Kibana/Lucene queries on the same cluster
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


### See Also


  * ElasticSearch
    + [Enrich APIs](https://www.elastic.co/guide/en/elasticsearch/reference/current/enrich-apis.html)
    + [Ingest pipelines](https://www.elastic.co/guide/en/elasticsearch/reference/current/ingest.html)
    + [Partial Update](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html)
    + [Tune Data Ingestion on APM Server and ElasticSearch server](https://www.elastic.co/guide/en/apm/guide/master/tune-data-ingestion.html) 
    + [Ingest node pipeline](https://www.elastic.co/guide/en/apm/server/7.15/elasticsearch-output.html#pipeline-option-es)
    + [Custom Pipelines](https://www.elastic.co/guide/en/apm/server/7.15/configuring-ingest-node.html#custom-pipelines)
    + [Using Ingest Pipelines to Enhance Elastic Observability Data](https://dzone.com/articles/using-ingest-pipelines-to-improve-elastic-observab)
    + [Enrich Processor](https://www.elastic.co/guide/en/elasticsearch/reference/current/enrich-processor.html#enrich-processor)
    + [Predefined grok patterns](https://github.com/hpcugent/logstash-patterns/blob/master/files/grok-patterns)
    + [Update Examples](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-update.html#docs-update)
    + [Script processor](https://www.elastic.co/guide/en/elasticsearch/reference/7.17/script-processor.html) - unclear how to make modifications to the metadata document from `SCRIPT` step
    + [blog about Ingest Pipeline](https://hevodata.com/learn/elasticsearch-ingest-pipeline/) - shows the `_ingest/pipeline/_simulate` and `_ingest/pipeline/sample-pipeline-id` REST API for automating simulation of the ingestion etc.
    + [log Correlation](https://www.elastic.co/guide/en/apm/agent/python/current/log-correlation.html)
    + [cluster state](https://www.elastic.co/guide/en/elasticsearch/reference/7.17/cluster-state.html) - to save and load the custom Ingest Pipeline steps

   * Painless ElasticSearch Scripting Language
     + [Script Examples](https://www.elastic.co/guide/en/elasticsearch/painless/7.0/painless-examples.html)
     + [Language Specification](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-lang-spec.html) - painless scripting language is rather simple but obscure
     + [Painless API Reference](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-api-reference.html)
     + [Java Classes exposed to Painless](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-api-reference-shared.html) a.k.a. Shared API
     + [Functions](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-functions.html)
     + [Walkthrough](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-walkthrough.html) - shows example using regex to replace the matches in a string
     + [Custom ScriptEngine](https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-scripting-engine.html#modules-scripting-engine)
     + [How To Script Painless-ly in Elasticsearch](https://www.compose.com/articles/how-to-script-painless-ly-in-elasticsearch/)
     + [old repository](https://github.com/rmuir/Painless) from 2015
     + [Circuit Breaker Settings](https://www.elastic.co/guide/en/elasticsearch/reference/current/circuit-breaker.html)
     + [repository](https://github.com/elastic/elasticsearch/tree/main/plugins/examples/script-expert-scoring) with example plugin implementing the interface `ScriptPlugin` and include a [test suite](https://github.com/elastic/elasticsearch/blob/main/plugins/examples/script-expert-scoring/src/yamlRestTest/java/org/elasticsearch/example/expertscript/ExpertScriptClientYamlTestSuiteIT.java) that is required for including in ElasticSearch


  * SOAP

     + building `SOAPAction` header [SOAP client app documentation](http://www.herongyang.com/Web-Services/SAAJ-addHeader-Set-SOAPAction-Header-Line.html) for java - uses `headers.addHeader("SOAPAction", uri+"/GetSpeech");` which leads to the header `SOAPAction: http://xmlme.com/WebServices/GetSpeech`
     + [how java code to set an attribute like SOAPAction in the Header?](https://coderanch.com/t/220498/java/set-attribute-SOAPAction-Header) shows example `https://coderanch.com/t/220498/java/set-attribute-SOAPAction-Header`
     + Java __Spring-WS__-based  library [repository](https://github.com/reficio/soap-ws),  that enables handling SOAP on a purely XML level and with SOAP standards supports version 1.1 and 1.2 - the `SOAPAction` attribute is automatically [properly placed](https://github.com/reficio/soap-ws/search?p=2&q=soapaction)
     + [call SOAP Server from Postman] (https://www.baeldung.com/postman-soap-request)
     + [call SOAP Server from the command line](https://www.baeldung.com/call-soap-command-line)
     + [lxml - XML and HTML with Python](https://lxml.de)
     + [zeep - Python SOAP client](https://docs.python-zeep.org/en/master/)
     + [plain Java Web Service](https://docs.oracle.com/javase/7/docs/api/javax/jws/package-summary.html) 
     + [Introduction to JAX-WS](https://www.baeldung.com/jax-ws)
     + [java-ws and .net interop example](https://gridwizard.wordpress.com/2014/12/26/java-ws-and-dotnet-interop-example/)

     + [run APM agent from the subject application](https://www.elastic.co/guide/en/apm/agent/java/current/setup-attach-api.html)
     + [running APM agent via Java javaagentflag](https://www.elastic.co/guide/en/apm/agent/java/current/setup-attach-api.html)

    + [Passing SOAPHeader with zeep](https://docs.python-zeep.org/en/master/headers.html)
    + [SOAP 1.2 vs. SOAP 1.1 HTTP Headers](https://github.com/mvantellingen/python-zeep/issues/211) -   
also, seems that SOAP 1.2 requires the action to be set via the Content-Type header instead of the separate SOAPAction header as in 1.1

    + Zeep has low level `post_xml(address, envelope, headers)` [transport method](https://docs.python-zeep.org/en/master/api.html?highlight=post_xml#zeep.Transport.post_xml) to allow Post the envelope xml element to the given address with the headers (e.g. `{"SOAPAction": '"convert"',"Content-Type": "text/xml; charset=utf-8"}`)

  * Node.js APM agent
    - [releases](https://www.elastic.co/guide/en/apm/agent/nodejs/index.html)
    - [documentation](https://www.elastic.co/guide/en/apm/agent/nodejs/current/intro.html)

  * Misc.
    + [monitoring python flask application with elastic apm](https://medium.com/analytics-vidhya/monitoring-python-flask-application-with-elastic-apm-bb0853f056ff)
    + [get application performance metrics on python flask witheElastic APM on kibana and elasticsearch](https://ruanbekker.medium.com/get-application-performance-metrics-on-python-flask-with-elastic-apm-on-kibana-and-elasticsearch-2859ea02ae30)
    + [setup APM Server on Ubuntu](https://blog.ruanbekker.com/blog/2018/11/11/setup-apm-server-on-ubuntu-for-your-elastic-stack-to-get-insights-in-your-application-performance-metrics)
    + [finding local IP addresses using Python's stdlib](https://stackoverflow.com/questions/166506/finding-local-ip-addresses-using-pythons-stdlib)

    + docker-compose `extends` syntax [documetation](https://docs.docker.com/compose/extends/) - share Compose configurations between files and projects

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
