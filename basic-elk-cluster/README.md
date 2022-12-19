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
curl -XGET 'localhost:9200/apm-7.17.7-transaction-000001/_search?_source=false'
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
and in __Dev Tools__
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

  * [log Correlation](https://www.elastic.co/guide/en/apm/agent/python/current/log-correlation.html)

  * Java __Spring-WS__-based  library [repository](https://github.com/reficio/soap-ws),  that enables handling SOAP on a purely XML level and with SOAP standards supports version 1.1 and 1.2 - the `SOAPAction` attribute is automatically [properly placed](https://github.com/reficio/soap-ws/search?p=2&q=soapaction)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
