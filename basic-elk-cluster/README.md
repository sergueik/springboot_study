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

and details in "transactions":

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-app-transactions.png)

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

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-two-services2.png)

### See Also

  * [monitoring python flask application with elastic apm](https://medium.com/analytics-vidhya/monitoring-python-flask-application-with-elastic-apm-bb0853f056ff)

  * [get application performance metrics on python flask witheElastic APM on kibana and elasticsearch](https://ruanbekker.medium.com/get-application-performance-metrics-on-python-flask-with-elastic-apm-on-kibana-and-elasticsearch-2859ea02ae30)

  * [setup APM Server on Ubuntu for Your Elastic Stack to Get Insights in Your Application Performance Metrics]( https://blog.ruanbekker.com/blog/2018/11/11/setup-apm-server-on-ubuntu-for-your-elastic-stack-to-get-insights-in-your-application-performance-metrics)

  * [finding local IP addresses using Python's stdlib](https://stackoverflow.com/questions/166506/finding-local-ip-addresses-using-pythons-stdlib)

	
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
