### Info

clone on [Elastic APM-Server Lab](https://github.com/SMin1620/Elastic_APM_Lab) ELK applications cluster for APM learning  testing (aftera failed to vanilla install of `apm-server` on a a custom Vagrant box with other components installed)

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-cluster.png)

### Usage

* pull images
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
yellow open   apm-6.2.4-2022.11.24 2xcjw25ZTJ-_iaj6vbAoAA   5   1          1            0      5.5kb          5.5kb
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
  "build_date": "2019-05-15T23:36:10Z",
  "build_sha": "75bf6e0a4a71e14c7c84cdfe7e4bce73e4afacc4",
  "version": "7.1.0"
}
```
```text
"You Know, for Search"
```
```text

"green"
```

The APM Server management via Kibana is not fully functional in this old version of ELK: `http://$(hostname -i):5601/app/apm` clicking __Check APM Server Status__ button shows the message:
```text
No APM Server detected
```
![Kibana APM Example](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-kibana-apm-detection.png)

One can proceed with a hello world application example on `app` node
NOTE:

if one can not connect to `apm-server`
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

![Kibana APM Example](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-kibana-apm-server-correctly-setup.png)
* interact with `app` server
```sh
curl -s http://192.168.0.64:6000
```
![APM Example](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-example.png)

### Configuration

By default, the stack exposes the following ports:

* 5000: Logstash TCP input.
* 9200: Elasticsearch HTTP
* 9300: Elasticsearch TCP transport
* 5601: Kibana
* 8200: APM

NOTE: the images are relatively heavy

```text
basic-elk-cluster_apm_server           latest                3abe88832b9e   18 hours ago    756MB
basic-elk-cluster_logstash             latest                93ae8cd11560   3 years ago     847MB
basic-elk-cluster_kibana               latest                714b175e84e8   3 years ago     745MB
basic-elk-cluster_elasticsearch        latest                12ad640a1ec0   3 years ago     894MB
```
### TODO

* remove the `logstash` node from the cluster (not needed for APM exercise)

### See Also

  * [monitoring python flask application with elastic apm](https://medium.com/analytics-vidhya/monitoring-python-flask-application-with-elastic-apm-bb0853f056ff)

  * [get application performance metrics on python flask witheElastic APM on kibana and elasticsearch](https://ruanbekker.medium.com/get-application-performance-metrics-on-python-flask-with-elastic-apm-on-kibana-and-elasticsearch-2859ea02ae30)

  * [setup APM Server on Ubuntu for Your Elastic Stack to Get Insights in Your Application Performance Metrics]( https://blog.ruanbekker.com/blog/2018/11/11/setup-apm-server-on-ubuntu-for-your-elastic-stack-to-get-insights-in-your-application-performance-metrics)

  * [finding local IP addresses using Python's stdlib](https://stackoverflow.com/questions/166506/finding-local-ip-addresses-using-pythons-stdlib)

	
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
