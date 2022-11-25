### Info

clone on [Elastic APM-Server Lab](https://github.com/SMin1620/Elastic_APM_Lab) ELK applications cluster for APM learning  testing (aftera failed to vanilla install of `apm-server` on a a custom Vagrant box with other components installed)


### Usage

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
The APM Server management via Kibana is not available in this old version of ELK
```sh
curl http://192.168.0.92:5601/app/apm
```

* add python3 on `apm-server` will be easy, it is an centos base image, but does not have sudo, therefore yum commands need to be added to `apm/Dockerfile`
```
USER root
RUN yum install -q -y python3 pip3
USER apm-server
```
and map port `6000` of `apm-server` to host `6000` (the port `5000` is already mapped to logstash container `5000`:

```yaml

```
```sh
docker-compose exec apm_server sh
```
```sh
pip3 install --user flask elastic-apm[flask]
```
```
hostname -i
vi /tmp/app.py
```
```python
from flask import Flask
app = Flask(__name__)
@app.route('/')
def index():
  return "Hello World!"
if __name__ == '__main__':
  app.run(host='172.20.0.5', port=6000)
```
can add APM code right away
```python
from flask import Flask
from elasticapm.contrib.flask import ElasticAPM
app = Flask(__name__)
app.config['ELASTIC_APM'] = {
          'SERVICE_NAME': 'FlaskApp',
          'SECRET_TOKEN': '',
          'SERVER_URL': 'http://localhost:8200'
}
apm = ElasticAPM(app)
@app.route('/')
def index():
  return "Hello World!"
if __name__ == '__main__':
  app.run(host='172.20.0.5', port=6000)

```
```sh
python3 /tmp/app.py
```
```sh
curl -s http://192.168.0.92:6000
```
currently it fails to submit index:
```text
Failed to submit message: 'HTTP 404: 404 page not found\n'
Traceback (most recent call last):
  File "/usr/local/lib/python3.6/site-packages/elasticapm/transport/base.py", line 279, in _flush
    self.send(data, forced_flush=forced_flush)
  File "/usr/local/lib/python3.6/site-packages/elasticapm/transport/http.py", line 114, in send
    raise TransportException(message, data, print_trace=print_trace)
elasticapm.transport.exceptions.TransportException: HTTP 404: 404 page not found
```
### Configuration
By default, the stack exposes the following ports:
* 5000: Logstash TCP input.
* 9200: Elasticsearch HTTP
* 9300: Elasticsearch TCP transport
* 5601: Kibana
* 8200: APM
The images are relatively heavy

```text
basic-elk-cluster_apm_server    latest                6fb06e8a9da3   4 years ago     319MB
basic-elk-cluster_logstash      latest                5d448d68a8ee   4 years ago     669MB
basic-elk-cluster_kibana        latest                23752503a930   4 years ago     547MB
basic-elk-cluster_elasticsearch latest                458eedf9515f   4 years ago     451MB

```
### TODO

remove `logstash` from the cluster (not needed for APM exercise)

### See Also

  * [monitoring python flask application with elastic apm](https://medium.com/analytics-vidhya/monitoring-python-flask-application-with-elastic-apm-bb0853f056ff)

  * [get application performance metrics on python flask witheElastic APM on kibana and elasticsearch](https://ruanbekker.medium.com/get-application-performance-metrics-on-python-flask-with-elastic-apm-on-kibana-and-elasticsearch-2859ea02ae30)

  * [setup APM Server on Ubuntu for Your Elastic Stack to Get Insights in Your Application Performance Metrics]( https://blog.ruanbekker.com/blog/2018/11/11/setup-apm-server-on-ubuntu-for-your-elastic-stack-to-get-insights-in-your-application-performance-metrics)




