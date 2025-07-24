### Info

Custom json exporter `file_sd_configs` to pass the querystring (timestamp in this example).

### Testing
```sh
docker pull prom/prometheus:v3.5.0
```
```sh
docker-compose up --build -d
```
```sh
docker-machine ip
```

```text
192.168.99.100
```
* update the `MACHINE_IP` in the following command
```sh
MACHINE_IP=192.168.99.100
```
```sh
 curl -s http://${MACHINE_IP}:9090/api/v1/targets | jq '.'
```
```json
{
  "status": "success",
  "data": {
    "activeTargets": [
      {
        "discoveredLabels": {
          "__address__": "app:80",
          "__meta_filepath": "/etc/prometheus/dynamic_targets.json",
          "__metrics_path__": "/probe",
          "__scheme__": "http",
          "__scrape_interval__": "15s",
          "__scrape_timeout__": "10s",
          "job": "json_exporter",
          "module": "stub",
          "target": "http://app:80/data?ts=1753369108"
        },
        "labels": {
          "instance": "http://app:80/data",
          "job": "json_exporter",
          "module": "stub",
          "source_file": "/etc/prometheus/dynamic_targets.json",
          "target": "http://app:80/data"
        },
        "scrapePool": "json_exporter",
        "scrapeUrl": "http://app:80/probe?target=http%3A%2F%2Fapp%3A80%2Fdata%3Fts%3D1753369108",
        "globalUrl": "http://app:80/probe?target=http%3A%2F%2Fapp%3A80%2Fdata%3Fts%3D1753369108",
        "lastError": "server returned HTTP status 404 Not Found",
        "lastScrape": "2025-07-24T14:58:32.882075254Z",
        "lastScrapeDuration": 0.017043394,
        "health": "down",
        "scrapeInterval": "15s",
        "scrapeTimeout": "10s"
      }
    ],
    "droppedTargets": [],
    "droppedTargetCounts": {
      "json_exporter": 0
    }
  }
}

```
* try exporter . NOTE:  ignore `http://app:80` in `scrapeUrl`:
```sh
curl -s "http://$MACHINE_IP:7979/probe?module=stub&target=http%3A%2F%2Fapp%3A80%2Fdata%3Fts%3D1753284427"
```
```text
# HELP stub_metric_value stub_metric_value
# TYPE stub_metric_value untyped
stub_metric_value 42
```

![working](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-file_sd_config-cluster/screenshots/prometeus-working.png)

### Configuring Grafana

![grafana](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-file_sd_config-cluster/screenshots/grafana.png)

* use data source `stub_metric_value`

By default Grafana shows data from json exporter with varying time stamp as a series of isolated dots.
To change rendering need to modify Panel Visualization  

### Cleanup
```sh
$ docker-compose stop
```
```text
[+] Running 0/1
[+] Running 0/1sic-prometheus-file_sd_config-cluster-prometheus-1  Stopping 10.0
[+] Running 2/3sic-prometheus-file_sd_config-cluster-prometheus-1  Stopping 10.1
[+] Running 2/3sic-prometheus-file_sd_config-cluster-prometheus-1  Stopped 10.1s
[+] Running 3/3sic-prometheus-file_sd_config-cluster-prometheus-1  Stopped 10.1s
 - Container basic-prometheus-file_sd_config-cluster-prometheus-1  Stopped 10.1s
 - Container basic-prometheus-file_sd_config-cluster-exporter-1    Stopped 0.1s1
 - Container basic-prometheus-file_sd_config-cluster-app-1         Stopped 10.1s
```

```sh
docker-compose rm -f
```
```text
[+] Running 3/0
 - Container basic-prometheus-file_sd_config-cluster-app-1         Removed 0.0s
 - Container basic-prometheus-file_sd_config-cluster-prometheus-1  Removed 0.0s
 - Container basic-prometheus-file_sd_config-cluster-exporter-1    Removed 0.0s
```
```sh
docker images --format "{{.Repository}}: {{.ID}}" | grep basic-prometheus-file_sd_config-cluster
```
```text
basic-prometheus-file_sd_config-cluster-prometheus: ada8ab456d6f
basic-prometheus-file_sd_config-cluster-exporter: 2ed84e3dfe21
basic-prometheus-file_sd_config-cluster-app: 3064af2d463c
```
```sh
 docker images --format "{{.Repository}}: {{.ID}}" | grep basic-prometheus-file_sd_config-cluster |awk '{print $2}' | xargs -IX  docker image rm X
```
### Failing  Configurations
Examine:

* `prometheus.log`	Scrape attempts and errors
```sh
docker-compose logs -f prometheus
```
* `exporter.log`	Probe requests and JSON errors:
```sh
docker-compose logs -f exporter
```
* `app` logs	Backend API request/responses
```sh
docker-compose logs -f app
```
```text
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 16:24:41.70158] [6] [trace] [Ezro4AAkn9wa] GET "/data"
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 16:24:41.70257] [6] [trace] [Ezro4AAkn9wa] Routing to a callback
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 16:24:41.70312] [6] [info] Received timestamp: 1753287871
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 16:24:41.70436] [6] [trace] [Ezro4AAkn9wa] 200 OK (0.002732s, 366.032/s)

```
* `/targets` page screenshot	Quick visual status check
* `/prometheus.yml`	Confirm scrape & relabel config
* `/etc/prometheus/dynamic_targets.json`	Confirm dynamic targets
* `/json_exporter_config.yml`	Confirm JSON metrics config

![not working](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-file_sd_config-cluster/screenshots/prometeus-broken.png)

when there is a misconfiguration in `prometheus.yaml` and / or `dynamic_targets.json` and /or `json_exporter_config.yml`, examine closely

```sh
docker-compose exec -it prometheus sh
```
in the container
```sh
wget -O - http://localhost:9090/api/v1/targets
```
this will print (there is no `jq` in `prom/prometheus:latest` image )
```text
Connecting to localhost:9090 (127.0.0.1:9090)
writing to stdout
{"status":"success","data":{"activeTargets":[{"discoveredLabels":{"__address__":"http%3A%2F%2Fapp%3A80%2Fdata%3Fts%3D1753275245","__meta_filepath":"/etc/prometheus/dynamic_targets.json","__metrics_path__":"/probe","__param_module":"stub","__scheme__":"http","__scrape_interval__":"1m","__scrape_timeout__":"10s","job":"json_exporter","module":"stub"},"labels":{"instance":"exporter:7979","job":"json_exporter","module":"stub"},"scrapePool":"json_exporter","scrapeUrl":"http://exporter:7979/probe?module=stub","globalUrl":"http://exporter:7979/probe?module=stub","lastError":"server returned HTTP status 400 Bad Request","lastScrape":"2025-07-23T12:53:14.449649828Z","lastScrapeDuration":0.000885487,"health":"down","scrapeInterval":"1m","scrapeTim-                    100% |***********************************|   824  0:00:00 ETA
written to stdout
```
```sh
 wget -O - http://localhost:9090/api/v1/targets
```
```text
Connecting to localhost:9090 (127.0.0.1:9090)
writing to stdout
{"status":"success","data":{"activeTargets":[{"discoveredLabels":{"__address__":"http%3A%2F%2Fapp%3A80%2Fdata%3Fts%3D1753275895","__meta_filepath":"/etc/prometheus/dynamic_targets.json","__metrics_path__":"/probe","__param_module":"stub","__scheme__":"http","__scrape_interval__":"1m","__scrape_timeout__":"10s","job":"json_exporter","module":"stub"},"labels":{"instance":"exporter:7979","job":"json_exporter","module":"stub"},"scrapePool":"json_exporter","scrapeUrl":"http://exporter:7979/probe?module=stub","globalUrl":"http://exporter:7979/probe?module=stub","lastError":"server returned HTTP status 400 Bad Request","lastScrape":"2025-07-23T13:04:48.66179468Z","lastScrapeDuration":0.003846283,"health":"down","scrapeInterval":"1m","scrapeTimeout":"10s"}],"droppedTargets":[],"droppedTargetCounts-                    100% |********************************|   823  0:00:00 ETA
written to stdout

```

### See Also
   * https://prometheus.io/docs/instrumenting/exporters/
   * https://github.com/prometheus-community/json_exporter/issues/393
   * [Prometheus relabel_configs](https://prometheus.io/docs/prometheus/latest/configuration/configuration/#relabel_config)
   * [Prometheus file_sd_configs](https://prometheus.io/docs/prometheus/latest/configuration/configuration/#file_sd_config)
   * [Relabeling Examples](https://prometheus.io/docs/prometheus/latest/configuration/relabeling/)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
