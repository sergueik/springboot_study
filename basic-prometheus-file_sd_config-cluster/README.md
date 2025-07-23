### Info

* `prometheus.log`	Scrape attempts and errors
```text
```

* `exporter.log`	Probe requests and JSON errors:
```text
basic-prometheus-file_sd_config-cluster-exporter-1  | time=2025-07-23T12:23:06.242Z level=INFO source=main.go:56 msg="Starting json_exporter" version="(version=0.7.0, branch=HEAD, revision=06fb506a4c5d242186f198b9c8bf072212f3a134)"
basic-prometheus-file_sd_config-cluster-exporter-1  | time=2025-07-23T12:23:06.244Z level=INFO source=main.go:57 msg="Build context" build="(go=go1.23.6, platform=linux/amd64, user=root@6a86be1e0b11, date=20250205-13:57:47, tags=unknown)"
basic-prometheus-file_sd_config-cluster-exporter-1  | time=2025-07-23T12:23:06.244Z level=INFO source=main.go:59 msg="Loading config file" file=/json_exporter_config.yml
basic-prometheus-file_sd_config-cluster-exporter-1  | time=2025-07-23T12:23:06.245Z level=INFO source=main.go:69 msg="Loaded config file" config="{\"Modules\":{\"stub\":{\"Headers\":{\"Accept\":\"application/json\"},\"Metrics\":[{\"Name\":\"stub_metric_value\",\"Path\":\"{.metric_value}\",\"Labels\":null,\"Type\":\"value\",\"ValueType\":\"untyped\",\"EpochTimestamp\":\"\",\"Help\":\"stub_metric_value\",\"Values\":null}],\"HTTPClientConfig\":{\"tls_config\":{\"insecure_skip_verify\":false},\"follow_redirects\":false,\"enable_http2\":false,\"proxy_url\":null},\"Body\":{\"Content\":\"\",\"Templatize\":false},\"ValidStatusCodes\":null}}}"
basic-prometheus-file_sd_config-cluster-exporter-1  | time=2025-07-23T12:23:06.252Z level=INFO source=tls_config.go:347 msg="Listening on" address=[::]:7979
basic-prometheus-file_sd_config-cluster-exporter-1  | time=2025-07-23T12:23:06.255Z level=INFO source=tls_config.go:350 msg="TLS is disabled." http2=false address=[::]:7979


```
* `app` logs	Backend API request/responses
```text
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:20:35.54416] [6] [info] Received timestamp:  data missing
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:20:35.54462] [6] [trace] [Pz0BFUj9tlNQ] 400 Bad Request (0.001465s, 682.594/s)
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:20:54.51329] [6] [trace] [8OUgnK77dMuG] GET "/data"
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:20:54.51387] [6] [trace] [8OUgnK77dMuG] Routing to a callback
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:20:54.51445] [6] [info] Received timestamp: 1234567890
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:20:54.51495] [6] [trace] [8OUgnK77dMuG] 200 OK (0.001647s, 607.165/s)
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:21:03.16321] [6] [trace] [tLM93WP3aKtF] GET "/data"
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:21:03.16376] [6] [trace] [tLM93WP3aKtF] Routing to a callback
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:21:03.16421] [6] [info] Received timestamp: 12345678901
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:21:03.16457] [6] [trace] [tLM93WP3aKtF] 405 Method Not Allowed (0.001349s, 741.290/s)
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:21:09.99592] [6] [trace] [pSwtrdG167-O] GET "/data"
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:21:09.99648] [6] [trace] [pSwtrdG167-O] Routing to a callback
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:21:09.99699] [6] [info] Received timestamp:  data missing
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 06:21:09.99735] [6] [trace] [pSwtrdG167-O] 400 Bad Request (0.001415s, 706.714/s)
basic-prometheus-file_sd_config-cluster-app-1  | started perl myapp.pl daemon -l "http://*:80" &
basic-prometheus-file_sd_config-cluster-app-1  | app is running with ID 6
basic-prometheus-file_sd_config-cluster-app-1  | [2025-07-23 11:18:15.65740] [6] [info] Listening at "http://*:80"

```
* `/targets` page screenshot	Quick visual status check
* `prometheus.yml`	Confirm scrape & relabel config
```yaml
scrape_configs:
  - job_name: 'json_exporter'
    metrics_path: /probe
    params:
      module: [stub]
    file_sd_configs:
      - files:
          - /etc/prometheus/dynamic_targets.json
        refresh_interval: 30s
    relabel_configs:
      - source_labels: [target]
        target_label: __param_target
      - source_labels: [__param_target]
        target_label: instance
      - source_labels: [module]
        target_label: __param_module
      - target_label: __address__
        replacement: exporter:7979

```
* `dynamic_targets.json`	Confirm dynamic targets
```json
[
  {
    "targets": ["http://app:80/data?ts=1753272175"],
    "labels": {
      "module": "stub"
    }
  }
]

```
* `json_exporter_config.yml`	Confirm JSON metrics config
```yaml
---
modules:
  stub:
    method: GET
    headers:
      Accept: application/json
    metrics:
      - name: stub_metric_value
        path: '{.metric_value}'


```
```sh
wget -O - http://localhost:9090/api/v1/targets
```
```text
Connecting to localhost:9090 (127.0.0.1:9090)
writing to stdout
{"status":"success","data":{"activeTargets":[{"discoveredLabels":{"__address__":"http%3A%2F%2Fapp%3A80%2Fdata%3Fts%3D1753275245","__meta_filepath":"/etc/prometheus/dynamic_targets.json","__metrics_path__":"/probe","__param_module":"stub","__scheme__":"http","__scrape_interval__":"1m","__scrape_timeout__":"10s","job":"json_exporter","module":"stub"},"labels":{"instance":"exporter:7979","job":"json_exporter","module":"stub"},"scrapePool":"json_exporter","scrapeUrl":"http://exporter:7979/probe?module=stub","globalUrl":"http://exporter:7979/probe?module=stub","lastError":"server returned HTTP status 400 Bad Request","lastScrape":"2025-07-23T12:53:14.449649828Z","lastScrapeDuration":0.000885487,"health":"down","scrapeInterval":"1m","scrapeTim-                    100% |***********************************|   824  0:00:00 ETA
written to stdout
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
