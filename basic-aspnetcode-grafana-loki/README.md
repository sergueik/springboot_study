### Info

refactored replica of [WesleySkeen/Grafana-Data-Sources](https://github.com/WesleySkeen/Grafana-Data-Sources)


### Usage
```sh
docker pull mcr.microsoft.com/dotnet/sdk:6.0
docker pull mcr.microsoft.com/dotnet/sdk:6.0
docker pull prom/prometheus:v2.41.0
docker pull grafana/promtail:2.5.0
docker pull grafana/loki:2.5.0
docker pull grafana/grafana:10.4.13
docker pull openzipkin/zipkin
docker pull jaegertracing/all-in-one:latest
docker pull jaegertracing/example-hotrod:latest
```

```
pushd app
docker build --pull --no-cache -t weather_forecast_api -f Dockerfile .
```
```sh
docker-compose -f docker-compose.yml up -d --build
```

```shell
curl http://localhost:4000/metrics
```
```text
# TYPE process_runtime_dotnet_gc_collections_count counter
# HELP process_runtime_dotnet_gc_collections_count Number of garbage collections that have occurred since process start.
process_runtime_dotnet_gc_collections_count{generation="gen2"} 0 1776624150749
process_runtime_dotnet_gc_collections_count{generation="gen1"} 0 1776624150749
process_runtime_dotnet_gc_collections_count{generation="gen0"} 0 1776624150749
...
# TYPE process_runtime_dotnet_assemblies_count gauge
# HELP process_runtime_dotnet_assemblies_count The number of .NET assemblies that are currently loaded.
process_runtime_dotnet_assemblies_count 147 1776624150749

# TYPE process_runtime_dotnet_exceptions_count counter
# HELP process_runtime_dotnet_exceptions_count Count of exceptions that have been thrown in managed code, since the observation started. The value will be unavailable until an exception has been thrown after OpenTelemetry.Instrumentation.Runtime initialization.
process_runtime_dotnet_exceptions_count 1 1776624150749

# EOF
```

To confirm promethius can access the API metric scraping endpoint, browse to Browse to the `http://localhost:9090/targets` You should see 

![promethius targets](screenshots/promethius_targets.png)


### Note

```text
ERROR: Named volume "prometheus/prometheus.yml:/etc/prometheus/prometheus.yml:ro" is used in service "prometheus" but no declaration was found in the volumes section
```
* heavy volume usage leads to some nodes exiting
```sh
docker-compose logs grafana
```
```text
mkdir: can't create directory '/var/lib/grafana/plugins': Permission denied
grafana_1               | GF_PATHS_DATA='/var/lib/grafana' is not writable.
```

```sh
docker-compose logs loki
```
```text
loki_1                  | failed parsing config: read /etc/loki/local-config.yml: is a directory
```

```sh
curl http://127.0.0.1:3100/ready
```
```text
Ingester not ready: waiting for 15s after being ready
```
retry in half a minute
```sh
curl http://127.0.0.1:3100/ready
```
```text
ready
```

```sh
docker-compose logs grafana | tail -5
```
```text
grafana_1               | logger=settings t=2026-04-19T21:00:37.856015862Z level=info msg="Path Data" path=/var/lib/grafana
grafana_1               | logger=settings t=2026-04-19T21:00:37.856039604Z level=info msg="Path Logs" path=/var/log/grafana
grafana_1               | logger=settings t=2026-04-19T21:00:37.856064512Z level=info msg="Path Plugins" path=/var/lib/grafana/plugins
grafana_1               | logger=settings t=2026-04-19T21:00:37.856122885Z level=info msg="Path Provisioning" path=/etc/grafana/provisioning
grafana_1               | logger=settings t=2026-04-19T21:00:37.85614787Z level=info msg="App mode production"
grafana_1               | logger=sqlstore t=2026-04-19T21:00:37.85760233Z level=info msg="Connecting to DB" dbtype=sqlite3
grafana_1               | logger=migrator t=2026-04-19T21:00:37.85828676Z level=info msg="Starting DB migrations"
grafana_1               | Error: ✗ failed to check table existence: unable to open database file: permission denied

```
```sh
docker run --entrypoint '' --rm grafana/grafana:10.4.13 id
```
```text
uid=472(grafana) gid=0(root) groups=0(root),0(root)
```

update the docker-compose.yml with UID:
```sh
export GRAFANA_UID=472
docker-compose stop
docker-compose rm -f
docker volume prune -f
docker-compose -f docker-compose.yml up -d --build
```

wait until `http://localhost:3100/ready` returns `ready`.

After seeing `Unable to connect with Loki. Please check the server logs for more details.`

examine the logs:
```text
grafana_1               | logger=tsdb.loki endpoint=checkHealth pluginId=loki dsName=loki dsUID=afjl580m3vt34c uname=admin fromAlert=false t=2026-04-19T21:55:59.723826075Z level=info msg="Prepared request to Loki" duration=60.106µs queriesLength=1 stage=prepareRequest runInParallel=false
grafana_1               | logger=tsdb.loki endpoint=checkHealth pluginId=loki dsName=loki dsUID=afjl580m3vt34c uname=admin fromAlert=false t=2026-04-19T21:55:59.726977291Z level=error msg="Error received from Loki" duration=2.906811ms stage=databaseRequest statusCode=400 contentLength=65 start=1970-01-01T00:00:01Z end=1970-01-01T00:00:04Z step=1s query=vector(1)+vector(1) queryType=instant direction=backward maxLines=0 supportingQueryType=none lokiHost=loki:3100 lokiPath=/loki/api/v1/query status=error error="parse error at line 1, col 1: syntax error: unexpected IDENTIFIER" statusSource=downstream
grafana_1               | logger=tsdb.loki endpoint=CheckHealth t=2026-04-19T21:55:59.72713Z level=error msg="Loki health check failed" error="error from loki: parse error at line 1, col 1: syntax error: unexpected IDENTIFIER"
grafana_1               | logger=context userId=1 orgId=1 uname=admin t=2026-04-19T21:55:59.727224874Z level=info msg="Request Completed" method=GET path=/api/datasources/uid/afjl580m3vt34c/health status=400 remote_addr=172.19.0.1 time_ms=5 duration=5.034318ms size=106 referer=http://localhost:3000/connections/datasources/edit/afjl580m3vt34c handler=/api/datasources/uid/:uid/health status_source=server
```

upgrade the `loki` and `promtail` to __2.9.6__ or later.
```sh
docke-compose down -v
docker-compose up -d
``` 

### Cleanup

```sh
docke-compose down -v
```
slightly more aggressive:

```
docke-compose stop 
docke-compose rm -f
docker container prune -f
docker volume prune -f
docker image rm weather_forecast_api:latest
docker image rm grafana/grafana:10.4.13 grafana/promtail:2.9.6 mcr.microsoft.com/dotnet/aspnet:6.0 mcr.microsoft.com/dotnet/sdk:6.0 prom/prometheus:v2.41.0
```
