### Usage
```sh
pushd app
mvn spring-boot:run
```

```
for CNT in $(seq 1 1 5) ;  do curl -s http://localhost:8080/analyze > /dev/null; done
```
Note metrics (`http://localhost:8080/actuator/prometheus`):
```text
# HELP file_rows_processed_total Rows processed by status
# TYPE file_rows_processed_total counter
file_rows_processed_total{application="${spring.application.name}",status="failing",} 12.0
file_rows_processed_total{application="${spring.application.name}",status="problematic",} 6.0
file_rows_processed_total{application="${spring.application.name}",status="perfect",} 6.0

```
### Docker
```cmd
pushd package
mvn package
```
```
docker-compose up --build -d
```
```sh
for CNT in $(seq 1 1 5) ;  do curl -s http://192.168.99.100:8080/analyze > /dev/null; done

```

`https://192.168.99.100:9090`

![prometheus](screenshots/counter-prometheus.png)


![grafana](screenshots/grafana1.png)

![grafana](screenshots/grafana2.png)

![grafana](screenshots/grafana3.png)

### Info
In Grafana you can build:
A. Pie chart (distribution)

Query:

sum by (status) (file_rows_processed_total)

👉 shows:

perfect vs problematic vs failing
B. Rate over time (pipeline health)
rate(file_rows_processed_total[1m])

👉 shows how fast issues appear

C. Error ratio (very powerful)
sum(rate(file_rows_processed_total{status="failing"}[5m]))
/
sum(rate(file_rows_processed_total[5m]))

👉 % of failing rows

D. Stacked time series
sum by (status) (rate(file_rows_processed_total[1m]))

👉 shows trend per category

