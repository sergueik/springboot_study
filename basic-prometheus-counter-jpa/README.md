### Info


this directory contains a JPA based version of [Java Node Exporter proxy for Prometheus](https://github.com/sergueik/springboot_study/tree/master/basic-prometheus-counter)
### Note

Currently if the JPA /SQL returns a null value for one of the labels, e.g. the `app`, the  Prometheus metric is created with a blank value:
```text
rpm{instance="hostname05",dc="dummy",app="",env="instance01",} 100.0
```

the code adding of Gauges with same name but varying number of labels array is not available yet.
### REGEXT syntax extension

```sh
sudo apt-get install sqlite3-pcre
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

