### Info

Is is possible to configure the node exporter report metric from another host ?

The [Sysdig Promscrape Monitor document](https://docs.sysdig.com/en/docs/sysdig-monitor/integrations-for-sysdig-monitor/collect-prometheus-metrics/collecting-prometheus-metrics-from-remote-hosts/)
answers affirmatively

### See also 

  * `node_exporter` [README](https://github.com/prometheus/node_exporter/blob/master/README.md)
  * __Textfile Collector__ section in the `node-exporter` Docker image [documentation](https://hub.docker.com/r/prom/node-exporter)
  * [Using the textfile collector from a shell script](https://www.robustperception.io/using-the-textfile-collector-from-a-shell-script#more-4014)
  * [tagging  textfile metrics with machine roles](https://www.robustperception.io/how-to-have-labels-for-machine-roles#more-1028)
  * [community contributed textfile collector generator scripts](https://github.com/lyda/node_exporter/blob/textfile-contrib/textfile_scripts)
  * promql query [examples](https://www.devopsschool.com/blog/prometheus-promql-example-query-node-exporter/)
  * [another doc](https://githubhelp.com/prometheus/node_exporter)(redundant)
  * [documentation](https://prometheus.io/docs/tutorials/understanding_metric_types/) on metric types
  * https://github.com/phuslu/remote_node_exporter  
  * [open bug](https://github.com/prometheus/node_exporter/issues/1885) of metric collision (same metric name, differenct labels) in  node_exporter
