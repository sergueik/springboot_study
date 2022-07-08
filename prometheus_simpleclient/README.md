### Info

* simpleclient and common code extracted from 
[prometheus/client_java](https://github.com/prometheus/client_java) to experiment with custom timestamp at the adapter level.
#### Pull Details

replica of `io.prometheus.simpleclient` from [prometheus/client_java](https://github.com/prometheus/client_java) at tag `parent-0.15.0` with sibling dependencies replaced with release jars. converted to standalone jar, and commented one failing unit test 
Originally intent to explore the possibility to add caller timestamp setter for `Counter`

### Background

The folllowing code exercising cutom metric collector feauring timestamps was found in [prometheus/client_java](https://github.com/prometheus/client_java)
```java
@Test
public void testMetricOutputWithTimestamp() throws IOException {
  //contrtucts custom metric collector, which uses the timestamp argument in Sample constructor
  class CustomCollector extends Collector {
    public List<MetricFamilySamples> collect() {
      List<MetricFamilySamples> mfs = new ArrayList<MetricFamilySamples>();
      ArrayList<String> labelNames = new ArrayList<String>();
      ArrayList<String> labelValues = new ArrayList<String>();
      ArrayList<MetricFamilySamples.Sample> samples = new ArrayList<Collector.MetricFamilySamples.Sample>();
      MetricFamilySamples.Sample sample = new MetricFamilySamples.Sample(
          "nolabels", labelNames, labelValues, 1.0, 1518123456L);
      samples.add(sample);
      mfs.add(new MetricFamilySamples("nolabels", Collector.Type.UNKNOWN,
          "help", samples));
      return mfs;
    }
  }

  new CustomCollector().register(registry);
  TextFormat.write004(writer, registry.metricFamilySamples());
  assertEquals("# HELP nolabels help\n" + "# TYPE nolabels untyped\n"
      + "nolabels 1.0 1518123456\n", writer.toString());
}

```

this is the only [example](https://github.com/prometheus/client_java/blob/master/simpleclient_common/src/test/java/io/prometheus/client/exporter/common/TextFormatTest.java#L114) 
of formatting the timestamp from the metric sender. 
It does not appear to be supported much, though code is still present.
It was added in the *Added optional "timestamp" value as part of metric sample*
[PR](https://github.com/prometheus/client_java/commit/5b0a2752bcb8a168b69a74fd6c230d6a7d74f9c1#diff-b5b368e079b04f16ffb1a62caacf0001aa05595b8f9a2ec999003336b3bf0ca5)
and includes changes to `Collector.java` and `TextFormat.java`
it is uncertain that specifying the timestamp at the time of creating a metrinc sample is supported in the code:  the `labels(String... labelValues)` method is not allowing that.
The code pattern used earlier, does not seem to fit. One can set timestamps *after* consructing metric samlpes,
using the code added in the referenced PR,
by introducing a method override with an `timestamp` argument
```java
write004(Writer writer, Enumeration<Collector.MetricFamilySamples> metricFamilySamples, long timestamp)
```
such solution will not be ideal, since it will require batching the metric ingestion by the oriinating server, for timestamp to be the same - but it will work.

### See Also
   * [self-hosted Monitoring For Spring Boot Applications](https://www.baeldung.com/spring-boot-self-hosted-monitoring)
   * [quick guide to Micrometer](https://www.baeldung.com/micrometer)
   * [simple way of using Micrometer, Prometheus and Grafana (Spring Boot 2)](https://www.north-47.com/knowledge-base/a-simple-way-of-using-micrometer-prometheus-and-grafana-spring-boot-2/)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
