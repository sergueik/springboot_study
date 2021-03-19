#### Info

This directory contains a replica of [Demo client test logging project](https://github.com/dadoonet/elasticsearch-integration-tests) tweaked from 7.7.1 to 6.6.2 to be able injest data to ELK 6.6.2 instandlaone Virtual Box [apolloclark/ubuntu16.04-elk](https://app.vagrantup.com/apolloclark/boxes/ubuntu16.04-elk/versions/20190426). Not 100% functionality works at Java level, but the indexed data is visible in Kibana aftert the run.

### Usage

* crate the index manually
```sh
INDEX=scenario1
curl -X PUT http://localhost:9200/$INDEX
```
* run test
```sh
mvn -Dtests.cluster.host=192.168.0.25 clean test
```
in the above command specify the ip address of the host the Virtual Box with ELK is running (assuming it is configured to user NAT). When not set, defaults to `hostname -i`

this will log to console:
```sh
03:08:51,676 INFO  [e.i.ElasticsearchTest] -> Document indexed with _id MIU-SHgByFO0a3_SFcwt.
```
if you also see the exception,
```sh
03:08:51,907 INFO  [e.i.ElasticsearchTest] Exception(ignored) ElasticsearchStatusException[Elasticsearch exception [type=illegal_argument_exception, reason=request [/scenario1/_search] contains unrecognized parameter: [ccs_minimize_roundtrips]]]
```
it is a result of back porting of API across incompatible revisions of [ElasticSearch RESTFul Client](https://github.com/elastic/elasticsearch) which was not the focus of this exercise and can be ignored.


* see the result in Kibana

![Example](https://github.com/sergueik/springboot_study/blob/master/basic-elasicsearch-testlogs/screenshots/capture.png)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

