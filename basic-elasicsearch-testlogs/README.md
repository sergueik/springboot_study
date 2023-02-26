#### Info

This directory contains a replica of [Demo client test logging project](https://github.com/dadoonet/elasticsearch-integration-tests) 

indexing the test logs on ElasticSearch 7.7.1 [Virtual Box](https://app.vagrantup.com/demonium/boxes/u18-elk/versions/20191110/providers/virtualbox.box) via [_ElasticSearch High Level Client](https://www.elastic.co/guide/en/elasticsearch/client/java-rest/master/java-rest-high.html)

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
Alternatively set in
```sh
src/test/resources/application.properties
```
(note this app does not scan `src/main/resources` location by design
see the result in Kibana

this will log to console:
```sh
03:08:51,676 INFO  [e.i.ElasticsearchTest] -> Document indexed with _id MIU-SHgByFO0a3_SFcwt.
```
if you also see the exception,
```sh
03:08:51,907 INFO  [e.i.ElasticsearchTest] Exception(ignored) ElasticsearchStatusException[Elasticsearch exception [type=illegal_argument_exception, reason=request [/scenario1/_search] contains unrecognized parameter: [ccs_minimize_roundtrips]]]
```
it is a result of back porting of API across incompatible revisions of [ElasticSearch RESTFul Client](https://github.com/elastic/elasticsearch) which was not the focus of this exercise and can be ignored.


* see the result in Kibana (NOTE: the screen shot taken when working with __ELK 6.6.2__)

![Example](https://github.com/sergueik/springboot_study/blob/master/basic-elasicsearch-testlogs/screenshots/capture.png)

* change password
```sh
pushd /usr/share/elasticsearch/bin
./elasticsearch-setup-passwords interactive
```
* on vanilla box, you will get the error
```sh
Unexpected response code [500] from calling GET http://10.0.2.15:9200/_security/_authenticate?pretty
It doesn't look like the X-Pack security feature is enabled on this Elasticsearch node.
Please check if you have enabled X-Pack security in your elasticsearch.yml configuration file.

ERROR: X-Pack Security is disabled by configuration.
```
* proceed with adding 
```sh
xpack.security.enabled: true
discovery.type: single-node
```
to `/etc/elasticsearch/elasticsearch.yml`. Then repeat command
```sh
pushd /usr/share/elasticsearch/bin
./elasticsearch-setup-passwords interactive
```
```sh
Enter password for [elastic]:
Reenter password for [elastic]:
Enter password for [apm_system]:
Reenter password for [apm_system]:
Enter password for [kibana]:
Reenter password for [kibana]:
Enter password for [logstash_system]:
Reenter password for [logstash_system]:
Enter password for [beats_system]:
Reenter password for [beats_system]:
Enter password for [remote_monitoring_user]:
Reenter password for [remote_monitoring_user]:
Changed password for user [apm_system]
Changed password for user [kibana]
Changed password for user [logstash_system]
Changed password for user [beats_system]
Changed password for user [remote_monitoring_user]
Changed password for user [elastic]
```

NOTE: can only do it once. Repeated runs will show the following
```sh
./elasticsearch-setup-passwords interactive
```
```sh
Possible causes include:
 * The password for the 'elastic' user has already been changed on this cluster
 * Your elasticsearch node is running against a different keystore
   This tool used the keystore at /etc/elasticsearch/elasticsearch.keystore

ERROR: Failed to verify bootstrap password
```

The attempt to run with invalid credentials (defined in `application.properties`will result in 
```sh
org.elasticsearch.client.ResponseException: method [GET], host [http://192.168.0.25:9200], URI [/], status line [HTTP/1.1 401 Unauthorized]

{
  "error": {
    "root_cause": [
      {
        "type": "security_exception",
        "reason": "failed to authenticate user [elastic]",
        "header": {
          "WWW-Authenticate": "Basic realm=\"security\" charset=\"UTF-8\""
        }
      }
    ],
    "type": "security_exception",
    "reason": "failed to authenticate user [elastic]",
    "header": {
      "WWW-Authenticate": "Basic realm=\"security\" charset=\"UTF-8\""
    }
  },
  "status": 401
}
```
after restoring the proper credentials, the error goes away
NOTE: `try...catch` around the code throwing this exception will not catch it.
### See Also

* [documentation](https://www.elastic.co/guide/en/elasticsearch/client/java-rest/master/java-rest-high.html)
* [REST Client](https://github.com/gentics/elasticsearch-java-client) not using any ElasticSeruch assemblies
* springboot __1.5.6__ elasticsearch __5.6.3__ [client](https://github.com/altfatterz/elasticsearch-java-client-demo/blob/master/pom.xml)
* https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-passwords.html

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

