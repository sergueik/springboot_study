### Info

replica of [Spring-boot-elasticsearch-timeseries-demo](https://github.com/wilsonyy/spring-boot-elasticsearch-timeseries-demo) with single instance ELK server
 the Elasticsearch is used to store time series database coupled with Grafana which is run on [Grafana packaged by Bitnami](https://bitnami.com/stack/grafana) VM (for sake of networking)
The original project focus was with shard creation.

Use [elasticsearch stack standalone Vagrantbox packaged by Bitnami](https://bitnami.com/stack/elk) as single ELK node (set number of replicas to zero)
This appliance is not configured to allow data posting to Elasticsearch from external hosts by default.


### Usage
* login to console and open port `3000`:
```sh
ufw allow 3000
```
* login to `http://192.168.0.140:3000/login` and configure via web UI using credentials provided by bitmani

```text
the default username and password is 'admin' and 'wNPHGcoP2RhS'
```
* import `bitnami-grafana-9.1.2-0-linux-vm-debian-11-x86_64-nami.ova`
* configure grafana elasticsearch plugin
Turns out the latest release Bitnami packaged Grafana __9.1.2__ refuses to work witn elasticsearch prior to __7.10__:
web console says: *Support for Elasticsearch versions after their end-of-life (currently versions < 7.10) was removed*
One can download rhe older release from `https://artifacthub.io/packages/helm/bitnami/grafana`

Download grafana Vagrant box from `https://app.vagrantup.com/Siarhei/boxes/grafana/versions/1.0`


```powershell
$ProgressPreference = 'SilentlyContinue'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
invoke-webrequest -uri 'https://app.vagrantup.com/Siarhei/boxes/grafana/versions/1.0/providers/virtualbox.box' -outfile "${env:USERPROFILE}\Downloads\grafana.box"
```
```sh
cd elasticsearch
vagrant up
```
* ignore the error
```text
There was an error while executing `VBoxManage`, a CLI used by Vagrant
for controlling VirtualBox. The command and stderr is shown below.

Command: ["startvm", "85ac7834-8be6-44bb-a9cb-f2b67d87595a", "--type", "headless"]

Stderr: VBoxManage.exe: error: RawFile#0 failed to create the raw output file C:/Users/Sergey_Bob/Documents/learn/prometeus+grafana/swarmprom/ubuntu-xenial-16.04-cloudimg-console.log (VERR_PATH_NOT_FOUND)
VBoxManage.exe: error: Details: code E_FAIL (0x80004005), component ConsoleWrap, interface IConsole
```
remove the serial port cofiguration 

and start the VM in Virtual Box manually

Turns out it does not contain grafana locally,but has a docker images  which may lead to networking challenges - discard
 
* try `https://app.vagrantup.com/novobi/boxes/labready-grafana/versions/1.0`

```powershell
$ProgressPreference = 'SilentlyContinue'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
invoke-webrequest -uri 'https://app.vagrantup.com/novobi/boxes/labready-grafana/versions/1.0/providers/virtualbox.box' -outfile "${env:USERPROFILE}\Downloads\grafana.box"
```
the VMappears to  have unreahcable bridge network card 2. Forward port 3000 on netrowk card 1 and connect to web ui as on `http://localhost:3000/login - it allows configuring the elasticsearch data source to `192.168.0.138` port `9200`. One ha to select the version `7.0+` of easticsearch backend in the configirtion screen, then it successfully tests the data source.


* download elasticsearch __7.6.2__ Vargant Box


```powershell
$ProgressPreference = 'SilentlyContinue'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

invoke-webrequest -uri 'https://app.vagrantup.com/stevesimpson/boxes/elasticsearch/versions/2.0.0/providers/virtualbox.box' -outfile "${env:USERPROFILE}\Downloads\elastic.box"
```
or 
```sh
curl -o ~/Downloads/elastic.box -Lk https://app.vagrantup.com/stevesimpson/boxes/elasticsearch/versions/2.0.0/providers/virtualbox.box
```
```sh
cd elasticsearch
vagrant up
```
NOTE: found that there is no elastic stact on [this](https://app.vagrantup.com/elastic/boxes/debian-8-x86_64-test/versions/20200209.0.0/providers/virtualbox.box) particular box despite the vendor.

```sh
vagrant destroy -f
vagrant box remove elastic/debian-8-x86_64-test
```
```sh
vagrant up
vagrant ssh
yum install net-tools


```

reconfigure elasticsearch to listen fro outside
```sh
curl 127.0.0.1:9200
```
```json
{
  "name" : "elasticsearch",
  "cluster_name" : "elasticsearch",
  "cluster_uuid" : "FtKAQ6YeQ_KYz9-AYGy2yg",
  "version" : {
    "number" : "7.3.1",
    "build_flavor" : "default",
    "build_type" : "rpm",
    "build_hash" : "4749ba6",
    "build_date" : "2019-08-19T20:19:25.651794Z",
    "build_snapshot" : false,
    "lucene_version" : "8.1.0",
    "minimum_wire_compatibility_version" : "6.8.0",
    "minimum_index_compatibility_version" : "6.0.0-beta1"
  },
  "tagline" : "You Know, for Search"
}

```
```sh
curl 192.168.0.138:9200
```
```text
curl: (7) Failed connect to 192.168.0.138:9200; Connection refused
```
```sh
vi /etc/elasticsearch/elasticsearch.yml
```
```text
# ---------------------------------- Network -----------------------------------
# Set the bind address to a specific IP (IPv4 or IPv6):
#

network.host: 0.0.0.0
```
```sh

systemctl stop elasticsearch
systemctl start elasticsearch

```
```
systemctl status elasticsearch
● elasticsearch.service - Elasticsearch
   Loaded: loaded (/usr/lib/systemd/system/elasticsearch.service; enabled; vendor preset: disabled)
   Active: failed (Result: exit-code) since Mon 2022-09-05 03:38:26 BST; 1min 1s ago
     Docs: http://www.elastic.co
  Process: 13428 ExecStart=/usr/share/elasticsearch/bin/elasticsearch -p ${PID_DIR}/elasticsearch.pid --quiet (code=exited, status=78)
 Main PID: 13428 (code=exited, status=78)

```
```sh
vi /var/log/elasticsearch/elasticsearch.log
```
```text
[elasticsearch] node validation exception
[1] bootstrap checks failed
[1]: the default discovery settings are unsuitable for production use; at least one of [discovery.seed_hosts, discovery.seed_providers, cluster.initial_master_nodes] must be configured
[2022-09-05T03:38:26,308][INFO ][o.e.n.Node               ] [elasticsearch] stopping ...
[2022-09-05T03:38:26,355][INFO ][o.e.n.Node               ] [elasticsearch] stopped
[2022-09-05T03:38:26,356][INFO ][o.e.n.Node               ] [elasticsearch] closing ...
[2022-09-05T03:38:26,391][INFO ][o.e.n.Node               ] [elasticsearch] closed

```
```sh
vi /etc/elasticsearch/elasticsearch.yml
```
```text	
# --------------------------------- Discovery ----------------------------------
#
# Pass an initial list of hosts to perform discovery when this node is started:
# The default list of hosts is ["127.0.0.1", "[::1]"]
#
discovery.seed_hosts: ["127.0.0.1", "192.168.0.138"]
```
```sh
 curl 192.168.0.138:9200
```
```json
{
  "name" : "elasticsearch",
  "cluster_name" : "elasticsearch",
  "cluster_uuid" : "FtKAQ6YeQ_KYz9-AYGy2yg",
  "version" : {
    "number" : "7.3.1",
    "build_flavor" : "default",
    "build_type" : "rpm",
    "build_hash" : "4749ba6",
    "build_date" : "2019-08-19T20:19:25.651794Z",
    "build_snapshot" : false,
    "lucene_version" : "8.1.0",
    "minimum_wire_compatibility_version" : "6.8.0",
    "minimum_index_compatibility_version" : "6.0.0-beta1"
  },
  "tagline" : "You Know, for Search"
}

```
exit Vagrant shell (where you were sudo)
and try interacting with elasticsearch from host:
```sh
curl 192.168.0.138:9200
```
should be 
```json
{
  "name" : "elasticsearch",
 "cluster_name" : "elasticsearch",
  "cluster_uuid" : "FtKAQ6YeQ_KYz9-AYGy2yg",
  "version" : {
    "number" : "7.3.1",
    "build_flavor" : "default",
    "build_type" : "rpm",
    "build_hash" : "4749ba6",
    "build_date" : "2019-08-19T20:19:25.651794Z",
    "build_snapshot" : false,
    "lucene_version" : "8.1.0",
    "minimum_wire_compatibility_version" : "6.8.0",
    "minimum_index_compatibility_version" : "6.0.0-beta1"
  },
  "tagline" : "You Know, for Search"
}


```
save the config
```sh
vagrant ssh
```
```sh
sudo cp /etc/elasticsearch/elasticsearch.yml ~
sudo chown vagrant:vagrant ~/elasticsearch.yml
```
```sh
scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -P 2222 vagrant@localhost:elasticsearch.yml .
```

```text
Warning: Permanently added '[localhost]:2222' (ECDSA) to the list of known hosts.
vagrant@localhost's password:
elasticsearch.yml                             100% 2853   392.3KB/s   00:00
```
initialiazize the schema
```sh
curl -H "Content-Type: application/json" -XDELETE 'http://192.168.0.138:9200/_template/my_index_test_template' 
```
NOTE: the URL appears to be different with later versions of ElasticSearch
```sh
curl -H "Content-Type: application/json" -XPUT "http://192.168.0.138:9200/_template/my_index_test_template" -d '{
  "template": "my_index_test*",
  "settings": {
  "analysis": {
      "analyzer": {
        "default": {
          "type": "keyword"
        }
      }
    },
    "number_of_shards": 3,
    "number_of_replicas": 0
  },
  "mappings": {
    "properties": {
      "createTime": {
        "type": "date",
        "format": "yyyy-MM-dd HH:mm:ss || yyyy-MM-dd || yyyy/MM/dd HH:mm:ss|| yyyy/MM/dd ||epoch_millis"
      },
      "cpu": {
        "type": "float"
      },
      "memory": {
        "type": "float"
      },
      "disk": {
        "type": "float"
      },
      "load_average": {
        "type": "float"
      },
      "rpm": {
        "type": "integer"
      },
      "uptime": {
        "type": "float"
      },
      "hostname": {
        "type": "keyword"
      },
      "environment": {
        "type": "keyword"
      },
      "dc": {
        "type": "keyword"
      },
      "appid": {
        "type": "keyword"
      }
    }
  },
  "aliases": {
    "my_index_test_alias": {}
  }
}
'
```

will respond with
```json
{"acknowledged":true}
```


Note: error when specifying the analyzer incorrectly:
```json
  "settings": {
    "analysis": {
      "analyzer": {
        "type": "keyword_analyzer"
      }
    },
```
getting error:
```json
{
  "error": {
    "root_cause": [
      {
        "type": "settings_exception",
        "reason": "Failed to get setting group for [index.analysis.analyzer.] setting prefix and setting [index.analysis.analyzer.type] because of a missing '.'"
      }
    ],
    "type": "settings_exception",
    "reason": "Failed to get setting group for [index.analysis.analyzer.] setting prefix and setting [index.analysis.analyzer.type] because of a missing '.'"
  },
  "status": 500
}

```
- see also [Default keyword analyzer](https://discuss.elastic.co/t/default-keyword-analyzer-not-work/293194)
See [schema reference](https://www.elastic.co/guide/en/elasticsearch/reference/7.17/number.html) for available metric types


Note: "_all" is no longer needs to be disabled - specifying:
```json
  "_all": {
    "enabled": false
  },


```
leads to error
```json
{
  "error": {
    "root_cause": [
      {
        "type": "illegal_argument_exception",
        "reason": "unknown setting [index._all.enabled] did you mean [index.warmer.enabled]?"
      }
    ],
    "type": "illegal_argument_exception",
    "reason": "unknown setting [index._all.enabled] did you mean [index.warmer.enabled]?"
  },
  "status": 400
}

```
acknowledge the index removal in Kibana `http://192.168.0.138:5601/app/kibana#/management/elasticsearch/index_management/indices?_g=()`

### Working with ElasticSearch from Java client

*  run the app

```sh
mvn spring-boot:run
```

* in the browser, or console, perform requests

NOTE: if the `query` is run before
`http://localhost:8844/query`

there have been any `insert`s, the index would not exist and application code does not currently catch it:

```txt
Servlet.service() for servlet [dispatcherServlet] in context with path [] threw exception [Request processing failed; nested exception is [my_index_test_alias]
ElasticsearchStatusException[Elasticsearch exception [type=index_not_found_exception, reason=no such index [my_index_test_alias]]]] with root cause
org.elasticsearch.ElasticsearchStatusException: Elasticsearch exception [type=index_not_found_exception, reason=no such index [my_index_test_alias]]
```
* in the browser or console, perform `insert`
```sh
http://localhost:8844/insert
```
will respond with
`SUCCESS`

repeating the `query` request now results in the collection JSON
```json
[{"createTime":1662352919707,"name":"Obj-13","id":"ac43c1bd-558d-4101-8793-3d0dca720c6b"}]
```
with the number of rows growing after more `insert` were performed

 * Note that `createTime` is in milliseconds:
```sh
date +"%s"
```
```text
1662353482
```
The index will be available in Kibana Management panel `http://192.168.0.138:5601/app/kibana#/management/elasticsearch/index_management/indices?_g=()`.

* observe data in Grafana

![Grafana Example](https://github.com/sergueik/springboot_study/blob/master/basic-elasticrsearch-timeseries/screenshots/capture-grafana-elasticsearch-panel.png)

### Note Using Bitnami ElasticSearch VM 
You may need to re-import the image ova when migrating the project to different host

If seeing 
the
```json
{"statusCode":503,"error":"Service Unavailable","message":"License is not available."}
```

on the port 80, after authenticating with `user` user and password from `~bitnami/bitnami_credentials`


may switch to use [Vagrantfile](https://github.com/sergueik/puppetmaster_vagrant/blob/master/elk/Vagrantfile) or [Vagrantfile](https://github.com/sergueik/puppetmaster_vagrant/blob/master/elk_box/Vagrantfile). This will take a little extra time to build custom ELK Virtualbox

### See Also
   * https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-network.html#network-interface-values
  * https://docs.bitnami.com/virtual-machine/apps/elk/administration/connect-remotely/
  * https://docs.bitnami.com/virtual-machine/faq/administration/use-firewall/
  * https://www.vagrantup.com/docs/networking/public_network
  * https://www.tecmint.com/install-netstat-in-linux/
  * https://discuss.elastic.co/t/network-host-issue-elasticsearch-service-main-process-exited-code-exited-status-78/200193

   * https://github.com/thoj/vagrant-influx-grafana oj/vagrant-influx-grafana - very old but may be easy to upgrade to latest grafana version.  effectively it is just
```sh
# install and start grafana
  wget https://grafanarel.s3.amazonaws.com/builds/grafana_2.1.1_amd64.deb
  sudo dpkg -i grafana_2.1.1_amd64.deb
  sudo /etc/init.d/grafana-server start
```  
  
  * https://github.com/djoven89/vagrant_monitoring_stack
is another alternative  adding grafana `https://packagecloud.io/grafana/stable/debian` repo public key and installing it on vanilla `ubuntu/xenial64`. We need Grafana __6.6.1__ to work with ElasticSearch __7.6.2__

 * https://www.lucenetutorial.com/lucene-query-syntax.html  

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
