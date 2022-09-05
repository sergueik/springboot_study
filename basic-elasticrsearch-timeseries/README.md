### Info

replica of [Spring-boot-elasticsearch-timeseries-demo](https://github.com/wilsonyy/spring-boot-elasticsearch-timeseries-demo) with single instance ELK server
 the Elasticsearch is used to store time series database
The original project focus was with shard creation.

Use [elasticsearch stack standalone Vagrantbox packaged by Bitnami](https://bitnami.com/stack/elk) as single ELK node (set number of replicas to zero)
This appliance is not configured to allow data posting to Elasticsearch from external hosts by default.


### Usage

You may need to re-import the image ova when migrating the project to different host

If seeing 
the
```json
{"statusCode":503,"error":"Service Unavailable","message":"License is not available."}
```

on the port 80, after authenticating with `user` user and password from `~bitnami/bitnami_credentials`


may switch to use [Vagrantfile](https://github.com/sergueik/puppetmaster_vagrant/blob/master/elk/Vagrantfile) or [Vagrantfile](https://github.com/sergueik/puppetmaster_vagrant/blob/master/elk_box/Vagrantfile). This will take a little extra time to build custom ELK Virtualbox
### Download Vargant Box

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
curl -H "Content-Type: application/json" -XPUT "http://192.168.0.138:9200/_template/my_index_test_template" -d '{
  "template": "my_index_test*",
  "settings": {
    "number_of_shards": 3,
    "number_of_replicas": 0
  },
  "mappings": {
        "properties": {
            "createTime": {
                "type": "date",
                "format": "yyyy-MM-dd HH:mm:ss || yyyy-MM-dd || yyyy/MM/dd HH:mm:ss|| yyyy/MM/dd ||epoch_millis"
            },
            "id": {
                "type": "keyword"
            },
            "name": {
                "type": "keyword"
            }
        }
    },
  "aliases": {"my_index_test_alias":{}}
}'
```

```json
{"acknowledged":true}
```

### Working with ElasticSearch from Java client

### See Also
   * https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-network.html#network-interface-values
  * https://docs.bitnami.com/virtual-machine/apps/elk/administration/connect-remotely/
  * https://docs.bitnami.com/virtual-machine/faq/administration/use-firewall/
  * https://www.vagrantup.com/docs/networking/public_network
  * https://www.tecmint.com/install-netstat-in-linux/
  * https://discuss.elastic.co/t/network-host-issue-elasticsearch-service-main-process-exited-code-exited-status-78/200193


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
