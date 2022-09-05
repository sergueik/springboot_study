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
invoke-webrequest -uri 'https://app.vagrantup.com/elastic/boxes/debian-8-x86_64-test/versions/20200209.0.0/providers/virtualbox.box' -outfile "${env:USERPROFILE}\Downloads\debian-8-x86_64-test.box"
```
or 
```sh
curl -o ~/Downloads/debian-8-x86_64-test.box -Lk https://app.vagrantup.com/elastic/boxes/debian-8-x86_64-test/versions/20200209.0.0/providers/virtualbox.box
```
```sh
vagrant up
```
found that there is no elastic stact on this particulat box...

```sh
vagrant destroy -f
vagrant box remove elastic/debian-8-x86_64-test
```
### See Also
   * https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-network.html#network-interface-values
  * https://docs.bitnami.com/virtual-machine/apps/elk/administration/connect-remotely/
  * https://docs.bitnami.com/virtual-machine/faq/administration/use-firewall/
  * https://www.vagrantup.com/docs/networking/public_network
