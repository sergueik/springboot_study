### Info
 Copy of

agentless prometheus/node_exporter
[remote_node_exporter](https://github.com/phuslu/remote_node_exporter)
linked to twodummy nodes running `gotechnies/alpine-ssh` [image](https://github.com/arvindr226/alpine-ssh), upgraced to Alpine __3.9.5__ base image
### Usage
*  build worker image
```sh
IMAGE1=worker
PASSWORD=TeStP_w0rD
docker build --build-arg PASSWORD=$PASSWORD -t $IMAGE1 -f Dockerfile.$IMAGE1 .
```

* run one or more worker image based container(s) in background
```sh
NAME1=worker1
docker run --name $NAME1 -d -p 2222:22 $IMAGE1
```
* connect from developer machine
```sh
ssh root@localhost -p 2222
```

type the password when prompted
```text
The authenticity of host '[localhost]:2222 ([127.0.0.1]:2222)' can't be established.
ECDSA key fingerprint is SHA256:KXkyX1YXUIpTJen2LMOipMXuLjqfm49ydWhIZCui3EA.
Are you sure you want to continue connecting (yes/no)? yes
Warning: Permanently added '[localhost]:2222' (ECDSA) to the list of known hosts.
```
```sh
root@localhost's password:
```
```text
Welcome to Alpine!

The Alpine Wiki contains a large amount of how-to guides and general
information about administrating Alpine systems.
See <http://wiki.alpinelinux.org/>.

You can setup the system with the command: setup-alpine

You may change this message by editing /etc/motd.
```
```sh
d5eecea1e26f:~# exit
```

```text
Connection to localhost closed.
```
NOTE: will need to delete the line in `~/.ssh/known_hosts` manually
* connect one worker the other:
```sh
NAME2=worker2
docker run --name $NAME2 --link $NAME1 -it $IMAGE1 sh
```
in the shell run
```sh
# ssh root@worker1
```
```text
The authenticity of host 'worker1 (172.17.0.2)' can't be established.
ECDSA key fingerprint is SHA256:KXkyX1YXUIpTJen2LMOipMXuLjqfm49ydWhIZCui3EA.
Are you sure you want to continue connecting (yes/no)? yes
Warning: Permanently added 'worker1,172.17.0.2' (ECDSA) to the list of known hosts.
```
type the password when prompted
```sh
root@worker1's password:
```

```text
Welcome to Alpine!

The Alpine Wiki contains a large amount of how-to guides and general
information about administrating Alpine systems.
See <http://wiki.alpinelinux.org/>.

You can setup the system with the command: setup-alpine

You may change this message by editing /etc/motd.
```
```sh
d5eecea1e26f:~# exit
```

```text
Connection to worker1 closed.
/ #
```
```sh
exit
```

restart worker2 in background
```sh
IMAGE1=worker
NAME2=worker2
docker run --name $NAME2 -d -p 2223:22 $IMAGE1
```
*  build remote node exporter
```sh
IMAGE2=alpine-remote-node-exporter
docker build -t $IMAGE2 -f Dockerfile.$IMAGE2 .
```
* run remote node exporter
```sh
docker run -p 9101:9101 -p 10001:10001 -p 10002:10002 --link worker1 --link worker2 -it $IMAGE2
```
it will print logs to console indicating successfully opening ssh connection to `worker` nodes:
```text
INFO[0000] Build context (go=devel +ffee3ab201 Thu Jul 26 00:01:11 2018 +0800, user=, date=)  source="remote_node_exporter.go:1031"
INFO[0033] ssh.Dial("tcp", "worker1:22", ...) ok

...
INFO[0033] "worker1:22" timezone is 0s, has timeout command is true source="remote_node_exporter.go:144"
INFO[0070] ssh.Dial("tcp", "worker2:22", ...) ok
...
```
show static page on additional ports:

```sh
curl http://localhost:10002/
```
```html
<html>
<head><title>Node Exporter</title></head>
<body>
<h1>Node Exporter</h1>
<p><a href="/metrics">Metrics</a></p>
</body>
</html>
```
connect to `remote_node_exporter` over extra TCP ports:
```sh
curl http://localhost:10001/metrics | tee worker1.txt
```

```sh
curl http://localhost:10002/metrics | tee worker2.txt
```
### Cleanup
```
docker container prune -f
```

### See Also 

  * [Kubernetes: monitoring with Prometheus – exporters, a Service Discovery, and its roles](https://rtfm.co.ua/en/kubernetes-monitoring-with-prometheus-exporters-a-service-discovery-and-its-roles/)
  * [Kubernetes monitoring with Prometheus, the ultimate guide](https://sysdig.com/blog/kubernetes-monitoring-prometheus/)
  * [Kubernetes Service Discovery for Prometheus](https://alexandrev.medium.com/kubernetes-service-discovery-for-prometheus-fcab74237db6)
  * [How to set up auto-discovery of Kubernetes endpoint services in Prometheus](https://www.acagroup.be/en/blog/auto-discovery-of-kubernetes-endpoint-services-prometheus)
  * [Discover applications running on Kubernetes with Prometheus](https://blog.sebastian-daschner.com/entries/prometheus-kubernetes-discovery)
  * [Intro to monitoring Kubernetes with Grafana Cloud](https://grafana.com/go/webinar/intro-to-monitoring-kubernetes/?src=ggl-s&mdm=cpc&camp=nb-kubernetes-exact&cnt=137839432452&trm=kubernetes%20application%20metrics&device=c&gclid=Cj0KCQjwmPSSBhCNARIsAH3cYgY0gYIL2McGmC2DKyhRMszDBpP33OBxvtH0g8pxMlPVyBJNTl-_dosaAsCSEALw_wcB)
  * [Monitoring Your Apps in Kubernetes Environment with Prometheus](https://medium.com/kubernetes-tutorials/monitoring-your-kubernetes-deployments-with-prometheus-5665eda54045)
  * [Monitoring Kubernetes with Prometheus & Grafana - 1/5](https://www.youtube.com/watch?v=bErGEHf6GCc) - illustratees "static configs" classic scenario - not showng the synthetic targets
  * [how relabeling works](https://grafana.com/blog/2022/03/21/how-relabeling-in-prometheus-works/)
  * [relabeling phase](https://github.com/prometheus/prometheus/blob/c0fd228badaa726e3549b5e9a5ab8351aa25cb13/docs/configuration/configuration.md#relabel_config) - NOTE, `relabel_config` is referring to target relabel configurations too, peer Prometheus documentation
  * [same](https://prometheus.io/docs/prometheus/latest/configuration/configuration/#relabel_config) on `prometheus.io`
  * [metric relabel config](https://github.com/prometheus/prometheus/blob/c0fd228badaa726e3549b5e9a5ab8351aa25cb13/docs/configuration/configuration.md#metric_relabel_configs)
  * [same](https://prometheus.io/docs/prometheus/latest/configuration/configuration/#metric_relabel_configs) on `prometheus.io`
  * [relabel_configs vs metric_relabel_configs](https://www.robustperception.io/relabel_configs-vs-metric_relabel_configs)
  * [taking advantage of Prometheus relabeling](https://www.slideshare.net/roidelapluie/taking-advantage-of-prometheus-relabeling-109483749)
  * [relabeling stepp by step](https://nsrc.org/workshops/2021/sanog37/nmm/netmgmt/en/prometheus/ex-relabeling.htm)
  * [relabeling tricks](https://medium.com/quiq-blog/prometheus-relabeling-tricks-6ae62c56cbda) - not covering target relabeling
  * [Relabeling in Prometheus and VictoriaMetrics](https://valyala.medium.com/how-to-use-relabeling-in-prometheus-and-victoriametrics-8b90fc22c4b2) - not covering target relabeling
  * file service discovery configurations a.k.a. `file_sd_configs` [](https://github.com/prometheus/prometheus/blob/c0fd228badaa726e3549b5e9a5ab8351aa25cb13/docs/configuration/configuration.md#file_sd_config) - also is used during (note - not in [master](https://github.com/prometheus/prometheus/blob/master/docs/configuration/configuration.md) for some reason?)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
