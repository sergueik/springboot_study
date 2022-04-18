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
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
