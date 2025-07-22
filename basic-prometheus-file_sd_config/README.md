### Info

Plain Alpine __3.9.5__ container installing Perl with [Mojolicious](https://metacpan.org/pod/Mojolicious) Real-time Web Framework and 
to exercise specific REST API requesting passing the label as a GET  parameter for JSON data.


The stub REST API will return
<ol>
<li>
__400__ Bad Request if the ts parameter is missing
</li>
<li>
__405__ Method Not Allowed if the ts is present but invalid (e.g. not a proper Unix timestamp in milliseconds)
</li>
</ol>]

### Testing

* build the image
```sh
IMAGE=basic-prometheus-file-sd-config
docker build -t $IMAGE -f Dockerfile.myapp . --progress=plain
```
* ignore the security warning if docker is run in a Toolbox on Windows:
```text
SECURITY WARNING: You are building a Docker image from Windows against a non-Windows Docker host. All files and directories added to build context will have '-rwxr-xr-x' permissions. It is recommended to double check and reset permissions for sensitive files and directories.
```
* run new instance of default command

```sh
NAME=basic-prometheus-file-sd-config
docker container rm $NAME
docker run -d -p 9090:80 -p 9443:443 --name $NAME $IMAGE
docker logs -f $NAME
```

this will respond with
```sh
wait for app pid
app is running with ID 500
```
the value of `ID` varies. Sometimes the script output is not shown immediately, re-running the `logs` command shows it


if the container was already constructed
```sh
NAME=basic-perl-mojolicious
ID=$(docker container ls -a | grep $NAME|cut -f 1 -d ' ')
docker start $ID
```
```sh
docker ps
```
```text
CONTAINER ID        IMAGE                             COMMAND                  CREATED             STATUS              PORTS                                       NAMES
a150b402b7db        basic-prometheus-file-sd-config   "sh -c 'PIDFILE='/ru…"   4 minutes ago       Up 4 minutes        0.0.0.0:9090->80/tcp, 0.0.0.0:9443->443/tcp   basic-prometheus-file-sd-config
```

```
* test REST API
```sh
docker-machine ip
```

```text
192.168.99.100
```
* update the `MACHINE_IP` in the following command
```sh
MACHINE_IP=192.168.99.100
```
```sh
curl http://${MACHINE_IP}:9090/data/
```
```json
{"error":"Missing required 'ts' query parameter"}
```

```sh
curl http://${MACHINE_IP}:9090/data?ts=bad
```
```json
{"error":"'ts' must be a valid Unix timestamp in ms"}
```

```sh
curl http://${MACHINE_IP}:9090/data?ts=$(date +"%s")
```
```json
{"metric_value":42,"timestamp":"1753218030"}
```

the console log will show

```text

[2025-07-22 21:06:14.83955] [6] [trace] [cjJ6wxgn_Y1Z] GET "/data/"
[2025-07-22 21:06:14.84015] [6] [trace] [cjJ6wxgn_Y1Z] Routing to a callback
[2025-07-22 21:06:14.84047] [6] [info] Received timestamp:  data missing
[2025-07-22 21:06:14.84126] [6] [trace] [cjJ6wxgn_Y1Z] 400 Bad Request (0.001678s, 595.948/s)
[2025-07-22 21:06:07.14212] [6] [trace] [732JfPG5nEVj] GET "/data"
[2025-07-22 21:06:07.14314] [6] [trace] [732JfPG5nEVj] Routing to a callback
[2025-07-22 21:06:07.14410] [6] [info] Received timestamp: bad
[2025-07-22 21:06:07.14507] [6] [trace] [732JfPG5nEVj] 405 Method Not Allowed (0.002911s, 343.525/s)
[2025-07-22 21:00:30.86926] [6] [trace] [3ZdbFDfRdy5J] GET "/data"
[2025-07-22 21:00:30.87015] [6] [trace] [3ZdbFDfRdy5J] Routing to a callback
[2025-07-22 21:00:30.87056] [6] [info] Received timestamp: 1753218030
[2025-07-22 21:00:30.87117] [6] [trace] [3ZdbFDfRdy5J] 200 OK (0.001871s, 534.474/s)
```
```sh
docker exec $NAME cat /etc/prometheus/dynamic_targets.json
```
```json
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


