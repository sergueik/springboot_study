f### Info

Plain Alpine 3.9 container installing Perl with Mojolicious


### Testing
* build the image
```sh
IMAGE=basic-perl-mojolicious
docker build -t $IMAGE -f Dockerfile . --progress=plain
```
* ignore the security warning if docker is run in a Toolbox on Windows:
```text
SECURITY WARNING: You are building a Docker image from Windows against a non-Windows Docker host. All files and directories added to build context will have '-rwxr-xr-x' permissions. It is recommended to double check and reset permissions for sensitive files and directories.
```
* run new instance of default command

```sh
NAME=basic-perl-mojolicious
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
CONTAINER ID        IMAGE               COMMAND                  CREATED     STATUS              PORTS                                         NAMES
c2312b18f192        basic-perl-mojolicious   "sh -c 'PIDFILE='/ru"   2 weeks ago    Up 2 minutes        0.0.0.0:9090->80/tcp, 0.0.0.0:9443->443/tcp   basic-perl-mojolicious
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
curl -s http://$MACHINE_IP:9090/api/greeting |jq.exe  '.'
```
```json
{
  "message": "Hello from Mojolicious"
}
```
* the console log will show
```text
[2025-07-19 03:53:28.54317] [6] [info] Listening at "http://*:80"
[2025-07-19 03:54:53.51606] [6] [trace] [_RS8hRSNGrr4] GET "/api/greeting"
[2025-07-19 03:54:53.51719] [6] [trace] [_RS8hRSNGrr4] Routing to a callback
[2025-07-19 03:54:53.51777] [6] [trace] [_RS8hRSNGrr4] 200 OK (0.001702s, 587.544/s)
```
* connect to container  and check version of Perl
```sh
docker exec -it $NAME sh
```


### See Also
  * https://stackoverflow.com/questions/75999522/perl-mojolicious-whats-the-correct-way-to-render-a-response-from-a-promise

  * https://metacpan.org/release/SRI/Mojolicious-9.41/source/lib/Mojolicious
  * https://fastapi.metacpan.org/source/SRI/Mojolicious-9.41/lib/
  * https://metacpan.org/pod/Mojolicious::Lite
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


