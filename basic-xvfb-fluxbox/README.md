### Info

This direcrory contains a replica of [alpine xvfb fluxbox](https://github.com/drcrane/xvfb-alpine-docker) Docker project
### Usage
* build image
```sh
IMAGE_NAME=alpine-xvfb-fluxbox
chmod +x bootstrap.sh
docker build -t $IMAGE_NAME -f Dockerfile.vanilla .
```
* run container
```sh
docker run -d $IMAGE_NAME
ID=$(docker container ls | grep $IMAGE_NAME |awk '{print $1}')
docker inspect $ID | jq '.[]|.NetworkSettings.Networks.bridge.IPAddress'
```
* update the ipaddress of the server in connection file
```sh
server=172.17.0.2:0
```
* copy the connection file `connection.remmina` to
```sh
/home/$(whoami)/.local/share/remmina/
```

if you intend to run remmina from desktop or run is with the option:
```
remmina -c $(pwd)/connection.remmina
```
* destroy container
```sh
docker stop $ID
docker container rm $ID
docker container prune -f
docker image prune -f
IMAGE_ID=$(docker image ls |  grep "$IMAGE_NAME" |awk '{print $3}')
docker image rm $IMAGE_ID
```
![example](https://github.com/sergueik/springboot_study/blob/master/basic-xvfb-fluxbox/screenshots/capture.png)

### Note
* if the xvbf session starts and closes immediately, may need to adjust color depth
* the config file named like
```sh
/home/$(whoami)/.local/share/remmina/1617557944381.remmina
```
### Connecting from Remote Host

To be able to connect remotely add the port argument. First check what port framebuffer listen to
```sh
docker exec -it $ID netstat -ant
```
```sh
tcp        0      0 0.0.0.0:5900            0.0.0.0:*               LISTEN
tcp        0      0 :::5900                 :::*                    LISTEN
```
then publish it
```sh
docker stop $ID
docker run -p 5900:5900 -d $IMAGE_NAME
```


this will make it possible to connect into the container using any stock vnc client app, e.g. TightVNC Viewer on Windows machine.


![example](https://github.com/sergueik/springboot_study/blob/master/basic-xvfb-fluxbox/screenshots/capture_tightvnc_viewer.png)

### Note
* if the xvbf session starts and closes immediately, may need to adjust color depth
* the config file named like
```sh
/home/$(whoami)/.local/share/remmina/1617557944381.remmina
```
### See Also

* https://github.com/FreeRDP/Remmina/wiki/Remmina-Usage-FAQ
* https://github.com/gros777/fluxbox
* http://blog.fx.lv/2017/08/running-gui-apps-in-docker-containers-using-vnc/

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
