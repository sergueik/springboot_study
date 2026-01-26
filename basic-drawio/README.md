### Info

Docker [image](https://hub.docker.com/r/jgraph/drawio) of [draw.io](https://app.diagrams.net) an Java back JavaScript, client-side editor for general diagramming.

download ubuntu version
```sh
export TAG='28.0.4'
docker pull jgraph/drawio:$TAG
```
or alpine version (docker image half the size)
```sh
export TAG=16.5.3-alpine
docker pull jgraph/drawio:$TAG
```
run service 
```sh
docker run -d --rm --name="draw" -p 8080:8080 -p 8443:8443 jgraph/drawio:$TAG
```
launch via `http://localhost:8080/?offline=1&https=0`
or running on Docker Toolbox `http://$(docker-machine ip):8080/?offline=1&https=0`

### See Also
  * [Desktop app](https://github.com/jgraph/drawio-desktop/releases/tag/v28.0.4)
  * https://app.diagrams.net/
  * https://jgraph.github.io/drawio/src/main/webapp/index.html
  * https://github.com/jgraph/drawio
  * dockerized `draw.io` image [source](https://github.com/fjudith/docker-draw.io)
