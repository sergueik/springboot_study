### Info

this directory contains a replica of [javascriptrrd](http://javascriptrrd.sourceforge.net) 
__Javascript library for rrdtool graphs Project__ `javascriptrrd-1.1.1`
put in a minimal custom apache/alpine or a [Super Lightweight Nginx](https://hub.docker.com/r/flashspys/nginx-static) Docker container.


### Testing
#### Under Apache
* build the image
```sh
NAME=basic-apache
docker build -t $NAME -f Dockerfile.apache .
```
* start run default command

```sh
docker run -d -p 8080:80 --name $NAME $NAME
```
#### Under Nginx

```sh
NAME=basic-nginx
docker build -t $NAME -f Dockerfile.nginx .
```

* start run default command

```sh
docker run -d -p 8080:80 -v $(pwd):/static --name $NAME $NAME
```
alternatively simply pull and run in the `flashspys/nginx-static:latest`
```sh
IMAGE=flashspys/nginx-static:latest
docker pull $IMAGE
NAME=nginx-static
docker run -d -p 8080:80 -v $(pwd):/static --name $NAME $IMAGE
```

* open demo page [http://127.0.0.1:8080/src/examples/index.html](http://127.0.0.1:8080/src/examples/index.html) in the browser

![Sample Header Information](https://github.com/sergueik/springboot_study/blob/master/basic-rrd-js/screenshots/rrd_header_capture.png)

![Sample Graph with Flot](https://github.com/sergueik/springboot_study/blob/master/basic-rrd-js/screenshots/rrd_graph_capture.png)
Use absolute path to access procided sample `rrd` files:
```sh
/data/example_rrds/example4.rrd
```
### Cleanup

```sh
docker rm -f $NAME
docker image rm $NAME
```

### See Also

  * [Javascript Flot renderer of RRDTool Network Traffic Monitoring data](https://www.youtube.com/watch?v=yY9rbOHxwyg) 
  * __JavascriptRRD__ [project page](https://sourceforge.net/projects/javascriptrrd/)
  * https://jgefroh.medium.com/a-guide-to-using-nginx-for-static-websites-d96a9d034940

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
