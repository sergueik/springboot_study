### Info

this directory contains a replica of [javascriptrrd](http://javascriptrrd.sourceforge.net) 
__Javascript library for rrdtool graphs Project__ `javascriptrrd-1.1.1`


### Testing

* build the image
```sh
NAME=basic-apache
docker build -t $NAME -f Dockerfile .
```
* start run default command

```sh
docker run -d -p 8080:80 --name $NAME $NAME
```

* open demo page [http://127.0.0.1:8080/src/examples/index.html](http://127.0.0.1:8080/src/examples/index.html) in the browser

![Sample Header Information](https://github.com/sergueik/springboot_study/blob/master/basic-yugabytedb/screenshots/rrd_header_capture.png)

![Sample Graph with Flot](https://github.com/sergueik/springboot_study/blob/master/basic-yugabytedb/screenshots/rrd_graph_capture.png)
Use absolute path to access procided sample `rrd` files:
```sh
/data/example_rrds/example4.rrd
```
### Cleanup

``sh
docker rm -f $NAME
```

### See Also

  * [Javascript Flot renderer of RRDTool Network Traffic Monitoring data](https://www.youtube.com/watch?v=yY9rbOHxwyg) 
  * __JavascriptRRD__ [project page](https://sourceforge.net/projects/javascriptrrd/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
