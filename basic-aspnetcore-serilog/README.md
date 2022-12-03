### Info

this directory contains basic __ASP.NET Core 6.0 Serilog__ [project](https://github.com/jernejk/AspNetCoreSerilogExample/) - only the pre-9(?) C# Serilog part
combined with [seq](https://hub.docker.com/r/datalust/seq) container to dump the messages pulled at revision __datalust/seq:2022.1.7378__. 
Note, __seq__ does not appear tobe availale for alpine or stretch, the image size is around 200Mb.

### Usage

* pull the image for `seq-server`
```sh
docker pull datalust/seq:2022.1.7378
```
run the `seq-server` configured just enough to prevent the aspnercore app from crashing - NOTE, probably will need a more elaborate configuration
```sh
NAME=seq-server
VENDOR_IMAGE=datalust/seq:2022.1.7378
docker run -p 80:80 -e ACCEPT_EULA=Y --name $NAME -d $VENDOR_IMAGE
```
```sh
IMAGE=basic-aspnetapp-serilog
docker build -t $IMAGE -f Dockerfile .
```
* test without APM
```sh
SEQ_SERVER=seq-server
NAME=basic-aspnetapp-serilog
docker run --name $NAME --link $SEQ_SERVER -it -p 8000:80 $IMAGE
```

* test

if  run in Docker Toolbox,  use the ip address on card#2 network adapter which is connected to __Host-only__ network `192.168.99.0`. When on straight Linux use `localhost`

![Virtual Box VM Setting Network](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-serilog/screenshots/capture-virtualbox-network-adapter.png)
* interact with the app
use 
```sh
curl -s http://192.168.99.100:8000/

```
for the app, will get the response
```text
Hello World!
Use /api/test/flatlog?input=Test, /api/test/StructuredLog?input=Test, etc. and observe console/Seq for logs.
```
and `http://192.168.99.100/` for `seq-server`
you will see the logged message
```text
Request finished HTTP/1.1 GET http://192.168.99.100:8000/ - - - 200
```
and also the startup warmup messages:
```text

Content root path: /app/
Hosting environment: Production
Application started. Press Ctrl+C to shut down.
Now listening on: http://[::]:80
```
from aspnetcore app.

![Seq First Log](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-serilog/screenshots/capture-seq.png)
procees as advised:
```sh
curl -s http://192.168.99.100:8000/api/test/StructuredLog?input=Test
```
observe detailed logs in seq-server


![Seq Structured Log](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-serilog/screenshots/capture-seq-structured-log.png)
### Cleanup
```sh

docker container stop  $NAME
docker container stop  $SEQ_SERVER
docker image prine -f
```
* NOTE presumably the multi-stage Docker build would cache some of the intermeidate  images and not repeat the `restore` part

### See Also

  * [Seq Documentation](https://docs.datalust.co/docs/getting-started-with-docker)
  * ELK APM Sut Logging support in .net a.k.a __Log Correlation__ [documentation](https://www.elastic.co/guide/en/apm/agent/dotnet/master/log-correlation.html)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
