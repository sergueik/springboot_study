### Info

this directory contains basic __ASP.NET Core 6.0 Serilog__ [project](https://github.com/jernejk/AspNetCoreSerilogExample/) with APM Serilog Enricher [nuget package](https://www.nuget.org/packages/Elastic.Apm.SerilogEnricher) dependency hosted on Docker container based on __ASP.NET Core Runtime__ on __Alpine__ [image](https://hub.docker.com/_/microsoft-dotnet-aspnet) by Microsoft linked with [seq](https://hub.docker.com/r/datalust/seq) container to dump the messages pulled at revision __datalust/seq:2022.1.7378__. 
Note, __seq__ does not appear tobe available for alpine or stretch, the image size is around 200Mb.

### Usage

* pull the image for `seq-server`
```sh
docker pull datalust/seq:2022.1.7378
```
run the `seq-server`
```sh
SEQ_SERVER_NAME=seq-server
VENDOR_IMAGE=datalust/seq:2022.1.7378
docker run -p 80:80 -e ACCEPT_EULA=Y --name $SEQ_SERVER_NAME -d $VENDOR_IMAGE
```
* build
```sh
IMAGE=basic-aspnetapp-serilog-alpine
docker build -t $IMAGE -f Dockerfile.alpine .
```
* run
```sh
SEQ_SERVER_NAME=seq-server
NAME=basic-aspnetapp-serilog-alpine
docker run --name $NAME --link $SEQ_SERVER_NAME -it -p 8000:80 $IMAGE
```

* test

```sh
curl -s http://localhost:8000/
```
```text
Hello World!
Use /api/test/flatlog?input=Test, /api/test/StructuredLog?input=Test, etc. and observe console/Seq for logs.
```
see the logged message in console

```text
[16:56:02 INF] Now listening on: http://[::]:80
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Now listening on: http://[::]:80
[16:56:03 INF] Application started. Press Ctrl+C to shut down.
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Application started. Press Ctrl+C to shut down.
[16:56:03 INF] Hosting environment: Production
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Hosting environment: Production
[16:56:03 INF] Content root path: /app/
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Content root path: /app/    
```
repeat providing a custom `ElasticApmTransactionId` [header](https://github.com/elastic/apm-agent-nodejs/issues/428) in curl web request
```sh
curl -H "X-Elastic-APM-Transaction-Id: 12345" -s http://192.168.99.100:8000/api/test/StructuredLog?input=Test
```
NOTE: currently this does not appear to have the expected effect on the APM headers in the log 

```text
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Request finished HTTP/1.1
GET http://192.168.99.100:8000/api/test/StructuredLog?input=Test - - - 200 0 - 842.9206ms
```
probably incorrect syntax

### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
docker container stop $SEQ_SERVER_NAME
docker container rm $SEQ_SERVER_NAME
docker image prune -f
```
### TODO

  * send aspnetcore app logs directly to Elasticsearch by usiSng the `Serilog.Sinks.ElasticSearch` package and format the message according to __Elastic.CommonSchema__ via `Elastic.CommonSchema.Serilog.EcsTextFormatter` package 

### See Also

  * __Elastic APM Enricher__ [documentation](https://www.elastic.co/guide/en/apm/agent/dotnet/current/serilog.html#serilog)
  * [Seq Documentation](https://docs.datalust.co/docs/getting-started-with-docker)
  * ELK APM Sut Logging support in .net a.k.a __Log Correlation__ [documentation](https://www.elastic.co/guide/en/apm/agent/dotnet/master/log-correlation.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
