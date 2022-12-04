### Info

this directory contains basic __ASP.NET Core 6.0 Serilog__ [project](https://github.com/jernejk/AspNetCoreSerilogExample/) - only the pre-9(?) C# Serilog part
combined with [seq](https://hub.docker.com/r/datalust/seq) container to dump the messages pulled at revision __datalust/seq:2022.1.7378__. 
Note, __seq__ does not appear tobe availale for alpine or stretch, the image size is around 200Mb.

### Background

[codeproject article](https://www.codeproject.com/Articles/1278018/Best-Logging-libraries-for-ASP-NET-MVC) with links to separate projects for each of the common Logging frameworks:

* Log4net
* Serilog
* Nlog 
* Elmah

The Elastic APM currently [supports](https://www.elastic.co/guide/en/apm/agent/dotnet/current/log-correlation.html)
only Nlog and Serilog


### Usage

* pull the image for `seq-server`
```sh
docker pull datalust/seq:2022.1.7378
```
run the `seq-server` configured just enough to prevent the aspnercore app from crashing - NOTE, probably will need a more elaborate configuration
```sh
SEQ_SERVER_NAME=seq-server
VENDOR_IMAGE=datalust/seq:2022.1.7378
docker run -p 80:80 -e ACCEPT_EULA=Y --name $SEQ_SERVER_NAME -d $VENDOR_IMAGE
```
```sh
IMAGE=basic-aspnetapp-serilog
docker build -t $IMAGE -f Dockerfile .
```
* test without APM
```sh
SEQ_SERVER_NAME=seq-server
NAME=basic-aspnetapp-serilog
docker run --name $NAME --link $SEQ_SERVER_NAME -it -p 8000:80 $IMAGE
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

if the __Elastic APM Enricher__ was [added](https://www.elastic.co/guide/en/apm/agent/dotnet/current/serilog.html#serilog)


```C#
builder.Host.UseSerilog((ctx, loggerConfiguration) => {
  loggerConfiguration
    .ReadFrom.Configuration(ctx.Configuration)
    .Enrich.FromLogContext()
    .Enrich.WithProperty("ApplicationName", typeof(Program).Assembly.GetName().Name)
    .Enrich.WithProperty("Environment", ctx.HostingEnvironment)
    .Enrich.WithElasticApmCorrelationInfo()
    .Enrich.WithProperty("CustomProperty", "My Custom Property")
    .WriteTo.Console(outputTemplate: @"[ElasticApmTraceId:""{ElasticApmTraceId}"" ElasticApmTransactionId:""{ElasticApmTransactionId}"" ApplicatioName: ""{ApplicationName}"" CustomProperty: ""{CustomProperty}""] {Message:lj} {NewLine}{Exception}")
    .MinimumLevel.Debug();

```
 the log messages will have new entries 
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
but  providing a custom `ElasticApmTransactionId` [header](https://github.com/elastic/apm-agent-nodejs/issues/428) in curl web request
```sh
curl -H "X-Elastic-APM-Transaction-Id: 12345" -s http://192.168.99.100:8000/api/test/StructuredLog?input=Test
```
does not appear to cause any effect on the APM headers in the log 

```text
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Request finished HTTP/1.1
GET http://192.168.99.100:8000/api/test/StructuredLog?input=Test - - - 200 0 - 842.9206ms
```
probably incorrect syntax


![Seq First Log](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-serilog/screenshots/capture-seq.png)
procees as advised:
```sh
curl -s http://192.168.99.100:8000/api/test/StructuredLog?input=Test
```
observe detailed logs in seq-server


![Seq Structured Log](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-serilog/screenshots/capture-seq-structured-log.png)


### Building on Windows Host

* use same commands as in `Dockerfile`, but omit the release platform option
```cmd
dotnet.exe restore
```


```cmd
pushd AspNetCoreSerilogExample.Web
dotnet.exe publish -c release -o /app  --self-contained false --no-restore
popd
```

* Note: `run` only appears to work from project directory, not the 'c:\app':

```
pushd AspNetCoreSerilogExample.Web
dotnet.exe run bin\release\net6.0\AspNetCoreSerilogExample.Web.dll
popd
````
shows the logging 
```text
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Content root path: C:\developer\sergueik\springboot_study\basic-aspnetcore-serilog\AspNetCoreSerilogExample.Web\
[20:46:05 INF] HTTP/2 over TLS is not supported on Windows versions older than Windows 10 and Windows Server 2016 due to incompatible ciphers or missing ALPN support. Falling back to HTTP/1.1 instead.
[20:46:05 INF] Now listening on: https://localhost:5001
[20:46:05 INF] Now listening on: http://localhost:5000
[20:46:05 INF] Application started. Press Ctrl+C to shut down.
[20:46:05 INF] Hosting environment: Development
[20:46:05 INF] Content root path: C:\developer\sergueik\springboot_study\basic-aspnetcore-serilog\AspNetCoreSerilogExample.Web\
```
and 

```sh
curl http://localhost:5000
```
works, however
```cmd
pushd c:\app
dotnet run AspNetCoreSerilogExample.Web.dll
popd
```
returns an error
```
is not found
```

to change `--self-contained` to `true`

need to specify `RuntimeIdentifier` [](https://learn.microsoft.com/en-us/dotnet/core/rid-catalog)
but using `win` leads to an errors in RID graph


the dependency packages will be cached in `%USERPROfILE%\.nuget\packages`
`
### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
docker container stop $SEQ_SERVER_NAME
docker container rm $SEQ_SERVER_NAME
docker image prune -f
```
* NOTE presumably the multi-stage Docker build would cache some of the intermediate  images and not repeat the `restore` part

### TODO

  * send aspnetcore app logs directly to Elasticsearch by usiSng the `Serilog.Sinks.ElasticSearch` package and format the message according to __Elastic.CommonSchema__ via `Elastic.CommonSchema.Serilog.EcsTextFormatter` package 

### See Also

  * [Seq Documentation](https://docs.datalust.co/docs/getting-started-with-docker)
  * ELK APM Sut Logging support in .net a.k.a __Log Correlation__ [documentation](https://www.elastic.co/guide/en/apm/agent/dotnet/master/log-correlation.html)
  * APM Serilog Enricher [nuget package](https://www.nuget.org/packages/Elastic.Apm.SerilogEnricher) - OK to ignore the Frameworks tab info
  * Log4Net [tutorial](https://www.codeproject.com/Articles/140911/log-net-Tutorial)
  * https://www.codeproject.com/Articles/5306987/2-Ways-to-Log-the-Current-User-in-ASP-NET-Core -  covers adding custom fields. The repository "ASP.Net Boilerplate Demos As Shown by Lee Richardson on Blog and YouTube" [project](https://github.com/lprichar/LeesStore/tree/master/aspnet-core/src/LeesStore.Application) is somewhat big
  * https://github.com/dmonza/log4kibana - integrates log4net with ELK via `log4net.Appender.UdpAppender` class, not using the `log4net.Appender.RollingFileAppender`, `ConsoleAppender` or `FileAppender`      
  * custom [async logger](https://www.codeproject.com/Articles/1214072/A-Simple-Asynchronous-Logger-in-Csharp) - on .NEt Framework v4.5 


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
