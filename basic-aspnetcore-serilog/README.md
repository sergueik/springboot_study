### Info

this directory contains basic __ASP.NET Core 6.0 Serilog__ [project](https://github.com/jernejk/AspNetCoreSerilogExample/) with the
`Elastic.Apm.SerilogEnricher` class [source](https://github.com/elastic/ecs-dotnet/tree/main/src/Elastic.Apm.SerilogEnricher)
from __NET integrations that use the Elastic Common Schema (ECS)__ [repo](https://github.com/elastic/ecs-dotnet)
added into workspace as a main application project dependency
linked with [seq](https://hub.docker.com/r/datalust/seq) container to dump the messages and eventually to be linked with the ELK cluster to send logs to

### Background

[codeproject article](https://www.codeproject.com/Articles/1278018/Best-Logging-libraries-for-ASP-NET-MVC) with links to separate projects for each of the common Logging frameworks:

* __Log4net__
* __Serilog__
* __Nlog__ 
* __Elmah__

The Elastic APM currently [supports](https://www.elastic.co/guide/en/apm/agent/dotnet/current/log-correlation.html)
only __Nlog__ and __Serilog__


### Usage

* pull the image for `seq-server` at revision __datalust/seq:2022.1.7378__. 
```sh
docker pull datalust/seq:2022.1.7378
```
Note, __seq__ does not appear tobe availale for alpine or stretch, the image size is around 535Mb.


* run the `seq-server` configured just enough to prevent the aspnercore app from crashing - NOTE, probably will need a more elaborate configuration
```sh
SEQ_SERVER_NAME=seq-server
VENDOR_IMAGE=datalust/seq:2022.1.7378
docker run -p 80:80 -e ACCEPT_EULA=Y --name $SEQ_SERVER_NAME -d $VENDOR_IMAGE
```
* build multi-stage
```sh
IMAGE=basic-aspnetapp-serilog
docker build -t $IMAGE -f Dockerfile .
```
* alternatively build in two steps with two Dockerfiles to assist troubleshooting:

```sh
docker container rm $NAME
```

```sh
BUILD_IMAGE=basic-aspnetapp-serilog-build
IMAGE=basic-aspnetapp-serilog
docker image rm $IMAGE
docker image rm $BUILD_IMAGE
docker build -t $BUILD_IMAGE -f Dockerfile.build .
docker build -f Dockerfile.app -t $IMAGE --build-arg BUILD_CONTAINER=$BUILD_IMAGE .
```
* run the app

```sh
SEQ_SERVER_NAME=seq-server
NAME=basic-aspnetapp-serilog
docker run --name $NAME --link $SEQ_SERVER_NAME -it -p 8000:80 $IMAGE
```
* test

* interact with the app
use 
```sh
curl -s http://192.168.99.100:8000/
```
if run in Docker Toolbox, use the ip address on network adapter which is connected to __Host-only__ network `192.168.99.0`. 

and `http://192.168.99.100/` for `seq-server`.
When on straight Linux use `localhost` for both
![Virtual Box VM Setting Network](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-serilog/screenshots/capture-virtualbox-network-adapter.png)
for the app, will get the response
```text
Hello World!
Use /api/test/flatlog?input=Test, /api/test/StructuredLog?input=Test, etc. and observe console/Seq for logs.
```
you will see the logged message

```text
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Request starting HTTP/1.1 GET http://192.168.0.92:8000/ - -
[20:12:15 INF] Request starting HTTP/1.1 GET http://192.168.0.92:8000/ - -
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Executing endpoint ' HTTP: GET'
[20:12:15 INF] Executing endpoint ' HTTP: GET'
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Executed endpoint ' HTTP: GET'
[20:12:15 INF] Executed endpoint ' HTTP: GET'
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] HTTP GET / responded 200 in 52.2764 ms
[20:12:15 INF] HTTP GET / responded 200 in 52.2764 ms
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Request finished HTTP/1.1 GET http://192.168.0.92:8000/ - - - 200 - - 68.7516ms
[20:12:15 INF] Request finished HTTP/1.1 GET http://192.168.0.92:8000/ - - - 200 - - 68.7516ms
`
```
and also the startup warmup messages:
```text
[20:09:56 INF] Now listening on: http://[::]:80
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Now listening on: http://[::]:80
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Application started. Press Ctrl+C to shut down.
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Hosting environment: Production
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Content root path: /app/
[20:09:56 INF] Application started. Press Ctrl+C to shut down.
[20:09:56 INF] Hosting environment: Production
[20:09:56 INF] Content root path: /app/
```
from aspnetcore app. NOTE: in ssh the colors may make some of the log invisible:

![SSH Console Colors](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-serilog/screenshots/capture-ssh-console-colors.png)


![Terminal Colors](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-serilog/screenshots/capture-terminal-colors.png)

The log message header entries are provided by the __Elastic APM Enricher__ was [added](https://www.elastic.co/guide/en/apm/agent/dotnet/current/serilog.html#serilog)


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
in the `Elastic.Apm.SerilogEnricher.ElasticApmEnricherExtension`:

```C#
namespace Elastic.Apm.SerilogEnricher {
  public static class ElasticApmEnricherExtension {
    public static LoggerConfiguration WithElasticApmCorrelationInfo(this LoggerEnrichmentConfiguration enrichmentConfiguration) {
      if (enrichmentConfiguration == null)
        throw new ArgumentNullException(nameof(enrichmentConfiguration));
      return enrichmentConfiguration.With<ElasticApmEnricher>();
```
and `Elastic.Apm.SerilogEnricher.ElasticApmEnricher`:

```c#
namespace Elastic.Apm.SerilogEnricher {
  public sealed class ElasticApmEnricher : ILogEventEnricher {
    public void Enrich(LogEvent logEvent, ILogEventPropertyFactory propertyFactory) {
      if (!Agent.IsConfigured) return;
      logEvent.AddPropertyIfAbsent(propertyFactory.CreateProperty("ElasticApmServiceName", Agent.Config.ServiceName));
      logEvent.AddPropertyIfAbsent(propertyFactory.CreateProperty("ElasticApmServiceVersion", Agent.Config.ServiceVersion));
      logEvent.AddPropertyIfAbsent(propertyFactory.CreateProperty("ElasticApmServiceNodeName", Agent.Config.ServiceNodeName));
      if (Agent.Tracer is null) return;
      if (Agent.Tracer.CurrentTransaction is null) return;
      logEvent.AddPropertyIfAbsent(propertyFactory.CreateProperty(
        "ElasticApmTransactionId", Agent.Tracer.CurrentTransaction.Id));
      logEvent.AddPropertyIfAbsent(propertyFactory.CreateProperty(
        "ElasticApmTraceId", Agent.Tracer.CurrentTransaction.TraceId));

...
```
For testing, one can provide a custom `ElasticApmTransactionId` [http header](https://github.com/elastic/apm-agent-nodejs/issues/428) in curl web request
```sh
curl -H "X-Elastic-APM-Transaction-Id: 12345" -s http://192.168.99.100:8000/api/test/StructuredLog?input=Test
```
but this does not appear to cause expected effect on the APM headers in the log, the properties added by are still blank

```text
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Request finished HTTP/1.1
GET http://192.168.99.100:8000/api/test/StructuredLog?input=Test - - - 200 0 - 842.9206ms
```
probably incorrect syntax - one of the checks performed by `Elastic.Apm.SerilogEnricher.ElasticApmEnricher` 
```c#	
if (!Agent.IsConfigured) return;
if (Agent.Tracer is null) return;
if (Agent.Tracer.CurrentTransaction is null) return;
```
is failing. Further debugging is possible, because the `Elastic.Apm.SerilogEnricher` is a part of the workspace:

```xml
 <ItemGroup>
    <ProjectReference Include="..\Elastic.Apm.SerilogEnricher\Elastic.Apm.SerilogEnricher.csproj" />
  </ItemGroup>
```

Adding code
```c#
namespace Elastic.Apm.SerilogEnricher {
  public sealed class ElasticApmEnricher : ILogEventEnricher {
    public void Enrich(LogEvent logEvent, ILogEventPropertyFactory propertyFactory) {
      logEvent.AddPropertyIfAbsent(propertyFactory.CreateProperty("ElasticApmCustomProperty", "Test"));
      if (!Agent.IsConfigured) return;
...
```
to the class in question
 and updating the template constructed in the `Program.cs`:
```sh

 builder.Host.UseSerilog((ctx, loggerConfiguration) => {
   loggerConfiguration
   .ReadFrom.Configuration(ctx.Configuration)
   .Enrich.FromLogContext()
   .Enrich.WithProperty("ApplicationName", typeof(Program).Assembly.GetName().Name)
   .Enrich.WithProperty("Environment", ctx.HostingEnvironment)
   .Enrich.WithElasticApmCorrelationInfo()
   .Enrich.WithProperty("CustomProperty", "My Custom Property")
   .WriteTo.Console(outputTemplate: @"[ElasticApmTraceId:""{ElasticApmTraceId}"" ElasticApmTransactionId:""{ElasticApmTransactionId}"" ElasticApmCustomProperty: ""{ElasticApmCustomProperty}"" ApplicatioName: ""{ApplicationName}"" CustomProperty: ""{CustomProperty}""] {Message:lj} {NewLine}{Exception}")
   .MinimumLevel.Debug();
```
proves it is being used -  the log entry receives `ElasticApmCustomProperty` data in the header:

```text
[ElasticApmTraceId:"" ElasticApmTransactionId:"" ElasticApmCustomProperty: "Test" ApplicatioName: "AspNetCoreSerilogExample.Web" CustomProperty: "My Custom Property"] Now listening on: http://[::]:80
[21:15:05 INF] Now listening on: http://[::]:80

```

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

need to specify `RuntimeIdentifier` Runtime Identifier 
but using `win` leads to an errors in RID graph


the dependency packages will be cached in `%USERPROfILE%\.nuget\packages`

```sh
IMAGE=basic-aspnetapp-serilog
docker build -t $IMAGE -f Dockerfile .
SEQ_SERVER_NAME=seq-server
NAME=basic-aspnetapp-serilog
docker run --name $NAME --link $SEQ_SERVER_NAME -it -p 8000:80 $IMAGE
```
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

  * Runtime Identifier [catalog](https://learn.microsoft.com/en-us/dotnet/core/rid-catalog)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
