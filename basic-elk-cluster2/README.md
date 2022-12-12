###  Info

replica of a boilerplate __ASP.NET Web API + Entity Framework__
[test repository](https://github.com/Bonifatius94/AspnetEfcoreTest)
but with SSL disabled, and added added `Elastic.Apm.NetCoreAll` to dependenies, and to `Startup.cs` for APM testing and added certain `Serilog` packages as done in example [project](https://github.com/serilog/serilog-aspnetcore)
 using Serilog + Seq for storing and viewing logs

Note: not using newer syntax
[project](https://github.com/rdos85/PocDatalustSeq)

![APM Services](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-sqlite/screenshots/capture-apm-services.png)

![ASP.Net Core REST App Events](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-sqlite/screenshots/capture-apm-aspnetcore-events.png)

### Usage

* run via `docker-compose`
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up --build
```
NOTE,the `seq-server` Web UI will be available on port `8080`.
alternatively, launch individual nodes one by one as shown below


* run the `seq-server` configured just enough to prevent the aspnercore app from crashing - NOTE, probably will need a more elaborate configuration
```sh
SEQ_SERVER_NAME=seq-server
VENDOR_IMAGE=datalust/seq:2022.1.7378
docker run -p 80:80 -e ACCEPT_EULA=Y --name $SEQ_SERVER_NAME -d $VENDOR_IMAGE
```

* pull the Docker linux debian base images for runtime and SDK
```sh
docker pull mcr.microsoft.com/dotnet/sdk:6.0
docker pull mcr.microsoft.com/dotnet/aspnet:6.0
```

* pull the Docker linux alpine base images for runtime and SDK
```sh
docker pull mcr.microsoft.com/dotnet/runtime:6.0-alpine3.16-amd64
docker pull mcr.microsoft.com/dotnet/sdk:6.0-alpine3.16-amd64
```
* build the application (NOTE: time comsuming)
```sh
IMAGE=basic-aspnetcore-sqlite
docker build -t $IMAGE -f Dockerfile .
```
```sh
IMAGE=basic-aspnetcore-sqlite-alpine
docker build -t $IMAGE -f Dockerfile.alpine .
  # TypeError: The view function did not return a valid response. The return type must be a string, dict, tuple, Response instance, or WSGI callable, but it was a Response.
```
the build will fail on alpine images:

```text
Step 8/12 : RUN dotnet publish --no-self-contained --runtime linux-musl-x64     --configuration Release     --output /app/bin/ --no-restore
 ---> Running in 86cb524d3c1e
MSBuild version 17.3.2+561848881 for .NET
/usr/share/dotnet/sdk/6.0.403/Sdks/Microsoft.NET.Sdk/targets/Microsoft.PackageDependencyResolution.targets(267,5): error NETSDK1047: Assets file '/app/src/Api/obj/project.assets.json' doesn't have a target for 'net6.0/linux-musl-x64'. Ensure that restore has run and that you have included 'net6.0' in the TargetFrameworks for your project. You may also need to include 'linux-musl-x64' in your project's RuntimeIdentifiers. [/app/src/Api/Api.csproj]
```
* run the app in foreground
```
NAME=basic-aspnetcore-sqlite
ELK_NETWORK=basic-elk-cluster_elastic
docker container rm $NAME
docker run --name $NAME -e ASPNETCORE_URLS="http://+" -e ASPNETCORE_HTTP_PORT=80 -p 5000:80 --network $ELK_NETWORK -it $IMAGE
```

```sh
SEQ_SERVER_NAME=seq-server
NAME=basic-aspnetcore-sqlite
docker run --name $NAME --link $SEQ_SERVER_NAME -e ASPNETCORE_URLS="http://+" -e ASPNETCORE_HTTP_PORT=80 -p 5000:80 -it $IMAGE
```
```sh
SEQ_SERVER_NAME=seq-server
ELK_NETWORK=basic-elk-cluster_elastic
NAME=basic-aspnetcore-sqlite
docker run --name $NAME --link $SEQ_SERVER_NAME -e ASPNETCORE_URLS="http://+" -e ASPNETCORE_HTTP_PORT=80 -p 5000:80 --network $ELK_NETWORK -it $IMAGE
```
####  Verify

NOTE: operate through HTTP -  currently docker-compose fails with loading certificate

* define the base URL

```sh
URL=http://localhost:5000/todo
```
* query empty todo items list
```sh
curl -X GET $URL
```
* create a todo item
```sh
curl -d '{"Id":0,"Text":"Buy 2 bottles of milk","Due":"2021-08-23"}' -H 'Content-Type: application/json' -X POST $URL
```

* create one other todo item
```
curl -d '{"Id":0,"Text":"Buy a brezel and butter","Due":"2021-08-24"}' -H 'Content-Type: application/json' -X POST $URL
```
* query todo items list just created
```
curl -X GET $URL
```

* update the second todo item
```sh
curl -d '{"Id":2,"Text":"Buy a brezel and buttermilk","Due":"2021-08-26"}' -H 'Content-Type: application/json' -X PUT "$URL/2"
```
* query the updated todo item
```sh
curl -X GET "$URL/2"
```
* delete all todo items from database
```sh
curl -X DELETE "$URL/1"
curl -X DELETE "$URL/2"
```

* query todo items list (make sure deleted items are gone)
```sh
curl -X GET $URL
```

* REST call are captured

![HTTP Event Transaction](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-sqlite/screenshots/capture-apm-rest-call.png)

* Entity Framework calls initiated by REST call are captured

![SQL Event Transaction](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-sqlite/screenshots/capture-apm-entityframework-sqlite.png)

NOTE: the stack trace inclues agent implementation details of both APM and Microsoft:

```text
Database statement
```
```SQL
INSERT INTO "Todos" ("Due", "Text") VALUES (@p0, @p1); SELECT "Id" FROM "Todos" WHERE changes() = 1 AND "rowid" = last_insert_rowid();  
```


```c#
Elastic.Apm.Model.Span in .ctor in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.Model.Transaction in StartSpanInternal in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.Model.ExecutionSegmentCommon in StartSpanOnCurrentExecutionSegment in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.Model.DbSpanCommon in StartSpan in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.EntityFrameworkCore.EfCoreDiagnosticListener in HandleOnNext in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.DiagnosticListeners.DiagnosticListenerBase in OnNext in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
System.Diagnostics.DiagnosticListener in Write in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Diagnostics.Internal.RelationalCommandDiagnosticsLogger in BroadcastCommandExecuting in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Diagnostics.Internal.RelationalCommandDiagnosticsLogger in CommandReaderExecutingAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Storage.RelationalCommand+<ExecuteReaderAsync>d__19 in ExecuteReaderAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
System.Runtime.CompilerServices.AsyncMethodBuilderCore in Start in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Storage.RelationalCommand in ExecuteReaderAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Update.ReaderModificationCommandBatch+<ExecuteAsync>d__29 in ExecuteAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
System.Runtime.CompilerServices.AsyncMethodBuilderCore in Start in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Update.ReaderModificationCommandBatch in ExecuteAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Update.Internal.BatchExecutor+<ExecuteAsync>d__9 in ExecuteAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
System.Runtime.CompilerServices.AsyncMethodBuilderCore in Start in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Update.Internal.BatchExecutor in ExecuteAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Storage.RelationalDatabase in SaveChangesAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.ChangeTracking.Internal.StateManager+<SaveChangesAsync>d__103 in SaveChangesAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
System.Runtime.CompilerServices.AsyncMethodBuilderCore in Start in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.ChangeTracking.Internal.StateManager in SaveChangesAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.ChangeTracking.Internal.StateManager+<SaveChangesAsync>d__107 in SaveChangesAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
System.Runtime.CompilerServices.AsyncMethodBuilderCore in Start in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.ChangeTracking.Internal.StateManager in SaveChangesAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.ChangeTracking.Internal.StateManager+<>c in <SaveChangesAsync>b__106_0 in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.Storage.NonRetryingExecutionStrategy in ExecuteAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.ChangeTracking.Internal.StateManager in SaveChangesAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.DbContext+<SaveChangesAsync>d__60 in SaveChangesAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
System.Runtime.CompilerServices.AsyncMethodBuilderCore in Start in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.EntityFrameworkCore.DbContext in SaveChangesAsync x 1 in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
EfcoreTest.Api.Controllers.TodoController+<CreateTodo>d__5 in CreateTodo in /app/src/Api/Controllers/TodoController.cs at line 55
System.Runtime.CompilerServices.AsyncMethodBuilderCore in Start in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
EfcoreTest.Api.Controllers.TodoController in CreateTodo in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
N/A in lambda_method182 in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ActionMethodExecutor+AwaitableObjectResultExecutor+<Execute>d__0 in Execute in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
System.Runtime.CompilerServices.AsyncMethodBuilderCore in Start in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ActionMethodExecutor+AwaitableObjectResultExecutor in Execute in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ControllerActionInvoker+<<InvokeActionMethodAsync>g__Logged|12_1>d in <InvokeActionMethodAsync>g__Logged|12_1 in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
System.Runtime.CompilerServices.AsyncMethodBuilderCore in Start in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ControllerActionInvoker in <InvokeActionMethodAsync>g__Logged|12_1 in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ControllerActionInvoker in InvokeActionMethodAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ControllerActionInvoker in Next in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ControllerActionInvoker in InvokeNextActionFilterAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ControllerActionInvoker in Next in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ControllerActionInvoker in InvokeNextActionFilterAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ControllerActionInvoker in Next in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ControllerActionInvoker in InvokeInnerFilterAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ResourceInvoker in Next in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Microsoft.AspNetCore.Mvc.Infrastructure.ResourceInvoker in InvokeFilterPipelineAsync in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
```
NOTE: the .Net build option
```sh
# make a release build
RUN dotnet publish --no-self-contained --runtime linux-x64 \
    --configuration Release \
    --output /app/bin/ --no-restore
```

has no effect on stack trace, all numerous utility API call frames are shown

to find the call details relevant to the *business* application, pay attention to the calling class package. The example app package was named `EfcoreTest.Api`

Therefore the lines to inspect are the following two:

```text
EfcoreTest.Api.Controllers.TodoController+<CreateTodo>d__5 in CreateTodo in /app/src/Api/Controllers/TodoController.cs at line 55
EfcoreTest.Api.Controllers.TodoController in CreateTodo in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
```
The caller is a lambda in `CreateTodo`:
```c#
48        [HttpPost]
49        public async Task<TodoItem> CreateTodo([FromBody] TodoItem item)
50        {
51
52            // create the todo item
53            var newItemRef = await _dbContext.Todos.AddAsync(item);
53
54            // apply the changes to database
55            await _dbContext.SaveChangesAsync();
```
it is signaling APM to kick in via `System.Diagnostics.DiagnosticListener` producing an event the APM is subscribed to:
```text
Elastic.Apm.Model.Span in .ctor in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.Model.Transaction in StartSpanInternal in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.Model.ExecutionSegmentCommon in StartSpanOnCurrentExecutionSegment in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.Model.DbSpanCommon in StartSpan in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.EntityFrameworkCore.EfCoreDiagnosticListener in HandleOnNext in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
Elastic.Apm.DiagnosticListeners.DiagnosticListenerBase in OnNext in /usr/share/dotnet/shared/Microsoft.NETCore.App/6.0.11/System.Private.CoreLib.dll
```

### Cleanup

forcibly remove all running conatiners
```sh
docker container rm -f $(docker container ps -qa)
```

###  APM Agent Configuration 

The easiest way to navigate to the APM Agent Configuration Kibana screen is via direct url typing:

`http://192.168.0.92:5601/app/apm/settings/agent-configuration`
This will prompt for creating a configuration when none existed and selecting the agent.


### APM Transaction Fields

The most recent version of ELK 
__APM Transaction fields__ (Transaction-specific data for __APM__) [documentation](https://www.elastic.co/guide/en/apm/guide/master/exported-fields-apm-transaction.html)
says about `http.request.headers` and `http.response.headers` that <font color="red">Object is not enabled</font>. 
In fact these are the only propertes with this status.

a similar [request from customer](https://discuss.elastic.co/t/searching-for-a-custom-made-header/200403) concerning availability of some headers was advices i DIY fashion through
programmatically  adding the value of interest 
using APM agent public APIs []() and set the values of these headers as labels in the transaction that corresponds the request handling

* APM Java Agent Public API [reference](https://www.elastic.co/guide/en/apm/agent/java/current/public-api.html)
* APM .Net Agent Public API [reference](https://www.elastic.co/guide/en/apm/agent/dotnet/current/public-api.html)

say

`Transaction` `setLabel(String key, value)` (Added in 1.5.0 as addLabel) in Java and
`void SetLabel(string key, T value)` [1.7.0] in .Net

Labels are used to add *indexed information* (searchable and aggregatable) to __transactions__, __spans__, and __errors__. 

The __APM Transaction Field__ [documentation](https://www.elastic.co/guide/en/apm/guide/master/exported-fields-apm-transaction.html) defines

 --- | --- 
`labels` | A flat mapping of user-defined labels with string, boolean or number values
type |object
Yes  | ECS field

for Python there  is no such document as "public-api.html" and the full [method reference](https://www.elastic.co/guide/en/apm/agent/python/current/index.html)

I added the label call to Python flask example application according to the instruction and called the REST API the correspondent label information  appears in that transaction metadata screen:

![APM Transaction Labels](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster2/screenshots/capture-transaction-labels.png)
Since label is ECS, it is probably also can be used in regular search, i have not yet figured out, how. The earlier name `labels` in ElasticSearch was `context.tags`

Currently unclear if the label can be made part of the name of the transaction
For Java, one option (clearly this is what the document advised to take) is add `apm-agent-api` jar to the business application dependencies and the developer of the business application to add the code there. In Java it is also possible to add custom code to the Agent that is instrumenting the business applications. It will require moderate modification of APM Java Agent

For .Net one will need to ask the developer of the business application to add the code there.

The original __custom made header__ [document](https://discuss.elastic.co/t/searching-for-a-custom-made-header/200403/2) also includes customization at APM Server enabling specific header in the headers group in `apm-server.yml` (have not tried yet) 

The Java Agent `Labels` interface source is [here](https://github.com/elastic/apm-agent-java/blob/main/apm-agent-core/src/main/java/co/elastic/apm/agent/metrics/Labels.java#L4) and [here](https://github.com/elastic/apm-agent-java/blob/master/apm-agent-api/src/main/java/co/elastic/apm/api/Transaction.java#L141) and [here](https://github.com/elastic/apm-agent-java/blob/master/apm-agent-api/src/main/java/co/elastic/apm/api/TransactionImpl.java#L107) and `setLabel` test is [here](https://github.com/elastic/apm-agent-java/blob/main/apm-agent-plugins/apm-api-plugin/src/test/java/co/elastic/apm/agent/pluginapi/TransactionInstrumentationTest.java#L111)



### TODO

* currently `src/Api/appsettings.json` is copied into the build and app container, and not mapped. The changes made to `appsettings.json` while the application is running, appear to be ignored

* update the `docker-compose.yml` to connect to the network created earlier in `basic-elk-cluster`
```sh
docker-compose up -d
```
* generate and use certificates and enable HTTPS

* debug occasional error
```text
MSBuild version 17.3.2+561848881 for .NET
MSBUILD : error MSB1011: Specify which project or solution file to use because this folder contains more than one project or solution file.
The command '/bin/sh -c dotnet publish --runtime linux-x64 --configuration Release                    --output /app/bin/ --no-restore' returned a non-zero code: 1
```
this happen after the project directory was renamed but not pruned and it is difficult to debug due to [multi-stage](https://docs.docker.com/build/building/multi-stage/) nature of the `Dockerfile` commonly used for .Net Core apps

### See Also

  * https://stackoverflow.com/questions/55485511/how-to-run-dotnet-dev-certs-https-trust
  * __APM .NET Agent__

    + [documentation](https://github.com/elastic/apm-agent-dotnet/blob/main/docs/index.asciidoc)
    + [releases](https://github.com/elastic/apm-agent-dotnet/releases)
    + [packaged](https://www.nuget.org/packages/Elastic.Apm.NetCoreAll/)

  * [repository](https://github.com/elastic/elasticsearch-net) for `Elastic.Clients.Elasticsearch` the official .NET client, maintained and supported by Elastic with previous clients, `NEST` and `Elasticsearch.Net` in older branches 
  * tutorial [repository](https://github.com/elastic/elasticsearch-net-example/tree/7.x-codecomplete) for Elasticsearch and NEST
  * ELK Agent configuration [documentation](https://www.elastic.co/guide/en/kibana/7.17/agent-configuration.html)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
