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

### TODO

* currently `src/Api/appsettings.json` is copied into the build and app container, and not mapped. The changes made to `appsettings.json` while the application is running, appear to be ignored

* update the `docker-compose.yml` to connect to the network created earlier in `basic-elk-cluster`
```sh
docker-compose up -d
```
* generate and use certificates and enable HTTPS

### TODO
occasional error
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
	


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
