### Info 

replica of [Opvolger/json.grafana.datasources](https://github.com/Opvolger/json.grafana.datasources) a [simple-json](https://github.com/grafana/simple-json-backend-datasource)(NOTE: deprecated in favor []()) datasource with alerting support- requires one to run own REST http 1.1 server
### Usage

NOTE: now installation of the now legacy plugin on the Grafana node may be required:

```sh
grafana-cli plugins install grafana-simple-json-datasource
```

### Data initial table load /info
Post the following info into `http://localhost:8181/storedata/set_info`, or via [swagger](http://localhost:8181/swagger)

```json
{
  "name": "test_enkel",
  "info": {
    "description": "Overzicht voor test_enkel",
    "type": "default"
  },
  "table": [
    {
        "jsonvalue":  "key",
        "type":  "string",
        "text":  "Machinename"
    },
    {
        "jsonvalue":  "kolom_bool",
        "type":  "bool",
        "text":  "bool kolom"
    },
    {
        "jsonvalue":  "kolom_time",
        "type":  "time",
        "text":  "tijd kolom"
    },
    {
        "jsonvalue":  "kolom_string",
        "type":  "string",
        "text":  "string kolom"
    }
  ]
}
```
This will lead to creation of a 'test' directory within the data folder, containing two files: `table.json` and `info.json`.

### Sending Data

POST the following information to [storedata/send_data](http://localhost:8181/storedata/send_data); this can be done via [Swagger](http://localhost:8181/swagger).

```json
{
  "name": "test_enkel",
  "json_data": [ 
    { "key": "machine1", "kolom_bool": true, "kolom_time": "2020-10-27T21:24:31.78Z", "kolom_string": "iets" },
    { "key": "machine2", "kolom_bool": true, "kolom_time": "2020-10-27T21:24:31.78Z", "kolom_string": "iets meer" },
    { "key": "machine3", "kolom_bool": false, "kolom_time": "2020-10-28T21:24:31.78Z", "kolom_string": "iets minder" },
    { "key": "machine4", "kolom_bool": false, "kolom_time": "2020-11-27T21:24:31.78Z", "kolom_string": "niets" },
    { "key": "machine5", "kolom_bool": true, "kolom_time": "2019-10-27T21:24:31.78Z", "kolom_string": "hoi iets" }
  ] 
}
```

For futher information see original project.

### Troubleshooting

Error in runtime, after seeing node 'UP' for a little while:
```text
jsongrafanadatasources | crit: Microsoft.AspNetCore.Hosting.Diagnostics[6]
jsongrafanadatasources |       Application startup exception
jsongrafanadatasources |       System.InvalidOperationException: Endpoint Routing does not support 'IApplicationBuilder.UseMvc(...)'. To use 'IApplicationBuilder.UseMvc' set 'MvcOptions.EnableEndpointRouting = false' inside 'ConfigureServices(...).
jsongrafanadatasources |          at Microsoft.AspNetCore.Builder.MvcApplicationBuilderExtensions.UseMvc(IApplicationBuilder app, Action`1 configureRoutes)
jsongrafanadatasources |          at Microsoft.AspNetCore.Builder.MvcApplicationBuilderExtensions.UseMvc(IApplicationBuilder app)
jsongrafanadatasources |          at Json.Grafana.DataSources.Startup.Configure(IApplicationBuilder app, IHostingEnvironment env) in /src/json.grafana.datasources/Startup.cs:line 73
jsongrafanadatasources |          at System.RuntimeMethodHandle.InvokeMethod(Object target, Span`1& arguments, Signature sig, Boolean constructor, Boolean wrapExceptions)
jsongrafanadatasources |          at System.Reflection.RuntimeMethodInfo.Invoke(Object obj, BindingFlags invokeAttr, Binder binder, Object[] parameters, CultureInfo culture)
jsongrafanadatasources |          at Microsoft.AspNetCore.Hosting.ConfigureBuilder.Invoke(Object instance, IApplicationBuilder builder)
jsongrafanadatasources |          at Microsoft.AspNetCore.Hosting.ConfigureBuilder.<>c__DisplayClass4_0.<Build>b__0(IApplicationBuilder builder)
jsongrafanadatasources |          at Microsoft.AspNetCore.Hosting.ConventionBasedStartup.Configure(IApplicationBuilder app)
jsongrafanadatasources |          at Microsoft.AspNetCore.Mvc.Filters.MiddlewareFilterBuilderStartupFilter.<>c__DisplayClass0_0.<Configure>g__MiddlewareFilterBuilder|0(IApplicationBuilder builder)
jsongrafanadatasources |          at Microsoft.AspNetCore.HostFilteringStartupFilter.<>c__DisplayClass0_0.<Configure>b__0(IApplicationBuilder app)
jsongrafanadatasources |          at Microsoft.AspNetCore.Hosting.WebHost.BuildApplication()
jsongrafanadatasources | Unhandled exception. System.InvalidOperationException: Endpoint Routing does not support 'IApplicationBuilder.UseMvc(...)'. To use 'IApplicationBuilder.UseMvc' set 'MvcOptions.EnableEndpointRouting = false' inside 'ConfigureServices(...).
jsongrafanadatasources |    at Microsoft.AspNetCore.Builder.MvcApplicationBuilderExtensions.UseMvc(IApplicationBuilder app, Action`1 configureRoutes)
jsongrafanadatasources |    at Microsoft.AspNetCore.Builder.MvcApplicationBuilderExtensions.UseMvc(IApplicationBuilder app)
jsongrafanadatasources |    at Json.Grafana.DataSources.Startup.Configure(IApplicationBuilder app, IHostingEnvironment env) in /src/json.grafana.datasources/Startup.cs:line 73
jsongrafanadatasources |    at System.RuntimeMethodHandle.InvokeMethod(Object target, Span`1& arguments, Signature sig, Boolean constructor, Boolean wrapExceptions)
jsongrafanadatasources |    at System.Reflection.RuntimeMethodInfo.Invoke(Object obj, BindingFlags invokeAttr, Binder binder, Object[] parameters, CultureInfo culture)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.ConfigureBuilder.Invoke(Object instance, IApplicationBuilder builder)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.ConfigureBuilder.<>c__DisplayClass4_0.<Build>b__0(IApplicationBuilder builder)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.ConventionBasedStartup.Configure(IApplicationBuilder app)
jsongrafanadatasources |    at Microsoft.AspNetCore.Mvc.Filters.MiddlewareFilterBuilderStartupFilter.<>c__DisplayClass0_0.<Configure>g__MiddlewareFilterBuilder|0(IApplicationBuilder builder)
jsongrafanadatasources |    at Microsoft.AspNetCore.HostFilteringStartupFilter.<>c__DisplayClass0_0.<Configure>b__0(IApplicationBuilder app)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHost.BuildApplication()
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHost.StartAsync(CancellationToken cancellationToken)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHostExtensions.RunAsync(IWebHost host, CancellationToken token, String startupMessage)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHostExtensions.RunAsync(IWebHost host, CancellationToken token, String startupMessage)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHostExtensions.RunAsync(IWebHost host, CancellationToken token)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHostExtensions.Run(IWebHost host)
jsongrafanadatasources |    at Json.Grafana.DataSources.Program.Main(String[] args) in /src/json.grafana.datasources/Program.cs:line 10
jsongrafanadatasources | Application startup exception: System.InvalidOperationException: Endpoint Routing does not support 'IApplicationBuilder.UseMvc(...)'. To use 'IApplicationBuilder.UseMvc' set 'MvcOptions.EnableEndpointRouting = false' inside 'ConfigureServices(...).
jsongrafanadatasources |    at Microsoft.AspNetCore.Builder.MvcApplicationBuilderExtensions.UseMvc(IApplicationBuilder app, Action`1 configureRoutes)
jsongrafanadatasources |    at Microsoft.AspNetCore.Builder.MvcApplicationBuilderExtensions.UseMvc(IApplicationBuilder app)
jsongrafanadatasources |    at Json.Grafana.DataSources.Startup.Configure(IApplicationBuilder app, IHostingEnvironment env) in /src/json.grafana.datasources/Startup.cs:line 73
jsongrafanadatasources |    at System.RuntimeMethodHandle.InvokeMethod(Object target, Span`1& arguments, Signature sig, Boolean constructor, Boolean wrapExceptions)
jsongrafanadatasources |    at System.Reflection.RuntimeMethodInfo.Invoke(Object obj, BindingFlags invokeAttr, Binder binder, Object[] parameters, CultureInfo culture)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.ConfigureBuilder.Invoke(Object instance, IApplicationBuilder builder)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.ConfigureBuilder.<>c__DisplayClass4_0.<Build>b__0(IApplicationBuilder builder)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.ConventionBasedStartup.Configure(IApplicationBuilder app)
jsongrafanadatasources |    at Microsoft.AspNetCore.Mvc.Filters.MiddlewareFilterBuilderStartupFilter.<>c__DisplayClass0_0.<Configure>g__MiddlewareFilterBuilder|0(IApplicationBuilder builder)
jsongrafanadatasources |    at Microsoft.AspNetCore.HostFilteringStartupFilter.<>c__DisplayClass0_0.<Configure>b__0(IApplicationBuilder app)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHost.BuildApplication()
```

after fixing the bootstrap code, the excetpion becomes

```text
rces | crit: Microsoft.AspNetCore.Hosting.Diagnostics[6]
jsongrafanadatasources |       Application startup exception
jsongrafanadatasources |       System.MissingMethodException: Method not found: 'Void Microsoft.AspNetCore.StaticFiles.StaticFileMiddleware..ctor(Microsoft.AspNetCore.Http.RequestDelegate, Microsoft.AspNetCore.Hosting.IHostingEnvironment, Microsoft.Extensions.Options.IOptions`1<Microsoft.AspNetCore.Builder.StaticFileOptions>, Microsoft.Extensions.Logging.ILoggerFactory)'.
jsongrafanadatasources |          at Swashbuckle.AspNetCore.SwaggerUI.SwaggerUIMiddleware.CreateStaticFileMiddleware(RequestDelegate next, IHostingEnvironment hostingEnv, ILoggerFactory loggerFactory, SwaggerUIOptions options)
jsongrafanadatasources |          at Swashbuckle.AspNetCore.SwaggerUI.SwaggerUIMiddleware..ctor(RequestDelegate next, IHostingEnvironment hostingEnv, ILoggerFactory loggerFactory, SwaggerUIOptions options)
jsongrafanadatasources |          at System.RuntimeMethodHandle.InvokeMethod(Object target, Span`1& arguments, Signature sig, Boolean constructor, Boolean wrapExceptions)
jsongrafanadatasources |          at System.Reflection.RuntimeConstructorInfo.Invoke(BindingFlags invokeAttr, Binder binder, Object[] parameters, CultureInfo culture)
jsongrafanadatasources |          at Microsoft.Extensions.Internal.ActivatorUtilities.ConstructorMatcher.CreateInstance(IServiceProvider provider)
jsongrafanadatasources |          at Microsoft.Extensions.Internal.ActivatorUtilities.CreateInstance(IServiceProvider provider, Type instanceType, Object[] parameters)
jsongrafanadatasources |          at Microsoft.AspNetCore.Builder.UseMiddlewareExtensions.<>c__DisplayClass5_0.<UseMiddleware>b__0(RequestDelegate next)
jsongrafanadatasources |          at Microsoft.AspNetCore.Builder.ApplicationBuilder.Build()
jsongrafanadatasources |          at Microsoft.AspNetCore.Hosting.WebHost.BuildApplication()
jsongrafanadatasources | Unhandled exception. System.MissingMethodException: Method not found: 'Void Microsoft.AspNetCore.StaticFiles.StaticFileMiddleware..ctor(Microsoft.AspNetCore.Http.RequestDelegate, Microsoft.AspNetCore.Hosting.IHostingEnvironment, Microsoft.Extensions.Options.IOptions`1<Microsoft.AspNetCore.Builder.StaticFileOptions>, Microsoft.Extensions.Logging.ILoggerFactory)'.
jsongrafanadatasources |    at Swashbuckle.AspNetCore.SwaggerUI.SwaggerUIMiddleware.CreateStaticFileMiddleware(RequestDelegate next, IHostingEnvironment hostingEnv, ILoggerFactory loggerFactory, SwaggerUIOptions options)
jsongrafanadatasources |    at Swashbuckle.AspNetCore.SwaggerUI.SwaggerUIMiddleware..ctor(RequestDelegate next, IHostingEnvironment hostingEnv, ILoggerFactory loggerFactory, SwaggerUIOptions options)
jsongrafanadatasources |    at System.RuntimeMethodHandle.InvokeMethod(Object target, Span`1& arguments, Signature sig, Boolean constructor, Boolean wrapExceptions)
jsongrafanadatasources |    at System.Reflection.RuntimeConstructorInfo.Invoke(BindingFlags invokeAttr, Binder binder, Object[] parameters, CultureInfo culture)
jsongrafanadatasources |    at Microsoft.Extensions.Internal.ActivatorUtilities.ConstructorMatcher.CreateInstance(IServiceProvider provider)
jsongrafanadatasources |    at Microsoft.Extensions.Internal.ActivatorUtilities.CreateInstance(IServiceProvider provider, Type instanceType, Object[] parameters)
jsongrafanadatasources |    at Microsoft.AspNetCore.Builder.UseMiddlewareExtensions.<>c__DisplayClass5_0.<UseMiddleware>b__0(RequestDelegate next)
jsongrafanadatasources |    at Microsoft.AspNetCore.Builder.ApplicationBuilder.Build()
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHost.BuildApplication()
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHost.StartAsync(CancellationToken cancellationToken)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHostExtensions.RunAsync(IWebHost host, CancellationToken token, String startupMessage)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHostExtensions.RunAsync(IWebHost host, CancellationToken token, String startupMessage)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHostExtensions.RunAsync(IWebHost host, CancellationToken token)
jsongrafanadatasources |    at Microsoft.AspNetCore.Hosting.WebHostExtensions.Run(IWebHost host)
jsongrafanadatasources |    at Json.Grafana.DataSources.Program.Main(String[] args) in /src/json.grafana.datasources/Program.cs:line 10
jsongrafanadatasources exited with code 139 
```
```sh
docker-compose ps
```
```text
                   Name                                 Command               State               Ports
-------------------------------------------------------------------------------------------------------------------
basic-aspnetcore-json-datasource_grafana_1   /run.sh                          Up      0.0.0.0:8182->3000/tcp
jsongrafanadatasources                       dotnet Json.Grafana.DataSo ...   Up      443/tcp, 0.0.0.0:8181->80/tcp
```
### Simplifying the Project Folder Structure and Settings

```sh
jq '.profiles.Program' < Program/Properties/launchSettings.json
```
```json
{
  "commandName": "Project",
  "applicationUrl": "http://localhost:5000",
  "environmentVariables": {
    "ASPNETCORE_ENVIRONMENT": "Development"
  }
}
```
