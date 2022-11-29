### Info

this directory contains a replica of
[ASP.NET Core Docker Sample](https://github.com/dotnet/dotnet-docker/tree/0fc0e2c6af6303cfd4676f1ac8c21090d82b0072) at commit  `0fc0e2c6af6303cfd4676f1ac8c21090d82b0072` *Update samples to .NET 6*  with `aspnetapp` project from 
[ASP.NET Core Docker Container Sample for DigitalOcean App Platform](https://github.com/creativefisher/aspnetcoresample) added `Elastic.Apm.NetCoreAll` to dependenies, and to `Startup.cs`

  + [packaged](https://www.nuget.org/packages/Elastic.Apm.NetCoreAll/)

  + [sources](https://github.com/elastic/apm-agent-dotnet) - many interdependent projects see e.g. `Elastic.Apm.NetCoreAll.csproj`

  + [releases](https://github.com/elastic/apm-agent-dotnet/releases)

### Usage

* pull base images

```sh
docker pull mcr.microsoft.com/dotnet/runtime:6.0-alpine3.16-amd64
```

```sh
docker image ls | grep mcr.microsoft.com/dotnet/runtime
```
```text
mcr.microsoft.com/dotnet/runtime          6.0-alpine3.16-amd64    532cc32c09d3   13 days ago     79.7MB
```
```sh
docker pull mcr.microsoft.com/dotnet/sdk:6.0-alpine3.16-amd64
```

```sh
docker image ls | grep mcr.microsoft.com/dotnet/sdk
```
```text
mcr.microsoft.com/dotnet/sdk              6.0-alpine3.16-amd64    c5e11f288acd   13 days ago     604MB
```
```sh
docker pull mcr.microsoft.com/dotnet/aspnet:6.0-alpine-amd64
```
* build
```sh
IMAGE=basic-aspnetapp
docker build -t $IMAGE -f Dockerfile .
```
* test without APM
```sh
NAME=basic-aspnetapp
docker run --name $NAME -it -p 8000:80 $IMAGE
```
* test with APM
```sh
NAME=basic-aspnetapp
ELK_NETWORK=basic-elk-cluster_elastic 
docker run --name $NAME --network $ELK_NETWORK -it -p 8000:80 $IMAGE
```
or
```sh
docker start $NAME
```

![ASPNetCore Example](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore/screenshots/capture-aspnetcore.png)


```sh
curl -s http://localhost:8000 | grep Welcome
```
```text
<h1 class="display-4">Welcome to ASP.NET Core on DigitalOcean App Platform</h1>
```


![Kibana APM ASPNetCore Example](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore/screenshots/capture-apm-aspnetcore.png)


### C# Code Styles

  * vanilla ASP .Net Core code aparently is a *regular* one - usage of statement Lambda operator indicates it's a __6.x__ C#  not a __5.x__ compatible C#
    + `Program.cs`
```C#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

namespace aspnetapp {
  public class Program {
    public static void Main(string[] args) {
      CreateHostBuilder(args).Build().Run(); }

    public static IHostBuilder CreateHostBuilder(string[] args) =>
      Host.CreateDefaultBuilder(args).ConfigureWebHostDefaults(
        webBuilder => { webBuilder.UseStartup<Startup>(); });
   }	
}
```

  * Some Microsoft [example C#]() looks a bit excesively modern and resembles a VB.Net:
    + `Program.cs`
```C#
var builder = WebApplication.CreateBuilder(args);

builder.Services.AddRazorPages();

var app = builder.Build();

if (!app.Environment.IsDevelopment()) {
    app.UseExceptionHandler("/Error");
    app.UseHsts();
}

app.UseHttpsRedirection();
app.UseStaticFiles();

app.UseRouting();

app.UseAuthorization();

app.MapRazorPages();

app.Run();
```
The latter syntax change appears unnnecessary for such a basic app.
Besides the code snippets and documentation on [Elastic](https://github.com/elastic/apm-agent-dotnet/blob/main/docs/setup-asp-net-core.asciidoc) are written in __C# 6.0__

### Cleanup
```sh
NAME=basic-aspnetapp
docker container stop $NAME
docker container rm $NAME
IMAGE=basic-aspnetapp
docker image rm $IMAGE
```

![ASPNetCore Docker Toolbox Example](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore/screenshots/capture-aspnetcore-docker-toolbox.png)


### See Also

    * https://hub.docker.com/_/microsoft-dotnet-runtime/
    * https://hub.docker.com/_/microsoft-dotnet-sdk/
    * [C# language versioning](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/configure-language-version)
    * what's new in [C# 9](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-9)
    * what's new in [C# 10](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-10)
    * what's new in [C# 11](https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-11)
