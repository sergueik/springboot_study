### Info
this directory contains a replica of
[ASP.NET Core Docker Sample](https://github.com/dotnet/dotnet-docker/tree/0fc0e2c6af6303cfd4676f1ac8c21090d82b0072) at commit  `0fc0e2c6af6303cfd4676f1ac8c21090d82b0072` *Update samples to .NET 6*


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
IMAGE=baic-aspnetapp
docker build -t $IMAGE -f Dockerfile .
```
```sh
NAME=basic-aspnetapp
docker run --name $NAME -it -p 8000:80 $IMAGE
```
or
```sh
docker start $NAME
```
```sh
curl -s http://localhost:8000 | grep Welcome
```
```text
<h1 class="display-4">Welcome to .NET</h1>
```
### TODO

* add __Elastic APM .NET agent__
  + [packaged](https://www.nuget.org/packages/Elastic.Apm.NetCoreAll/_

  + [sources](https://github.com/elastic/apm-agent-dotnet/blob/main/src/Elastic.Apm.NetCoreAll/Elastic.Apm.NetCoreAll.csproj) - not the best way to clone - too many interdependent projects

  + [releases](https://github.com/elastic/apm-agent-dotnet/releases)


### See Also

    * https://hub.docker.com/_/microsoft-dotnet-runtime/
    * https://hub.docker.com/_/microsoft-dotnet-sdk/
