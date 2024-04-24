### Info

this directory contains an [example of using multi stage build](https://qna.habr.com/q/1348210?e=14409710#clarification_1876590) to reduce built time of aspnetcore microservices placed into separate containers but featuring the common shared library

The original problem arise with the Docker-unfriendly shared library direcrory layout produced by Visual Studio

For application code is based on the  [exampleapp](https://github.com/omerasaf01/AspNetCoreDockerExample) 

### Usage

```sh
docker pull mcr.microsoft.com/dotnet/sdk:6.0-alpine
docker pull mcr.microsoft.com/dotnet/aspnet:6.0-alpine-amd64
```
```sh
IMAGE=basic-aspnetcore-shared
docker build -t $IMAGE -f Dockerfile .
```
* inspect the application directory

```sh
NAME=basic-aspnetcore-shared
docker run --rm --name $NAME  -p 8000:80 --entrypoint sh -it $IMAGE
```
in the container
```sh
pwd
```
```text
/app
```
```sh
ls -1
```

```text
Microsoft.OpenApi.dll
Swashbuckle.AspNetCore.Swagger.dll
Swashbuckle.AspNetCore.SwaggerGen.dll
Swashbuckle.AspNetCore.SwaggerUI.dll
app
app.deps.json
app.dll
app.pdb
app.runtimeconfig.json
appsettings.Development.json
appsettings.json
```
* run the application without leaving the container

```sh
dotnet app.dll
```
this will print

```text
info: Microsoft.Hosting.Lifetime[14]
      Now listening on: http://[::]:80
info: Microsoft.Hosting.Lifetime[0]
      Application started. Press Ctrl+C to shut down.
info: Microsoft.Hosting.Lifetime[0]
      Hosting environment: Production
info: Microsoft.Hosting.Lifetime[0]
      Content root path: /app/

```

and on the host will observe port `8000` LISTEN:
```sh
netstat -ant| grep LISTEN |grep 8000
```
```text
tcp        0      0 0.0.0.0:8000            0.0.0.0:*               LISTEN
tcp6       0      0 :::8000                 :::*                    LISTEN
```
```sh
curl -H 'Content-type: application/json' -XPOST http://localhost:8000/api/user -d '{"username":"alexander"}'
```
```json
{"statu":"Succes"}

```
indicating the service code is operating
alternatiely
```sh
NAME=basic-aspnetcore-shared
docker run --name $NAME -it -p 8000:80 $IMAGE
```
### See Also
 * https://stackoverflow.com/questions/40108106/reference-external-dll-in-net-core-project

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
