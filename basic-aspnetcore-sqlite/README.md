###  Info

replica of a boilerplate __ASP.NET Web API + Entity Framework__
[test repository](https://github.com/Bonifatius94/AspnetEfcoreTest)
but with SSL disabled, and added added `Elastic.Apm.NetCoreAll` to dependenies, and to `Startup.cs` for APM testing

![APM Services](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-sqlite/screenshots/capture-apm-services.png)

![ASP.Net Core REST App Events](https://github.com/sergueik/springboot_study/blob/master/basic-aspnetcore-sqlite/screenshots/capture-apm-aspnetcore-events.png)

### Usage

* pull the Docker images for runtime and SDK
```sh
docker pull mcr.microsoft.com/dotnet/sdk:6.0
docker pull mcr.microsoft.com/dotnet/aspnet:6.0
```

* build the application (NOTE: time comsuming)
```sh
IMAGE=basic-aspnetcore-sqlite
docker build -t $IMAGE -f Dockerfile .
```
* run the app in foreground
```
NAME=basic-aspnetcore-sqlite
ELK_NETWORK=basic-elk-cluster_elastic
docker container rm $NAME
docker run --name $NAME -e ASPNETCORE_URLS="http://+" -e ASPNETCORE_HTTP_PORT=80 -p 5000:80 --network $ELK_NETWORK -it $IMAGE
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
