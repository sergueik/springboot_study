### Usage

compiling and running the [basic-httpserver](https://github.com/sergueik/powershell_samples/tree/master/external/csharp/basic-httpserver) (a replica of [httpServer](https://github.com/qinyuanpei/HttpServer) with some fixes) on [Mono](https://www.mono-project.com/)

> NOTE: the original project also managed (?) to install and run [Nunit](https://docs.nunit.org/) [Console Test](https://github.com/nunit/nunit-console)

```sh
IMAGE=basic-mono
docker pull ubuntu:22.04
docker build -f Dockerfile -t $IMAGE .
```
followed by
```sh
NAME=$IMAGE
docker run --name $NAME -d -p 4050:4050 $IMAGE
```
```sh
echo '{"foo": "bar" }' > ./data.json
```
```sh
docker cp data.json $NAME:/tmp/
```
```text
Successfully copied 2.05kB to c9df5669806f:/tmp/app
```
```sh
curl -s -H 'Content-Type: application/json' http://localhost:4050/data.json
```

```json
{"foo": "bar"}
```

### Cleanup
```sh
docker container stop $NAME
docker container prune -f
docker image prune -f 
docker image rm $IMAGE
```


### Troubleshooting

```text
ERROR] FATAL UNHANDLED EXCEPTION: System.DllNotFoundException: urlmon.dll assembly:<unknown assembly> type:<unknown type> member:(null)
  at (wrapper managed-to-native) Utils.ResponseHelper.FindMimeFromData(intptr,string,byte[],int,string,int,intptr&,int)
  at Utils.ResponseHelper.GetMimeFromFile (System.String filePath) [0x00063] in <3befbba5c0784c24bfed9fefb3c22509>:0 
  at Utils.ResponseHelper.FromFile (Utils.HttpResponse response, System.String fileName) [0x00034] in <3befbba5c0784c24bfed9fefb3c22509>:0 
  at Program.ExampleServer.OnGet (Utils.HttpRequest request, Utils.HttpResponse response) [0x00056] in <bda7b169b55f46d099e918b96df97ff8>:0 
  at Utils.HttpServer.ProcessRequest (System.Net.Sockets.TcpClient handler) [0x00064] in <3befbba5c0784c24bfed9fefb3c22509>:0
```

Need to implement `MIME` mapping in pure __.Net__ - worked around with a  stub

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


