
i### Info

  This project contains exercises covered in [Azure/AppConfiguration-Emulator](https://github.com/Azure/AppConfiguration-Emulator)

NOTE: the meaning of *emulator* is apparently non-standard in Microsoft vocabulary.

### Usage

```sh
docker pull mcr.microsoft.com/azure-app-configuration/app-configuration-emulator:1.2.0
```
```sh
docker image ls
```
```text
mcr.microsoft.com/azure-app-configuration/app-configuration-emulator   1.2.0                 f71d872aeff8        2 months ago        133MB
```

```
docker run --rm -it mcr.microsoft.com/azure-app-configuration/app-configuration-emulator:1.2.0 --entrypoint '' sh
```
```text
Failed to create CoreCLR, HRESULT: 0x80070008
```
```
Inspecting error code: 2147942408
Not enough memory resources are available to process this command. (Exception from HRESULT: 0x80070008)

```
from Linux host
```sh
docker run -it --entrypoint '' mcr.microsoft.com/azure-app-configuration/app-configuration-emulator:1.2.0 echo test
```
```text
docker: Error response from daemon: failed to create task for container:
failed to create shim task:
OCI runtime create failed:
runc create failed:
unable to start container process:
error during container init:
exec: "echo": executable file not found in $PATH
```

`mcr.microsoft.com/azure-app-configuration/app-configuration-emulator` is an dotnet 8.0.x

```sh
docker run -d -p 8483:8483 -e Tenant:AnonymousAuthEnabled=true -e Authentication:Anonymous:AnonymousUserRole=Owner mcr.microsoft.com/azure-app-configuration/app-configuration-emulator:1.2.0
```
```sh
docker container ls -a
```
Docker Toolbox host:

```text
CONTAINER ID        IMAGE                                                                        COMMAND                  CREATED             STATUS                        PORTS               NAMES
ce4b7481fa8e        mcr.microsoft.com/azure-app-configuration/app-configuration-emulator:1.2.0   "dotnet Azure.AppCon…"   11 seconds ago      Exited (137) 10 seconds ago                       sweet_moser
```
> NOTE: container was forcibly terminated by the operating system using a SIGKIL

Linux host:
```
CONTAINER ID   IMAGE                                                                        COMMAND                  CREATED          STATUS          PORTS                                         NAMES
1a14241e0c40   mcr.microsoft.com/azure-app-configuration/app-configuration-emulator:1.2.0   "dotnet Azure.AppCon…"   33 seconds ago   Up 33 seconds   0.0.0.0:8483->8483/tcp, [::]:8483->8483/tcp   sweet_mendel
```

```sh
curl -I http://localhost:8483

```
```text
HTTP/1.1 200 OK
Content-Length: 415
Content-Type: text/html
Date: Tue, 03 Nov 2026 00:49:39 GMT
Server: Kestrel
Accept-Ranges: bytes
ETag: "1dd2a316824f69f"
Last-Modified: Wed, 12 Aug 2026 08:05:58 GMT
x-ms-request-id: 857e7125-116c-497a-a30a-937034d516c6
x-ms-correlation-request-id: 857e7125-116c-497a-a30a-937034d516c6



```
```sh
curl -s http://localhost:8483/kv  -H "Accept: application/json" | jq '.'
```
```json
{
  "items": [
    {
      "etag": "Um2t31kB6kzEV3cwuum2WQ",
      "key": "Message",
      "label": null,
      "content_type": null,
      "value": "Hello world!",
      "tags": {},
      "locked": false,
      "last_modified": "2026-11-03T00:55:43.6308498+00:00"
    }
  ]
}
```
```sh
curl -s http://127.0.0.1:8483/health
```
```text
Healthy
```

### Examples

> NOTE: only c# example [Program.cs](https://github.com/Azure/AppConfiguration-Emulator/blob/main/examples/dotnet-sdk/Demo/Program.cs), 
JS example [demo.mjs](https://github.com/Azure/AppConfiguration-Emulator/blob/main/examples/javascript-sdk/demo.mjs) and GoLang example
[main.go](https://github.com/Azure/AppConfiguration-Emulator/blob/main/examples/go-sdk/main.go) are available.

### See Also

  * [Azure App Configuration emulator overview](https://learn.microsoft.com/en-us/azure/azure-app-configuration/emulator-overview?tabs=docker)
  * [Azure App Configuration ConfigMap and Secret](https://github.com/Azure/AppConfiguration-KubernetesProvider). NOTE: example install command:
```sh
helm install azureappconfiguration.kubernetesprovider oci://mcr.microsoft.com/azure-app-configuration/helmchart/kubernetes-provider --namespace azappconfig-system --create-namespace
```
  * [Azure App Configuration Kubernetes Provider](https://mcr.microsoft.com/artifact/mar/azure-app-configuration/kubernetes-provider/about)
  * [Questions, feedback and samples for Azure App Configuration service](https://github.com/Azure/AppConfiguration) 
  * https://github.com/Azure/AppConfiguration-DotnetProvider
  * .NET Feature Flags with Azure: Everything You Need to Know - Mohamad Dbouk https://www.youtube.com/watch?v=z6LNrMtJa6Y
  * https://github.com/choonkeun/AppConfigFeature-Filter
  * https://github.com/RemcoVisserVitas/AppConfiguration
  * https://github.com/cezapata/appconfiguration-sample java (very basic)
  * `az appconfig` [CLI Examples](https://learn.microsoft.com/en-us/cli/azure/appconfig?view=azure-cli-latest)
  

NOTE:  No tags found for 1.8 in https://hub.docker.com/r/alpine/java
