### Info

replica of [Dynatrace-AppMon-Docker](https://github.com/Dynatrace/Dynatrace-AppMon-Docker)
Dockerized components of the Dynatrace Application Monitoring enterprise solution

pulling the image from [rbrandstaedter/dockerized-server-with-hybris-fastpack](https://hub.docker.com/r/rbrandstaedter/dockerized-server-with-hybris-fastpack/tags)
 - image with __DynaTrace__ __Server__/__Collector__ ready to monitor hybris eCommerce platforms
by Rene Brandstaedter


NOTE: need to use an older Docker Tool Suite - the 
error from __Docker__ version __29.1.3__ or later is:
```text
Error response from daemon: unsupported manifest media type and no default available: application/vnd.docker.distribution.manifest.v1+prettyjws
```
The error is mentioned in [here](https://github.com/docker/cli/issues/1724). It is
 that `application/vnd.docker.distribution.manifest.v1+prettyjws`, 
 you must either upgrade the image to a newer manifest schema (a.k.a. __V2__)
 or use a __Docker__ client that still supports the legacy __V1__ format. 
 This error occurs because modern __Docker__ versions and container 
 tools have dropped support for the outdated __Schema 1__ manifest format in favor of
 __Schema 2__ or __OCI__-compliant formats.
Luckily no __DIND__  is needed - the __Docker ToolBox__ (__Docker__ version __19.03.1__) fits
```sh
docker pull rbrandstaedter/dockerized-server-with-hybris-fastpack:latest
```
### Usage

```sh
export COMPOSE_PROJECT_NAME=dynatracedocker
export DT_HOME="/opt/dynatrace"
export DT_SERVER_NAME="dtserver"
export DT_SERVER_LICENSE_KEY_FILE_URL=""
export VERSION="7.0"
export BUILD_VERSION="7.0.0.2469"
docker-compose up --build
```
```
export INSTALLER_FILE_NAME=dynatrace-full-${BUILD_VERSION}-linux-x86-64.jar
export INSTALLER_URL=https://files.dynatrace.com/downloads/OnPrem/dynaTrace/${VERSION}/${BUILD_VERSION}/${INSTALLER_FILE_NAME}
curl -skLI ${INSTALLER_URL} 
```

```text
HTTP/2 403 
content-type: text/html
content-length: 1765
date: Sun, 03 May 2026 23:05:44 GMT
last-modified: Tue, 12 Dec 2023 15:09:14 GMT
etag: "0505a012a201f88ef6690cafff9e2299"
x-amz-server-side-encryption: AES256
x-amz-version-id: .Nry6v99TVwI3uZ_AAR_lCUwkXGxrb8z
accept-ranges: bytes
server: AmazonS3
x-cache: Error from cloudfront
via: 1.1 e315040cdcb6abeba8d4edc55f4280f6.cloudfront.net (CloudFront)
x-amz-cf-pop: MIA50-P6
x-amz-cf-id: L8G2-qhnwRDidUk3m31uH6YIzgPgNYxCWN3R3MlWjK76xeh48iUqUA==
age: 948

```
in fact the `https://files.dynatrace.com/downloads/OnPrem/` is `403` __Access Denied__

### Background Info
[Dynatrace Brings Self-Monitoring Dashboards to Dynatrace Managed Users](https://solutionsreview.com/network-monitoring/dynatrace-brings-self-monitoring-dashboards-to-dynatrace-managed-users/)
NOTE :

The Dynatrace OneAgent [Docker image](https://hub.docker.com/r/dynatrace/oneagent)
is a [non-immutable container]() image that downloads Dynatrace OneAgent binaries 
for full-stack and container monitoring.

The Dynatrace OneAgent in turn, in combination with Dynatrace code modules,
to collect all observability info about subject app into [Dynatrace Managed](https://docs.dynatrace.com/managed)
	
	
One can also istall [Dynatrace Managed Cluster](https://docs.dynatrace.com/managed/managed-cluster)
Note: demanding (hosts must have at least 32 GB RAM.) 
Appears to run ElasticSeatch inder the hood
https://docs.dynatrace.com/managed/managed-cluster/installation	

Dynatrace provisions your Managed entitlement, they send an email containing something like:

wget -O dynatrace-managed-1.xx.x.sh "https://<signed-temporary-url>"
wget -O dynatrace-managed-1.xx.x.sh.sig "https://<signed-temporary-url>"

That exact command is what the docs refer to.

Important details:

it is usually time-bound / signed
often tied to your customer environment
includes both:
installer shell script
.sig signature file
can be downloaded on any Linux box, then copied elsewhere

So unlike public OSS artifacts, there is no permanent Artifactory-like public wget endpoint.

For Managed, current guidance is:

contact Dynatrace Sales / solution engineer for a Managed POC entitlement

Recommends the normal SaaS free trial is much easier
0

https://en.wikipedia.org/wiki/Dynatrace
https://en.wikipedia.org/wiki/AppDynamics
https://en.wikipedia.org/wiki/HP_OpenView

Agent is free docker hub image, maybe Helm chart (daemonset)
The windows agent is called `Dynatrace-OneAgent-Windows.exe` - may not be free). What is hard to find standalone is called __Dynatrace Cluster__


The __Dynatrace Cluster__, specifically __Dynatrace Managed__, can be deployed on __VirtualBox__ using standard __Linux__ server images (e.g., __Ubuntu__/__CentOS__) as the underlying host, but no official "pre-built VirtualBox image" is publicly available. Users typically asked to create a blank VM, install Linux, and run the __Dynatrace Managed__ installer on it

There is also an __Dynatrace Classic Environment__ but again it is a client ( an node.js [package](https://developer.dynatrace.com/develop/sdks/client-classic-environment-v1/) which must be configured with the address of the dynatrace servert before being useful).


### See Also 
  * https://hub.docker.com/r/mcp/dynatrace-mcp-server
  * https://hub.docker.com/mcp/server/dynatrace-mcp-server/overview
  * https://www.dynatrace.com/hub/detail/docker/	 
  * [Microsoft Dynatrace](https://learn.microsoft.com/en-us/azure/partner-solutions/dynatrace/overview)
  * [Dynatrace Overview](https://habr.com/ru/articles/949106/) (in Russian, disguised promotional)

  *  __Dynatrace__ __Self-Monitoring__ a.k.a. __Managed__[landing page](https://www.dynatrace.com/hub/detail/dynatrace-self-monitoring-managed/). NOTE: "Start Free Trial" is redirecting to sign up page with the "Something went wrong Please try again later" after filling personal data.
  * [WebCatalog Desktop](https://webcatalog.io/en/desktop) Turn websites into desktop apps:  "Installs" Wikipedia, ArchWiki, and other wiki sites as standalone nodejs / Electron apps. written in JavaScript, utilizing the Electron framework and Chromium to turn web applications into native desktop apps. [project repo](https://github.com/resilientred/webcatalog). Warning!  WebCatalog - Run Web Apps Like Real Apps = Essentially Managed Dynatrace, inversed. 
  * https://www.versio.io/en/product-release-end-of-life-eol-dynatrace-dynatrace-managed.html 
  * [Dynatrace University](https://university.dynatrace.com/learn)

### Troubleshooting:
```text
Executing java-common-0.1-r0.trigger
OK: 81 MiB in 42 packages
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  1765  100  1765    0     0    553      0  0:00:03  0:00:03 --:--:--   553
Error: Invalid or corrupt jarfile /tmp/dynatrace-full-7.0.0.2469-linux-x86-64.jar

```
