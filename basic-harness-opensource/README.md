### info

This directory contains the install of __Harness Open Source__ *an all-in-one platform that ...*  - It is not just the __Harness Delegate__, actually

### Background

At its core, Harness Open Source Gitspaces appears to be a clear example of commercialization / platformization:

  * a heavily vendor-branded, plain container-based development environment built on a domain-specific Docker image, containing:
  * a checked-out Git workspace,
  * Microsoft VS Code Server (remote) preinstalled,
  * selected language SDKs and tooling.

Under the hood, Harness Open Source functions primarily as a controller layer that interacts with a locally available Docker daemon via the standard Unix socket to:

  * invoke core Docker APIs
  * create, start, and stop containers,
  * mount volumes for workspace persistence.

The system then exposes [VS Code Remote Server](https://code.visualstudio.com/docs/remote/remote-overview) through a browser interface, communicating over HTTP/WebSocket. The user experience closely mirrors that of a locally hosted Visual Studio Code instance. The actual remote editor / debuggerfunctionality is provided by Microsoft rather than implemented by Harness Open Source

Functionally equivalent containerized VS Code Server environments (minus the vendor branding and orchestration layer) are widely available, including numerous prebuilt images [published on Docker Hub](https://hub.docker.com/search?q=vscode-server).

In addition to the containerized development environment itself, Harness Open Source provides an orchestration layer offering:

  * workspace lifecycle management
  * authentication and identity
  * repository binding
  * UI routing and access control
  * secret and repository configuration

Taken together, Harness Open Source primarily delivers centralized orchestration, integration, and management over otherwise commoditized container-based development tooling, packaged and presented as a unified platform.

---
### Usage
```sh
TAG='3.2.0'
TAG='3.0.1'
docker pull harness/harness:$TAG
```
it will attempt to access all other containers run locally
```sh
TAG='3.2.0'
TAG='3.0.1'
docker run -d \
  -p 3000:3000 -p 3022:3022 \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /tmp/harness:/data \
  --name opensource \
  --restart always \
  harness/harness:$TAG
```
add repository. For pipeline, import yaml from maven [sample](https://developer.harness.io/docs/open-source/pipelines/samples/maven)
but use a smaller image `openjdk:8-jdk-alpine3.9`

![Create Repository](screenshots/capture-import-repository.png)

![Create Pipeline](screenshots/capture-create-pipeline.png)

If seeing the error
```text
Error response from daemon: client version 1.41 is too new. Maximum supported API version is 1.40
```
downgrade to `3.0.1`

NOTE: aftrer recycling the container, the already made changes are still present:

![Run Pipeline](screenshots/capture-pipeline-run.png)

```sh
$ docker-machine ssh
```
```text
   ( '>')
  /) TC (\   Core is distributed with ABSOLUTELY NO WARRANTY.
 (/-_--_-\)           www.tinycorelinux.net
```
```sh
ls /tmp/harness
```
```text
cleanup          database.sqlite  repos            shared_temp
```
to move to another machine

```sh
mkdir /tmp/harness
cp -R ~/harness/ /tmp/harness
sudo chown -R root:root /tmp/harness/
```

> NOTE, some basic functionality is broken in the default configuration, e.g. [Haeness Open Source Gitspaces](https://developer.harness.io/docs/category/gitspaces)

![Attempt Gitspaces](screenshots/capture-gitspaces.png)

```text
Failed to load resource: http://192.168.99.100:3000/api/v1/spaces/test/+/gitspaces?page=1&limit=20 the server responded with a status of 500 (Internal Server Error)
```

A latest version `3.20` of `open source harness` does not show an error, and is able to proceed to the next step pulling the `mcr.microsoft.com/devcontainers/base:dev-ubuntu-24.04` to run VS Code Server from there.

---
### See Also

  * Harness Open Source [documentation](https://developer.harness.io/docs/open-source)
  * __Harness Open Source__ Docker hub[images](https://hub.docker.com/r/harness/harness)
  * __Harness Delegate__ Docker hub[images](https://hub.docker.com/r/harness/delegate)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

