### Info

Ths directory contains replica of [orangootan/gitlab-alpine-docker-image](https://github.com/ring0club/gitlab-alpine-docker-image) gitlab single node installation compiled from source gitlab running on alpine 3.9.5

### Usage
* build server

```sh
docker build -t gitlab-server -f Dockerfile.server .
```
* build agent

```sh
docker build -t gitlab-agent -f Dockerfile.agent .
```
this will give the following image sizes 

```text

java-executor                                      latest              248f672cf045        6 seconds ago       84.9MB
gitlab-runner                                      latest              e148ee2e88b5        38 minutes ago      72.4MB

```
as opposed to vendor ce 
```text
REPOSITORY                                         TAG                 IMAGE ID            CREATED             SIZE
gitlab/gitlab-runner                               latest              05cd232de647        3 days ago          335MB
gitlab/gitlab-ce                                   latest              5d8154a38693        5 days ago          3.66GB
java-runner                                        latest              c6ae0bef0e95        5 minutes ago       629MB
eclipse-temurin                                    11-jdk-alpine       a212a1b08af9        2 months ago        305MB
```

* build runner


```sh
docker exec -it gitlab grep 'Password:' /etc/gitlab/initial_root_password
```
### See Also
  * https://docs.gitlab.com/runner/install/docker.html

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
