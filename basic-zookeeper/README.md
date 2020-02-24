# https://www.baeldung.com/spring-cloud-zookeeper 
- the referenced github project ([spring-cloud/spring-cloud-zookeeper](https://github.com/eugenp/tutorials/tree/master/spring-cloud/spring-cloud-zookeeper)) has deep parent project chain - making it difficult too extract

# `Dockerfile` based on [jplock/zookeeper](https://hub.docker.com/r/jplock/zookeeper/dockerfile/). A cleaner alternative may be the [digital-wonderland/docker-zookeeper](https://github.com/digital-wonderland/docker-zookeeper/blob/master/Dockerfile)

# https://github.com/mkrcah/virtual-zookeeper-cluster - disadvantage: bundles ansible just to install zookeeper	where shell presumably perfectly do.

[zhouhao96/ZookeperStudy](https://github.com/zhouhao96/ZookeperStudy)
```sh
docker build -f Dockerfile -t basic-zookeeper . 
docker run -p 8086:8085 basic-zookeeper
```
