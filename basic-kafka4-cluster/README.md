### Info

Attempt to  constuct minimal kafka cluster demo environment on Docker (w/o kuberbetes/ minikube)

it has to contain /  exhibit

* Kafka v4 with redundancy (multi-broker, Zookeeper-free)

* Partitioned topic

* plain Java-based producer and consumer/processor
 
* Ultra-minimal base images

requirements to be hosted on
* Docker Toolbox compatibility (no Kubernetes)
which is installed on an Windows host with  16 FB RAM therefore the container  machine is limited to
Fit in <16GB RAM + <16GB disk dynamic


NOTE: latest kafka client jar compatible with java 8 is 3.5.1:

```xml
<dependency>
  <groupId>org.apache.kafka</groupId>
  <artifactId>kafka-clients</artifactId>
  <version>3.5.1</version> <!-- or 3.4.1 -->
</dependency>

```
