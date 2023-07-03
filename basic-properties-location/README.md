### Info

### Usage
```sh
mvn package
```
```sh
docker build -f Dockerfile -t basic-location .
```
* run in container (attached)
```sh
docker run -p 8085:8085 basic-location
```
* test 
```sh
curl -s http://192.168.99.100:8085/check
```
Key file path: /work/place/../../root/.keys/key.txt
Key file path: /work/place/../../root/.keys/key.txt
Key file path: /root/.keys/key.txt
Data: 123
```
### NOTE:

* special property

`spring.config.location`

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
