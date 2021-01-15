### Info

This directory containes basic JSP Tomcat application packaged into war file.
### Usage

```sh
export IMAGE=basic-jsp
mvn package
```
```sh
docker container ls -a | grep $IMAGE| awk '{print $1}' | xargs docker container rm -f
```
```sh
docker build -t $IMAGE -f Dockerfile .
docker run --name $NAME -p 8080:8080 -d $IMAGE start
```
```
curl https://localhost:8080/demo
```

### See Also

 * demo [app](https://github.com/vborrego/jsp-example) with bean / handler integration

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

