### Info

This directory contins a ultra basic static html file put into tomcat docker container

### Testing

```sh
mvn clean package
```

```sh
IMAGE=tomcat-static
docker build -t $IMAGE -f Dockerfile .
docker run -d -p 8080:8080 $IMAGE
```
* confirm the app deployed and handles the request:
```sh
curl http://localhost:8080/dummy/index.html
```
```html
<?xml version="1.0"?>
<html>
  <body>
    <h2>Dummy</h2>
  </body>
</html>
```

```sh
curl http://localhost:8080/manual/index.html
```
```html
<?xml version="1.0"?>
<html>
  <body>
    <h2>Dummy</h2>
  </body>
</html>
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


