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
curl -s http://localhost:8080/dummy/index.html
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
curl -s http://localhost:8080/manual/index.html
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
curl -s http://localhost:8080/
```
```html
<?xml version="1.0"?>
<html>
  <body>
    <h2>Dummy</h2>
  </body>
</html>
```
The `index.html` can be omitted in any of the above commands
### Cleanup

* stop and remove container
```sh
docker container ls | grep $IMAGE | awk '{print $1}' | xargs -IX docker rm -f X
```
a longer version

```sh
docker container ls | grep $IMAGE | awk '{print $1}' | xargs -i sh -c 'docker stop {};docker rm {}'
```
### See Also

* [multiple commands with xargs](https://stackoverflow.com/questions/6958689/running-multiple-commands-with-xargs)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


