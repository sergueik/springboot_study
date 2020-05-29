### Info

This directory contains a replica of down-scaled [small Docker images for Selenium Grid](https://github.com/SUNx2YCH/docker-alpine-selenium) by Alexander Ivanovsky
temporarily unbundled from the resource-stressful `docker-compose.yml`

### Usage

* download the dependency jar

```sh
curl -o data.xml  http://selenium-release.storage.googleapis.com/

RELEASE_TAG=3.14.0
RELEASE_TAG_SHORT=$(echo $RELEASE_TAG | sed 's|\.[0-9][0-9]*$||')
xmllint --xpath "//*[local-name()='Key'][contains(text(),'${RELEASE_TAG_SHORT}')]" data.xml 
SELENIUM_JAR_URL=http://selenium-release.storage.googleapis.com/$RELEASE_TAG_SHORT/selenium-server-standalone-${RELEASE_TAG}.jar
SELENIUM_JAR=selenium-server-standalone.jar
wget -O $SELENIUM_JAR ${SELENIUM_JAR_URL}
```

*  build hub Docker image and run it
```sh
export IMAGE='basic-hub'
export NAME='example-hub'
docker container stop $NAME
docker container rm $NAME
docker image rm $IMAGE
docker build -t $IMAGE -f Dockerfile.$IMAGE .
docker run -e "SELENIUM_JAR=selenium-server-standalone.jar" -e "SELENIUM_HUB_PORT=4444" --name $NAME -p 4444:4444 -d $IMAGE
docker ps
```

* check the health of hub
```sh
docker logs $NAME
```
will display
```sh
Selenium Grid hub is up and running
21:06:29.063 INFO [Hub.start] - Nodes should register to http://172.17.0.2:4444/grid/register/
21:06:29.063 INFO [Hub.start] - Clients should connect to http://172.17.0.2:4444/wd/hub
```
* follow with browser page check
```sh
curl localhost:4444 2>/dev/null |  grep Happy
```
which will produce
```sh
Happy Testing!
```
and
```sh
docker exec -it $NAME sh
```
if needed to debug
### Cleanup
```sh
export IMAGE='basic-hub'
export NAME='example-hub'
docker container stop $NAME
docker container rm $NAME
docker image rm $IMAGE
```
### See Also
  * selenium + Chrome [Dockerfile project](https://github.com/leafney/alpine-selenium-chrome/blob/master/Dockerfile) geared for Python
  * http://selenium-release.storage.googleapis.com/2.53/selenium-server-standalone-2.53.1.jar
