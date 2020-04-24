### Testing
* build the docker container by invoking maven plugin
```sh
IMAGE=app
mvn install
```
* inspect the image
```sh
docker run -it $IMAGE sh
```
* compose the cluster
```sh
cd src/main/docker
export COMPOSE_HTTP_TIMEOUT=300
docker-compose up -d 
```
### Troubleshooting

The maven plugin imposes a "virtual" file layout in `Dockerfile`  making is useless for a standalone docker build. To workaround this, do
```
cd src/main/docker
mkdir maven
cp 
cp
```
### Cleanup


