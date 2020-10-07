### Info

This directory contains standard Docker example of creating the container with
specific non-root user and mound a writable volume

### Usage


```sh
TAG=basic_user
_UID=$(id -u)
sed -i "s|UID=.*|UID=$_UID|g" Dockerfile
docker build -t $TAG -f Dockerfile .
mkdir logs
chmod 777 logs
docker run -it $TAG -v $(pwd)/logs:/home/muyser/logs:rw
```
the issue that files created in the docker session with non-root default user
are not visible from the host - is still observed
#### Note
Can also use the (readonly) environment `UID` on the host user login session
#### Note
modifying the `UID` though run argument way will not have intended effect

in a sibling shell inspect the home directory of the default user:
```sh
docker exec -it $(docker container ls | grep "$TAG" | cut -f 1 -d' ') sh
```

### See Also

 * original "coding interview"-grade [discussion](https://www.cyberforum.ru/shell/thread2707382.html) (in Russian)
 * https://stackoverflow.com/questions/34031397/running-docker-on-ubuntu-mounted-host-volume-is-not-writable-from-container

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
