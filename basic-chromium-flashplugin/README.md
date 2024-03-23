###  Info

replica of [Docker Ubuntu with Chromium and Adobe Flas pluginh](https://github.com/dmitrmax/flash-in-docker)
also covered in [post](https://habr.com/ru/articles/802363/) in Russian by project author

### Usage
* build 

```sh
IMAGE=chromium-flashplugin
docker build -f Dockerfile -t $IMAGE .
```
* observe the image size is quite heavy:
```sh
docker images $IMAGE
```
```text
REPOSITORY             TAG       IMAGE ID       CREATED         SIZE
chromium-flashplugin   latest    fb0f230f71b8   6 minutes ago   601MB
```
* run container
```sh
docker run -it --user user:user -v /tmp/.X11-unix/:/tmp/.X11-unix  --cap-add SYS_ADMIN $IMAGE
```

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-chromium-flashplugin/screenshots/capture-browser.png)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
