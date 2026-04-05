### Info

[yt-dlp](https://github.com/yt-dlp/yt-dlp) is a fork of youtube-dl.

### Usage

* check [docker hub](https://hub.docker.com/r/jauderho/yt-dlp) for updates but uness it is failing can pin

```sh
TAG=latest
TAG=2026.03.17
docker pull  jauderho/yt-dlp:$TAG
docker run --rm - jauderho/yt-dlp:$TAG
```

```sh
TAG=2026.03.17
NAME=ytdlp
docker run --name $NAME --entrypoint='' -it jauderho/yt-dlp:$TAG sh
```
in the container
```sh
URL=https://www.youtube.com/watch?v=ZlLrqsnwPHM
yt-dlp -x --audio-format mp3 --audio-quality 320K "$URL"
```
> NOTE when rerun, may fail do detect the earlier (un)finished download and start over

```sh
NAME=ytdlp
docker exec -it $NAME sh -c "ls /downloads/*.mp3"
```

```sh
NAME=ytdlp
docker cp $NAME:/downloads .
```
```text
Successfully copied 209MB to .
```

> NOTE: the destination directory will be world read,write, execute

### Cleanup

```sh
NAME=ytdlp
docker stop $NAME
docker container prune -f
docker volume prune -f
docker image prune -f
```

