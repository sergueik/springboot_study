### Info


This directory contains 
with original multi-stage Dokcerfile split into build and run pieces and working directory on the host used to transfer the artifacts.


### Usage

* pull the project source
```sh
mkdir -p tmp
git clone https://github.com/mrichman/hargo tmp
rm -fr tmp/.git
```
* pull the basic images
```sh
docker pull golang:alpine3.9
docker build -t basic-hargo.build -f Dockerfile.build tmp
docker run --name build -d basic-hargo.build
```

```sh
docker container ls -a
```
```sh
docker cp builder:/go/src/hargo/hargo . 
docker cp builder:/etc/ssl/certs/ca-certificates.crt . 
```
```sh
docker build -t basic-hargo -f Dockerfile.run .
docker run --rm --name hargo -it basic-hargo hargo --help
docker run --rm --name hargo -it  -v $(pwd)/tmp/test:/test basic-hargo hargo run /test/golang.org.har
```
this wil produce the console report:
```sh
INFO[0000] run .har file: /test/golang.org.har
[GET,200] URL: https://golang.org/
[GET,200] URL: https://fonts.googleapis.com/css?family=Work+Sans:600|Roboto:400,700
[GET,200] URL: https://golang.org/lib/godoc/style.css
[GET,200] URL: https://golang.org/lib/godoc/jquery.js
[GET,200] URL: https://fonts.googleapis.com/css?family=Product+Sans&text=Supported%20by%20Google&display=swap
[GET,200] URL: https://golang.org/lib/godoc/images/footer-gopher.jpg
[GET,200] URL: https://golang.org/lib/godoc/playground.js
[GET,200] URL: https://golang.org/lib/godoc/godocs.js
[GET,200] URL: https://golang.org/lib/godoc/images/go-logo-blue.svg
[GET,200] URL: https://golang.org/lib/godoc/images/cloud-download.svg
[GET,200] URL: https://golang.org/lib/godoc/images/home-gopher.png
[GET,200] URL: https://golang.org/lib/godoc/images/play-link.svg
[GET,200] URL: https://www.youtube.com/embed/cQ7STILAS0M
[GET,200] URL: https://fonts.gstatic.com/s/worksans/v5/QGYpz_wNahGAdqQ43Rh3o4T8mNhNy_r-Kw.woff2
[GET,200] URL: https://ssl.google-analytics.com/ga.js
[GET,200] URL: https://fonts.gstatic.com/l/font?kit=pxiDypQkot1TnFhsFMOfGShlFd2JQEl2pE7v9ZAr7BbFrlEMpMTt&skey=b7d9d887ed217aec&v=v10
[GET,200] URL: https://blog.golang.org/.json?jsonp=feedLoaded&_=1567424947760
[GET,200] URL: https://ssl.google-analytics.com/r/__utm.gif?utmwv=5.7.2&utms=1&utmn=1256598427&utmhn=golang.org&utmcs=UTF-8&utmsr=1920x1080&utmvp=1905x417&utmsc=24-bit&utmul=en-us&utmje=0&utmfl=-&utmdt=The%20Go%20Programming%20Language&utmhid=824022456&utmr=-&utmp=%2F&utmht=1567424948526&utmac=UA-11222381-2&utmcc=__utma%3D110886291.1318786886.1567424948.1567424948.1567424948.1%3B%2B__utmz%3D110886291.1567424948.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B&utmjid=912966273&utmredir=1&utmu=qACAAAAAAAAAAAAAAAAAAAAE~
[GET,200] URL: https://www.youtube.com/yts/cssbin/www-player-webp-vflVj-aXG.css
[GET,200] URL: https://www.youtube.com/yts/jsbin/www-embed-player-vflljwD9V/www-embed-player.js
[GET,200] URL: https://www.youtube.com/yts/jsbin/player_ias-vflt4leIo/en_US/base.js
[GET,200] URL: https://googleads.g.doubleclick.net/pagead/id
[GET,200] URL: https://www.google.com/js/bg/3-hpRAd0_wuB6laQqaJG0uu5agxtfADTbxmN8ntEYS8.js
[GET,200] URL: https://static.doubleclick.net/instream/ad_status.js
[GET,200] URL: https://stats.g.doubleclick.net/r/collect?v=1&aip=1&t=dc&_r=3&tid=UA-11222381-2&cid=1318786886.1567424948&jid=912966273&_v=5.7.2&z=1256598427
[GET,200] URL: https://www.youtube.com/yts/jsbin/player_ias-vflt4leIo/en_US/remote.js
[GET,200] URL: https://googleads.g.doubleclick.net/pagead/id?slf_rd=1
[GET,200] URL: https://yt3.ggpht.com/-8qdO4Xl6jAU/AAAAAAAAAAI/AAAAAAAAAAA/ngjQPxBLXjk/s68-c-k-no-mo-rj-c0xffffff/photo.jpg
[GET,404] URL: https://i.ytimg.com/vi/cQ7STILAS0M/sddefault.jpg
[GET,204] URL: https://www.youtube.com/generate_204?i9yLwg
[GET,200] URL: https://golang.org/favicon.ico
[POST,200] URL: https://www.youtube.com/youtubei/v1/log_event?alt=json&key=AIzaSyAO_FJ2SlqU8Q4STEHLGCilw_Y9_11qcW8
sergueik@sergueik71:~/src/springboot_study/hargo$
sergueik@sergueik71:~/src/springboot_study/hargo$ docker run --rm --name hargo -it  -v $(pwd)/tmp/test:/test basic-hargo hargo run /test/golang.org.har
INFO[0000] run .har file: /test/golang.org.har
[GET,200] URL: https://golang.org/
[GET,200] URL: https://fonts.googleapis.com/css?family=Work+Sans:600|Roboto:400,700
[GET,200] URL: https://golang.org/lib/godoc/style.css
[GET,200] URL: https://golang.org/lib/godoc/jquery.js
[GET,200] URL: https://fonts.googleapis.com/css?family=Product+Sans&text=Supported%20by%20Google&display=swap
[GET,200] URL: https://golang.org/lib/godoc/images/footer-gopher.jpg
[GET,200] URL: https://golang.org/lib/godoc/playground.js
[GET,200] URL: https://golang.org/lib/godoc/godocs.js
[GET,200] URL: https://golang.org/lib/godoc/images/go-logo-blue.svg
[GET,200] URL: https://golang.org/lib/godoc/images/cloud-download.svg
[GET,200] URL: https://golang.org/lib/godoc/images/home-gopher.png
[GET,200] URL: https://golang.org/lib/godoc/images/play-link.svg
[GET,200] URL: https://www.youtube.com/embed/cQ7STILAS0M
[GET,200] URL: https://fonts.gstatic.com/s/worksans/v5/QGYpz_wNahGAdqQ43Rh3o4T8mNhNy_r-Kw.woff2
[GET,200] URL: https://ssl.google-analytics.com/ga.js
[GET,200] URL: https://fonts.gstatic.com/l/font?kit=pxiDypQkot1TnFhsFMOfGShlFd2JQEl2pE7v9ZAr7BbFrlEMpMTt&skey=b7d9d887ed217aec&v=v10
[GET,200] URL: https://blog.golang.org/.json?jsonp=feedLoaded&_=1567424947760
[GET,200] URL: https://ssl.google-analytics.com/r/__utm.gif?utmwv=5.7.2&utms=1&utmn=1256598427&utmhn=golang.org&utmcs=UTF-8&utmsr=1920x1080&utmvp=1905x417&utmsc=24-bit&utmul=en-us&utmje=0&utmfl=-&utmdt=The%20Go%20Programming%20Language&utmhid=824022456&utmr=-&utmp=%2F&utmht=1567424948526&utmac=UA-11222381-2&utmcc=__utma%3D110886291.1318786886.1567424948.1567424948.1567424948.1%3B%2B__utmz%3D110886291.1567424948.1.1.utmcsr%3D(direct)%7Cutmccn%3D(direct)%7Cutmcmd%3D(none)%3B&utmjid=912966273&utmredir=1&utmu=qACAAAAAAAAAAAAAAAAAAAAE~
[GET,200] URL: https://www.youtube.com/yts/cssbin/www-player-webp-vflVj-aXG.css
[GET,200] URL: https://www.youtube.com/yts/jsbin/www-embed-player-vflljwD9V/www-embed-player.js
[GET,200] URL: https://www.youtube.com/yts/jsbin/player_ias-vflt4leIo/en_US/base.js
[GET,200] URL: https://googleads.g.doubleclick.net/pagead/id
[GET,200] URL: https://www.google.com/js/bg/3-hpRAd0_wuB6laQqaJG0uu5agxtfADTbxmN8ntEYS8.js
[GET,200] URL: https://static.doubleclick.net/instream/ad_status.js
[GET,200] URL: https://stats.g.doubleclick.net/r/collect?v=1&aip=1&t=dc&_r=3&tid=UA-11222381-2&cid=1318786886.1567424948&jid=912966273&_v=5.7.2&z=1256598427
[GET,200] URL: https://www.youtube.com/yts/jsbin/player_ias-vflt4leIo/en_US/remote.js
[GET,200] URL: https://googleads.g.doubleclick.net/pagead/id?slf_rd=1
[GET,200] URL: https://yt3.ggpht.com/-8qdO4Xl6jAU/AAAAAAAAAAI/AAAAAAAAAAA/ngjQPxBLXjk/s68-c-k-no-mo-rj-c0xffffff/photo.jpg
[GET,404] URL: https://i.ytimg.com/vi/cQ7STILAS0M/sddefault.jpg
[GET,204] URL: https://www.youtube.com/generate_204?i9yLwg
[GET,200] URL: https://golang.org/favicon.ico
[POST,200] URL: https://www.youtube.com/youtubei/v1/log_event?alt=json&key=AIzaSyAO_FJ2SlqU8Q4STEHLGCilw_Y9_11qcW8

```
### Cleanup
```sh
rm -fr tmp
```
