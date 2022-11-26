## Info 

* basic [python3 flask app](https://www.geeksforgeeks.org/flask-creating-first-simple-application/) in alpine Python container
showing the locale processing

 
### Usage

* build
```sh
export IMAGE=basic-python-flask
```
```sh
docker build -t $IMAGE  -f Dockerfile .
```
* run in background

```sh
export NAME=basic-python-flask
docker run --name $NAME -p 5000:5000 -d $IMAGE
```
* test localized static page
```sh
curl http://localhost:5000/
```
this will respond with
```text
Hello йцукен
```
test passing the URL - incorrectly
```sh
curl http://localhost:5000/hello/абырвалг
```
will show misencoded UTF-8 to WIN CP1251 text
```text
Hello DdegD+-NND2DdegD>>D
Hello Ð°Ð±ÑÑÐ²Ð°Ð»Ð³ [208, 176, 208, 177, 209, 139, 209, 128, 208, 178, 208, 176, 208, 187, 208, 179] 
```
using URL encoded argument localized string in the path
```sh
curl localhost:5000/hello/%D0%B0%D0%B1%D0%B2%0A
```
this will print
```text
Hello абв
 [1072, 1073, 1074, 10] абвгд [1072, 1073, 1074, 1075, 1076]!
```
in the browser it also url-encodes the url, from ubuntu 

![Ubuntu](https://github.com/sergueik/springboot_study/blob/master/basic-python-flask/screenshots/capture_url_chromium.png)

or windows

![windows](https://github.com/sergueik/springboot_study/blob/master/basic-python-flask/screenshots/capture_url_chrome.png)

### Note

`curl` allows URL-encoding POSTDATA body but not the URL by specifying `--data-urlencode` flag.

### See Also
  * https://qna.habr.com/q/1184600
  * https://progi.pro/nevozmozhno-prochitat-kirillicheskie-simvoli-v-url-adrese-brauzera-s-pomoshyu-flask-python-2590023

  * https://github.com/gliderlabs/docker-alpine/issues/144
  * https://github.com/rilian-la-te/musl-locales * https://grrr.tech/posts/2020/add-locales-to-alpine-linux-docker-image/ - complex story
`Dockerfile` showing [setting](https://gist.github.com/alextanhongpin/aa55c082a47b9a1b0060a12d85ae7923) up locale on alpine:3.6 image 
  * https://www.urlencoder.org

  * https://github.com/gliderlabs/docker-alpine/issues/144#issuecomment-436455850
  * [encoding](https://stackoverflow.com/questions/24234987/urlencode-cyrillic-characters-in-python) the UTF-8 console cyrillic as urlencoded Windows `cp1251` locale

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


