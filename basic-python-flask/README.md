## Info 

* basic [python3 flask app](https://www.geeksforgeeks.org/flask-creating-first-simple-application/) in alpine Python container
showing the locale processing and an example from 
__Build a Swagger UI for your Python Flask Application__ [post](https://code.likeagirl.io/swagger-and-postman-build-a-swagger-ui-for-your-python-flask-application-141bb4d0c203)

### Usage

* build
```sh
export IMAGE=basic-python-flask
```
```sh
docker build -t $IMAGE -f Dockerfile . --progress=plain
```
* run in background

```sh
export NAME=basic-python-flask
docker run --name $NAME -p 5000:5000 -d $IMAGE
```

* test connection
on Linux
```sh
sudo netstat -antp | grep 5000
```
```text
tcp        0      0 0.0.0.0:5000            0.0.0.0:*               LISTEN      3957/docker-proxy
tcp6       0      0 :::5000                 :::*                    LISTEN      3964/docker-proxy
```

```sh
nc -v 127.0.0.1 5000
```
```text
Connection to 127.0.0.1 5000 port [tcp/*] succeeded!
```
enter the commmand followed with double carriage return
```sh
GET / HTTP/1.1

```

```text
HTTP/1.1 302 FOUND
Server: Werkzeug/3.0.1 Python/3.8.2
Date: Tue, 02 Jan 2024 15:55:56 GMT
Content-Type: text/html; charset=utf-8
Content-Length: 199
Location: /hello
Access-Control-Allow-Origin: *
Connection: close

<!doctype html>
<html lang=en>
<title>Redirecting...</title>
<h1>Redirecting...</h1>
<p>You should be redirected automatically to the target URL: <a href="/hello">/hello</a>. If not, click the link.
```

on Windows 8.x when using Docker Toolbox
```sh
docker-machine ip
```
```text
192.168.99.100
```
```powershelll
test-netconnection -computername 192.168.99.100 -port 5000
```
```text
ComputerName           : 192.168.99.100
RemoteAddress          : 192.168.99.100
RemotePort             : 5000
InterfaceAlias         : VirtualBox Host-Only Network #2
SourceAddress          : 192.168.99.1
PingSucceeded          : True
PingReplyDetails (RTT) : 0 ms
TcpTestSucceeded       : True
```

* install ncat.exe fron [nmap download page](https://nmap.org/download.html)

![Install ncat](https://github.com/sergueik/springboot_study/blob/master/basic-python-flask/screenshots/capture-nmap-windows.png)


```cmd
PATH=%PATH%;c:\Program Files\Nmap
ncat.exe 192.168.99.100 5000
```
```text
Ncat: Version 7.94 ( https://nmap.org/ncat )
Ncat: Connected to 192.168.99.100:5000.
```
enter the commmand followed with double carriage return
```text
GET / HTTP/1.1

```
```text
HTTP/1.1 302 FOUND
Server: Werkzeug/3.0.1 Python/3.8.2
Date: Tue, 02 Jan 2024 20:26:41 GMT
Content-Type: text/html; charset=utf-8
Content-Length: 199
Location: /hello
Access-Control-Allow-Origin: *
Connection: close

<!doctype html>
<html lang=en>
<title>Redirecting...</title>
<h1>Redirecting...</h1>
<p>You should be redirected automatically to the target URL: <a href="/hello">/hello</a>. If not, click the link.
```
* test Docker container running on Linux host from Windows host:
```cmd
PATH=%PATH%;c:\Program Files\Nmap
ncat.exe 192.168.0.92 5000
```
enter the commmand followed with double carriage return
```cmd
GET / HTTP/1.1


```
```text
HTTP/1.1 302 FOUND
Server: Werkzeug/3.0.1 Python/3.8.2
Date: Tue, 02 Jan 2024 19:56:59 GMT
Content-Type: text/html; charset=utf-8
Content-Length: 199
Location: /hello
Access-Control-Allow-Origin: *
Connection: close

<!doctype html>
<html lang=en>
<title>Redirecting...</title>
<h1>Redirecting...</h1>
<p>You should be redirected automatically to the target URL: <a href="/hello">/hello</a>. If not, click the link.
```
```cmd
GET /hello HTTP/1.1

```
```text
HTTP/1.1 200 OK
Server: Werkzeug/3.0.1 Python/3.8.2
Date: Tue, 02 Jan 2024 20:36:09 GMT
Content-Type: text/html; charset=utf-8
Content-Length: 18
Access-Control-Allow-Origin: *
Connection: close

Hello ╨╣╤å╤â╨║╨╡╨╜
```

```cmd
GET /hello/%D0%B0%D0%B1%D0%B2%0A HTTP/1.1

```
```text
HTTP/1.1 200 OK
Server: Werkzeug/3.0.1 Python/3.8.2
Date: Tue, 02 Jan 2024 20:37:47 GMT
Content-Type: text/html; charset=utf-8
Content-Length: 63
Access-Control-Allow-Origin: *
Connection: close

Hello ╨░╨▒╨▓
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

### Swagger UI

![Landing](https://github.com/sergueik/springboot_study/blob/master/basic-python-flask/screenshots/capture-swaggerui1.png)

![Call](https://github.com/sergueik/springboot_study/blob/master/basic-python-flask/screenshots/capture-swaggerui2.png)


NOTE: to prevent CORS error do not specify hostname in the `api_url` parameter of the `get_swaggerui_blueprint` method

### Cleanup

```sh
docker stop $NAME
docker rm $NAME
```

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

  * https://flask-restplus.readthedocs.io/en/stable/
   * https://code.likeagirl.io/swagger-and-postman-build-a-swagger-ui-for-your-python-flask-application-141bb4d0c203
   * https://stackoverflow.com/questions/25594893/how-to-enable-cors-in-flask
   * https://swagger.io/docs/open-source-tools/swagger-ui/usage/cors/ 
   * [Ncat for Windows](https://nmap.org/ncat/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


