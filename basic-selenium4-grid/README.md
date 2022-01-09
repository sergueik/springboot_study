### Info

this directory contains basic Python Selenium __4.x__ project on docker compose 2 container cluster (relatively heavy due to usage of Ubuntu instead of Alpine base images)

### Usage

```sh
docker pull python:3.8.12
docker pull selenium/standalone-chrome
```
for __3.x__ tests, also
```sh
docker pull selenium/standalone-chrome:3.141.59-vanadium
```
note, these are 300+ Mb / image and may be time consuming to pull
* Run selenium container in foreground
```sh
docker run --name selenium -p 4444:4444 selenium/standalone-chrome
```
* build a sample python client script container
```sh
docker build -t 'selenium-client' -f Dockerfile .
```
* run it launching browser in the first container
```
docker run --link selenium -it selenium-client sh
```

in the `selenium-client` container run Python program directly:
```sh
python3 main.py
```
this will print page source and take a screenshot of `https://www.wikipedia.org`

run through `docker-compose`
```
export COMPOSE_HTTP_TIMEOUT=600
docker-compose -f docker-compose.yaml up --build
```
note this will keep the `docker-compose` cluster `basic-selenium4-grid` running, to make it finish aftet the test returns 
add the argument
```sh
docker-compose -f docker-compose.yaml up --build --abort-on-container-exit
```
### Selenium 4 versus 3
NOTE: replacing the hub url
```python
hub = 'http://{}:4444'.format(host)
```
with a __3.X__ style one
```python
hub = 'http://{}:4444/wd/hub'.format(host)
```
will lead to a
```text
HTTPConnectionPool(host='selenium', port=4444): 
Max retries exceeded with url: /wd/hub/session 
Caused by NewConnectionError('<urllib3.connection.HTTPConnection object at 0x7f933b174880>: 
Failed to establish a new connection: [Errno 111] Connection refused')
```
and enentually to

```text
basic-selenium4-grid_app_1 exited with code 1
Aborting on container exit...
Stopping basic-selenium4-grid_selenium_1 ... done
```
### Cleanup

```sh
docker container prune -f
docker container rm selenium
docker image rm selenium-client selenium/standalone-chrome selenium/standalone-chrome selenium/standalone-chrome:3.141.59-vanadium basic-selenium4-grid_app python:3.8.12
```
### See Also

[selenium 4 grid getting started](https://www.selenium.dev/documentation/grid/getting_started/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
