### Info

this directory contains some checks of the [alpine python chromedriver](https://hub.docker.com/r/joyzoursky/python-chromedriver/) Docker container.

NOTE: the author's `Dockerfile` does not reflect the switch to glibc he is made with his images:


### Usage


```sh
IMAGE=basic-selenium-python
docker build -f Dockerfile -t $IMAGE .
```
run the basic test script launching Chromium headless and examining the version via [CDP](https://www.selenium.dev/documentation/webdriver/bidi/cdp/) call:

```sh
IMAGE=basic-selenium-python
docker run --shm-size=2g --cap-add=SYS_ADMIN -w /usr/workspace -v $(pwd):/usr/workspace -it $IMAGE python chromium_selenium4_test.py
```
this will print
```text
Browser.getVersion:
dict_keys(['jsVersion', 'product', 'protocolVersion', 'revision', 'userAgent'])
jsVersion: 14.7.173.20
product: Chrome/147.0.7727.116
revision: @dbcf1b1bfb506cc580859bcb5ff9460a8443af90
userAgent: "Chromium 95.0.4638.69"
```

run test to take screenshot
```sh
IMAGE=basic-selenium-python
docker run --shm-size=2g --cap-add=SYS_ADMIN -w /usr/workspace -v $(pwd):/usr/workspace -it $IMAGE python screenshot_selenium4_test.py
```
ignore the 
```text
exception (ignored): a bytes-like object is required, not 'dict'
```

For Selenium 3 pull the 5 year old [alpine python chromedriver](https://hub.docker.com/r/joyzoursky/python-chromedriver/) Docker image:

```sh
docker pull joyzoursky/python-chromedriver:3.9-alpine-selenium
```
```sh
docker run -w /usr/workspace -v $(pwd):/usr/workspace -it joyzoursky/python-chromedriver:3.9-alpine-selenium python chromium_selenium3_test.py
```
this will print
```text
Browser.getVersion:
POST to http://127.0.0.1:47453/session/400fb168f3f8e373abb9db4a5a9eb9ed/chromium/send_command_and_get_result
params: {"cmd": "Browser.getVersion", "params": {}}
dict_keys(['value'])
dict_keys(['jsVersion', 'product', 'protocolVersion', 'revision', 'userAgent'])
jsVersion: 9.1.269.36
product: HeadlessChrome/91.0.4472.101
revision: @af52a90bf87030dd1523486a1cd3ae25c5d76c9b
userAgent: "Chromium 95.0.4638.69"
```

```sh
docker run -w /usr/workspace -v $(pwd):/usr/workspace -it joyzoursky/python-chromedriver:3.9-alpine-selenium python screenshot_selenium3_test.py
```
### Cleanup
```
docker container prune -f
docker builder prune -f
docker volume prune -f
docker image prune -f
docker image rm basic-selenium-python
```
### See Also
  * [github repository of the image used in this project](https://github.com/joyzoursky/docker-python-chromedriver)
  * https://github.com/joyzoursky/docker-python-chromedriver/blob/master/deprecated/py3.6-xvfb-selenium/Dockerfile
  * https://github.com/joyzoursky/docker-python-chromedriver/blob/master/py-alpine/3.8-alpine-selenium/Dockerfile
  * https://github.com/cypress-io/cypress/issues/419

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
