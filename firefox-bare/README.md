### Info

Work in progress attempt to construct a lone Firefog Mozilla and Gecko Driver
container starting from a bare bones openjdk:8 Docker image
installing Firefox version  __70.0__ and trying to lauch the browser without relying on
Selenium. Turns out that despite supporting the headless mode, the firefox still lists various X
libraries as dependencies.

### Testing

* Build Docker image
```sh
docker build -f Dockerfile -t firefox-example .
```
* Lanch the `mysql-example` backed Docker container
```sh
docker run -p 4444:4444 -d firefox-example
```

Verify being able to talk to geckodriver directly using
[webdriver protocol](https://w3c.github.io/webdriver/#processing-model).

```sh
curl http://localhost:4444/status 2> /dev/null | jq '.' -
```
```js
{
  "value": {
    "message": "",
    "ready": true
  }
}
```
```sh
curl -X POST http://localhost:4444/session -H "Content-Type:application/json" -d "{}" 2>/dev/null | jq '.' -
```
error to find mozilla:
```js
{
  "value": {
    "error": "session not created",
    "message": "Expected browser binary location, but unable to find binary in default location, no 'moz:firefoxOptions.binary' capability provided, and no binary flag set on the command line",
    "stacktrace": ""
  }
}
```
Error to launch mozilla:
```js
{
  "value": {
    "error": "unknown error",
    "message": "invalid argument: can't kill an exited process",
    "stacktrace": ""
  }
}

```
success actually using an X destop image, which is much heavier:

```js
{
  "value": {
    "sessionId": "7281fd1e-ccc2-4da6-841f-1c7e6374bd52",
    "capabilities": {
      "acceptInsecureCerts": false,
      "browserName": "firefox",
      "browserVersion": "70.0",
      "moz:accessibilityChecks": false,
      "moz:geckodriverVersion": "0.26.0",
      "moz:headless": true,
      "moz:processID": 1575,
      "moz:profile": "/tmp/rust_mozprofile.yiTwTSUTdMjc",
      "moz:shutdownTimeout": 60000,
      "moz:useNonSpecCompliantPointerOrigin": false,
      "moz:webdriverClick": true,
      "pageLoadStrategy": "normal",
      "platformName": "linux",
      "platformVersion": "3.13.0-46-generic",
      "rotatable": false,
      "setWindowRect": true,
      "strictFileInteractability": false,
      "timeouts": {
        "implicit": 0,
        "pageLoad": 300000,
        "script": 30000
      },
      "unhandledPromptBehavior": "dismiss and notify"
    }
  }
}
```
### Troubleshooting

* Stop container
* Launch interactive session
```sh
docker run -p 4444:4444 -it firefox-example "/bin/bash"g
```
* Investigate what is missing
```sh
root@c685ca44c713:/# /opt/docker/firefox/firefox --version
XPCOMGlueLoad error for file /opt/docker/firefox/libmozgtk.so:
libgtk-3.so.0: cannot open shared object file: No such file or directory
Couldn't load XPCOM.
```
```sh
apt-get install -q -y libgtk-3-0
```
(adds 200 MB of downloads)

```sh
libdbus-glib-1.so.2: cannot open shared object file: No such file or directory
```
* After a few more attempts to launch mozilla.
```sh
apt-get install -q -y libdbus-glib-1-2 libxt6
```

```sh
./firefox --headless -version
*** You are running in headless mode.
Mozilla Firefox 70.0
```
### TODO:

Specify the headless option through geckodriver.



### See also
  * List of [WebDriver commands](https://developer.mozilla.org/en-US/docs/Web/WebDriver/Commands) from MDN.

