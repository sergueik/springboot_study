### info

`Dockerfile` and test  script from __nodeJS Selenium WebDriver for Linux Chrome Docker Image__ [stackoverflow topic](https://stackoverflow.com/questions/71135033/nodejs-selenium-webdriver-for-linux-chrome-docker-image)


### Note

uses default image for nodejs which is huge
```text
node                               latest                  548714e444f4   12 days ago     998MB

```
the size of the final image is huge

```text
```

image build is very time consuming

### Usage

* build
```sh
IMAGE=basic-nodejs-selenium
docker build -f Dockerfile -t $IMAGE . 
```
* run

```sh
docker run -it $IMAGE
```
```text
Google Search button text: Google Search
```


### Async Run of Tasks

* copy the async loop example and rebuild

```sh
cp test6.js runtest.js
```
```text
run on: Mon Jan 02 2023 21:18:6
Wikipedia page for :fr
text: Wikipédia
run on: Mon Jan 02 2023 21:18:6
Wikipedia page for :ru
text: Добро пожаловать в Википедию,
run on: Mon Jan 02 2023 21:18:6
Wikipedia page for :de
text: Willkommen bei Wikipedia

```
- the intended serialization was not observed

### Note

the application is not stable, observing
error in loop run:
```
run on: Mon Jan 02 2023 22:17:38
Wikipedia page for :de
text: Willkommen bei Wikipedia
run on: Mon Jan 02 2023 22:17:39
Wikipedia page for :fr
text: Wikipédia
WebDriverError: unknown error: session deleted because of page crash
from unknown error: cannot determine loading status
from tab crashed
  (Session info: headless chrome=108.0.5359.124)
    at Object.throwDecodedError (/program/node_modules/selenium-webdriver/lib/error.js:524:15)
    at parseHttpResponse (/program/node_modules/selenium-webdriver/lib/http.js:587:13)
    at Executor.execute (/program/node_modules/selenium-webdriver/lib/http.js:515:28)
    at process.processTicksAndRejections (node:internal/process/task_queues:95:5)
    at async Driver.execute (/program/node_modules/selenium-webdriver/lib/webdriver.js:741:17)
    at async hello (/program/runtest.js:16:5)
    at async /program/runtest.js:41:3 {
  remoteStacktrace: '#0 0x55bd5a0c72a3 <unknown>\n' +
    '#1 0x55bd59e85dfd <unknown>\n' +
    '#2 0x55bd59e71b78 <unknown>\n' +
    '#3 0x55bd59e71250 <unknown>\n' +
    '#4 0x55bd59e703d2 <unknown>\n' +
    '#5 0x55bd59e70225 <unknown>\n' +
    '#6 0x55bd59e6edc6 <unknown>\n' +
    '#7 0x55bd59e6f3a8 <unknown>\n' +
    '#8 0x55bd59e7c72f <unknown>\n' +
    '#9 0x55bd59e7d382 <unknown>\n' +
    '#10 0x55bd59e8e390 <unknown>\n' +
    '#11 0x55bd59e9271b <unknown>\n' +
    '#12 0x55bd59e6f965 <unknown>\n' +
    '#13 0x55bd59e8df92 <unknown>\n' +
    '#14 0x55bd59efa690 <unknown>\n' +
    '#15 0x55bd59ee2903 <unknown>\n' +
    '#16 0x55bd59eb5ece <unknown>\n' +
    '#17 0x55bd59eb6fde <unknown>\n' +
    '#18 0x55bd5a11763e <unknown>\n' +
    '#19 0x55bd5a11ab79 <unknown>\n' +
    '#20 0x55bd5a0fd89e <unknown>\n' +
    '#21 0x55bd5a11ba83 <unknown>\n' +
    '#22 0x55bd5a0f0505 <unknown>\n' +
    '#23 0x55bd5a13cca8 <unknown>\n' +
    '#24 0x55bd5a13ce36 <unknown>\n' +
    '#25 0x55bd5a158333 <unknown>\n' +
    '#26 0x7fa4accedea7 start_thread\n'
}

/program/node_modules/selenium-webdriver/lib/error.js:524
    let err = new ctor(data.message)
              ^

NoSuchSessionError: invalid session id
    at Object.throwDecodedError (/program/node_modules/selenium-webdriver/lib/error.js:524:15)
    at parseHttpResponse (/program/node_modules/selenium-webdriver/lib/http.js:587:13)
    at Executor.execute (/program/node_modules/selenium-webdriver/lib/http.js:515:28)
    at process.processTicksAndRejections (node:internal/process/task_queues:95:5)
    at async Driver.execute (/program/node_modules/selenium-webdriver/lib/webdriver.js:741:17)
    at async hello (/program/runtest.js:29:7)
    at async /program/runtest.js:41:3 {
  remoteStacktrace: '#0 0x55bd5a0c72a3 <unknown>\n' +
    '#1 0x55bd59e85dfd <unknown>\n' +
    '#2 0x55bd59eb54cd <unknown>\n' +
    '#3 0x55bd59ee2a36 <unknown>\n' +
    '#4 0x55bd59ededda <unknown>\n' +
    '#5 0x55bd59ede424 <unknown>\n' +
    '#6 0x55bd59e57213 <unknown>\n' +
    '#7 0x55bd5a11763e <unknown>\n' +
    '#8 0x55bd5a11ab79 <unknown>\n' +
    '#9 0x55bd5a0fd89e <unknown>\n' +
    '#10 0x55bd5a11ba83 <unknown>\n' +
    '#11 0x55bd5a0f0505 <unknown>\n' +
    '#12 0x55bd59e55720 <unknown>\n' +
    '#13 0x7fa4ac6aed0a __libc_start_main\n'
}

Node.js v19.3.0
```
* test
```sh
docker run -it $IMAGE /usr/bin/google-chrome --headless --version
```
```text
[0102/195156.417405:ERROR:zygote_host_impl_linux.cc(100)] Running as root without --no-sandbox is not supported. See https://crbug.com/638180.
```


* test
```sh

docker run -it $IMAGE /usr/bin/google-chrome --headless --version --no-sandbox
```
```text

[0102/195347.927076:ERROR:bus.cc(399)] Failed to connect to the bus: Failed to connect to socket /run/dbus/system_bus_socket: No such file or directory
[0102/195347.927835:ERROR:bus.cc(399)] Failed to connect to the bus: Failed to connect to socket /run/dbus/system_bus_socket: No such file or directory
[0102/195347.932955:WARNING:bluez_dbus_manager.cc(247)] Floss manager not present, cannot set Floss enable/disable.
[0102/195347.955072:WARNING:sandbox_linux.cc(380)] InitializeSandbox() called with multiple threads in process gpu-process.
```

### See Also:

  * https://stackoverflow.com/questions/68869085/how-to-make-chrome-use-session-bus-instead-of-system-bus


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

