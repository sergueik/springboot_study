### Info

This directory contains a `Dockerfile` from [zenika/alpine-chrome](https://github.com/Zenika/alpine-chrome/blob/master/Dockerfile) but switched to __maven/jdk8__ apline base [image]( https://hub.docker.com/r/zenika/alpine-maven/tags) with maven and chromium to run the test suites. Note: the `chromium` binary installs some X libraries but is runnable in headless mode in console mode.

### Usage
* install the `chromium-browser` locally
```sh
apt-get install -q -y chomium-browser
```
* download and place into `/usr/bin` the `chromedriver`
```sh
wget https://chromedriver.storage.googleapis.com/89.0.4389.23/chromedriver_linux64.zip -O ~/Downloads/chromedriver_linux64.zip
cd ~/Downloads
unzip -f chromedriver_linux64.zip
sudo mv chromedriver /usr/bin
```
* compile test project. Note the need to remove the target directory

```sh
cd demo.project
sudo rm -fr target
mvn test-compile
mvn test
cd ..
```
- the test runs the chmromium headless and saves a pdf file into current directory
* build the image with the `chromium` and `chromium-driver` installed via `apk` installer accepting that both will be relatively old versions

```sh
IMAGE='basic-maven-chromium'
docker build -t $IMAGE -f Dockerfile .
```
* run chromium directly in the container:

```sh
export NAME=$IMAGE

docker run --name $NAME -it $IMAGE /usr/bin/chromium-browser --headless --disable-gpu  --no-sandbox --dump-dom https://www.wikipedia.org

```
this will print HTML in console
* NOTE: the version of chromium-browser in the container will be rather old, but common options (`dump-dom`, `print-to-pdf`,`screenshot`) are supported
```sh
docker run -it $IMAGE sh
```
or 
```sh
ID=$(docker container ls -a |grep $IMAGE| head -1 | awk '{print $1}')
docker start $ID
docker exec -it $ID sh
```
- unfortunately this container instance may quit too quickly
```sh
/usr/bin/chromium-browser --headless --disable-gpu --no-sandbox --screenshot https://www.wikipedia.org
```
```text
Written to file screenshot.png.
```
```sh
/usr/bin/chromium-browser --headless --disable-gpu --no-sandbox --print-pdf https://www.wikipedia.org
```

```text
Written to file output.pdf.
```



* run some ultra basic regular Selenium test 
```sh
docker run -it -v "$PWD/demo.project":/demo -w /demo $IMAGE mvn clean test
```
for repeated runs use the commands:

```sh
ID=$(docker container ls -a |grep $IMAGE| head -1 | awk '{print $1}')
docker start $ID 
docker exec -it -w /demo $ID mvn clean test
```
which returns
```sh
Results :
Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
```
* NOTE: the same test will fail with Selenium __4.7.2__:
```text
[INFO]
[INFO] Results:
[INFO]
[ERROR] Failures:
[ERROR]   ChromiumBrowserTest.downloadPDF:116
Expected: is <true>
     but: was <false>
[INFO]
[ERROR] Tests run: 2, Failures: 1, Errors: 0, Skipped: 0

```
that is confirmed through
```sh
ID=$(docker container ls -a |grep $IMAGE| head -1 | awk '{print $1}')
docker start $ID
docker exec -it $ID sh -c 'test -f /demo/sample.pdf && echo OK'
```
will print
```sh
OK
```
and basic CDP test (e.g. copy from [sergueik/cdp4j_tests](https://github.com/sergueik/cdp4j_tests)
```sh
git clone https://github.com/sergueik/cdp4j_tests demo.cdp
docker run -it -v "$PWD/demo.cdp":/demo -w /demo -e USE_CHROMIUM=true $IMAGE mvn clean test
```
from mounted host directory
* alternatively connect into the container
```sh
docker run -v demo -it $IMAGE sh
```
### Cleanup

if there is no other stopped containers 
```sh
docker container prune -f
```

selectively
```sh
docker container ls -a | grep $IMAGE | awk '{print $1}' | xargs -IX docker container rm X
```
### Note

* in the current layout the `target` directory in `demo` project becomes owned by root account.

### File Download Test

```java
@Test
public void downloadPDF() {
  url = "http://www.africau.edu/images/default/sample.pdf";
  driver.get(url);
  try {
    Thread.sleep(5000);
  } catch (InterruptedException e) {
  }
  File file = new File((downloadDirectory != null ? downloadDirectory : "/tmp") + "/" + "sample.pdf");
  assertThat(file.exists(), is(false));
  File f = new File(System.getProperty("user.dir") + "/" + "sample.pdf");
  assertThat(f.exists(), is(true));
}
```
```sh
docker run -e DOWNLOAD_DIRECTORY=/tmp -it -v "$PWD/demo.selenium":/demo -w /demo $IMAGE mvn clean test ;  CONTAINER=$(docker container ls -a |grep $IMAGE | head -1 | cut -f1 -d ' '); docker container start $CONTAINER;docker exec -it $CONTAINER sh -c "find / -iname '*pdf' 2>/dev/null"
```
```sh
fe728cfc1b7e
/demo/sample.pdf
```
The setting seems to have no effect:
```java

chromeOptions.setExperimentalOption("prefs", new HashMap<String, Object>() {
  {
    put("profile.default_content_settings.popups", 0);
    put("download.default_directory", downloadDirectory != null ? downloadDirectory : "/tmp");
    put("download.prompt_for_download", false);
    put("download.directory_upgrade", true);
    put("safebrowsing.enabled", false);
    put("plugins.always_open_pdf_externally", true);
    put("plugins.plugins_disabled", new ArrayList() {
      {
        add("Chrome PDF Viewer");
      }
    });
  }
});
```


### See Also
 * blog on [running Chrome in Docker](https://medium.com/@sahajamit/can-selenium-chrome-dev-tools-recipe-works-inside-a-docker-container-afff92e9cce5)

### NOTE

* the issue observed occasionally
```sh
fetch http://dl-cdn.alpinelinux.org/alpine/v3.11/main/x86_64/APKINDEX.tar.gz
ERROR: unable to select packages:
  /bin/sh (virtual):D	
    provided by: busybox-1.33.0-r6
                 busybox-1.31.1-r10
```
* in attempt to solve, replicate `Dockerfile` from [](https://github.com/Zenika/alpine-maven/blob/master/jdk8/Dockerfile) upgrade the base jdk to e.g. alpine3.9 and do a image cleanup with [dependency traversal](https://stackoverflow.com/questions/36584122/how-to-get-the-list-of-dependent-child-images-in-docker)
```sh
ID=$(docker image ls  |grep 'zenika/alpine-maven' | head -1| awk '{print $1}')
docker inspect --format='{{.Id}} {{.Parent}}'     $(docker images --filter since=$ID --quiet)
```
and remove all and start over
### See Also

  * https://alpinelinux.org/releases/
  * https://developer.chrome.com/blog/headless-chrome/
  * [python - Running Selenium with Headless Chrome Webdriver - Stack Overflow](https://stackoverflow.com/questions/53657215/running-selenium-with-headless-chrome-webdriver)
  * [python - Selenium: WebDriverException:Chrome failed to start: crashed as google-chrome is no longer running so ChromeDriver is assuming that Chrome has crashed](https://stackoverflow.com/questions/53073411/selenium-webdriverexceptionchrome-failed-to-start-crashed-as-google-chrome-is)
  * [how To Run Selenium With Chrome In Docker](https://stackoverflow.com/questions/45323271/how-to-run-selenium-with-chrome-in-docker)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
