### Info

This dir contains examples from [Python click tutorial](https://zetcode.com/python/click/)

### Usage

* build, using pip to install the module (pinned to __7.0__) system-wide on Alpine 3.9.5 Python based container
```sh
IMAGE=basic-python-click
docker build -t $IMAGE -f Dockerfile.system .
```
run 
```sh
docker run -it $IMAGE --help
```
```text
Usage: groups.py [OPTIONS] COMMAND [ARGS]...

Options:
  --help  Show this message and exit.

Commands:
  method1
  method2
```

```sh
docker run -it $IMAGE method2
```
```text
Method 2 is called
```
download `click` release and copy into Docker container (pick a differnet release, from installed eaelier)
```sh
VERSION=7.1
curl -kL -s https://github.com/pallets/click/archive/refs/tags/$VERSION.zip -o  click-$VERSION.zip
unzip -t click-$VERSION.zip
unzip -x click-$VERSION.zip
```

### File Based Module Install

* build container with `click` installed through file copy
```sh
docker build -t $IMAGE -f Dockerfile.file .
```
* run the plain shell in the container

```sh
docker run --entrypoint '' -it $IMAGE sh
```
* in container navigate away from workdir:
```sh
/app # pwd
/app
/app # ls
click      groups.py
/app # cd /
```
* open REPL
```sh
/ # python
```
```text
```text
Python 3.9.16 (main, Mar 14 2023, 03:24:34)
[GCC 12.2.1 20220924] on linux
Type "help", "copyright", "credits" or "license" for more information.
```
* confirm the `click` is not available globally
```sh
>>> import click
```
```text
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
ModuleNotFoundError: No module named 'click'
```
```sh
>>> exit()
```

* repeat the test that uses configured entrypoint:

```text
Usage: groups.py [OPTIONS] COMMAND [ARGS]...

Options:
  --help  Show this message and exit.

Commands:
  method1
  method2
```

```sh
docker run -it $IMAGE method2
```
```text
Method 2 is called
```

#### Cleanup
```sh
docker container prune -f
docker image rm $IMAGE
```

### See Also

  * [definitive Guide to Python Click](https://www.assemblyai.com/blog/the-definitive-guide-to-python-click/) - possibly, not the best
  * https://zetcode.com/python/click/
  * [building command line applications with Click](https://pymbook.readthedocs.io/en/latest/click.html)
  * __Working with Databases in Python 3__ [pluralsight training](https://app.pluralsight.com/library/courses/python-3-working-databases) making extensive usage of `click` in practical programs

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
