### Info

This dir contains examples from [Python click tutorial](https://zetcode.com/python/click/) and regular example from [Python getopts library](https://docs.python.org/3/library/getopt.html)

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
*  run default entrypoint (all options are provided)
```sh
docker run -it $IMAGE
```
```text
Method 1 is called
search = "something"
filename = "outname"
debug = True
```
* run with some arguments omitted
```
docker run --entrypoint '' -it $IMAGE python click_groups.py method2 -s foo

```
it will stop with prompt
```text
Your file argument:
```

* enter filename[dummy.txt]:

```sh
Your file argument[dummy.txt]: filename
```
the script will print ant return
```text
Method 2 is called
search = "foo"
filename = "filename"
debug = False
```

Note: providing `default` for a non-flag option does not suppress the prompt for that option

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
### Multiple alighed Argument pairs
* construct long command line:
```sh
echo -n 'python3 click_option_pairs.py '
 for i in  {a..z} ; do echo  -n " --keys ${i}1  --vals ${i}1" ; done
for i in  {a..z} ; do echo  -n " --keys ${i}1 " ; echo -n  '--vals ' ;echo -n "${i}1" |tr '[a-z]' '[A-Z]' ; done
```
* run the command line
```sh
python3 click_option_pairs.py  --keys a1 --vals A1 --keys b1 --vals B1 --keys c1 --vals C1 --keys d1 --vals D1 --keys e1 --vals E1 --keys f1 --vals F1 --keys g1 --vals G1 --keys h1 --vals H1 --keys i1 --vals I1 --keys j1 --vals J1 --keys k1 --vals K1 --keys l1 --vals L1 --keys m1 --vals M1 --keys n1 --vals N1 --keys o1 --vals O1 --keys p1 --vals P1 --keys q1 --vals Q1 --keys r1 --vals R1 --keys s1 --vals S1 --keys t1 --vals T1 --keys u1 --vals U1 --keys v1 --vals V1 --keys w1 --vals W1 --keys x1 --vals X1 --keys y1 --vals Y1 --keys z1 --vals Z1
```
```text
keys: a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1, w1, x1, y1, z1
vals: A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1

```
#### Cleanup
```sh
docker container prune -f
docker image rm $IMAGE
```

### TODO

* figure out if the freehand arguments can be used with `click`. Currently unable to do so:

```sh
python3 click_groups.py  method1 --search text  --filename file --debug  x,y,z
```
```text
Usage: click_groups.py method1 [OPTIONS]
Error: Got unexpected extra argument (x,y,z)
```

### See Also

  * [definitive Guide to Python Click](https://www.assemblyai.com/blog/the-definitive-guide-to-python-click/) - possibly, not the best
  * https://zetcode.com/python/click/
  * [building command line applications with Click](https://pymbook.readthedocs.io/en/latest/click.html)
  * __Working with Databases in Python 3__ [pluralsight training](https://app.pluralsight.com/library/courses/python-3-working-databases) making extensive usage of `click` in practical programs

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
