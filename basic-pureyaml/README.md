### Info

### Usage

* install `pyyaml` module system-wide, using pip on `python:3.9-alpine` Python Alpine based container
```sh
export IMAGE=basic-pyyaml
docker build -f  Dockerfile.system  -t $IMAGE .
```
run 
```sh
docker run -it $IMAGE python pyyaml_test.py
```
```text
`{'a': 1, 'b': {'c': 3, 'd': 4}}
a: 1
b:
  c: 3
  d: 4
``

### File Based Module Install

NOTE: as the `pureyaml` has no releases, need to download the full repo

```
curl -skL https://github.com/manuphatak/pureyaml/archive/refs/heads/develop.zip -o pureyaml.zip
unzip pureyaml.zip
```
* build container with `pureyaml` installed through file copy
```sh
export IMAGE=basic-pureyaml
docker build -t $IMAGE -f Dockerfile.file .

```
* run the container

```sh
docker run -it $IMAGE sh
```
```text
Traceback (most recent call last):
  File "/app/pureyaml_test.py", line 3, in <module>
    import pureyaml
  File "/app/pureyaml/__init__.py", line 18, in <module>
    from .decoder import YAMLDecoder
  File "/app/pureyaml/decoder.py", line 7, in <module>
    from .parser import YAMLParser
  File "/app/pureyaml/parser.py", line 11, in <module>
    from .ply.lex import lex
ModuleNotFoundError: No module named 'pureyaml.ply'

```
Turns out that  pureyaml needs `ply` and the latter was not installed yet into the container. Separate issue is that loading of `ply` is implemented in a non-portable fashion

#### Cleanup
```sh
docker container prune -f
docker image rm $IMAGE
```

### See Also
  * https://pyyaml.org/wiki/PyYAMLDocumentation (NOTE: examples might be outdated)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
