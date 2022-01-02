### Usage

* example container that entirely relies on directory mapping and environment
* build:
```sh
export FILENAME=hello_world.py
docker-compose up
```
```text
Creating python_runner_container ... done
Attaching to python_runner_container
python_runner_container | check
python_runner_container exited with code 0
```
* another way to reuse the container after a failed run
```sh
unset FILENAME
docker-compose up
```
the key environment var was set and the container quits immediately
```text
WARNING: The FILENAME variable is not set. Defaulting to a blank string.
Starting python_runner_container ... done
```

* run again now mounting the current directory:

```sh
 docker run -v $(pwd)/temp:/temp --env FILENAME=hello_world.py basic-python2_python_runner
```
```text
check
```

* clean up
```sh
docker-compose rm -f
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
