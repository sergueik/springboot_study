### Info

* example container that entirely relies on directory mapping and environment
### Usage
* build:
```sh
export FILENAME=hello_world.py
docker-compose up --build
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
### Timing Commands

This can be useful to time graalvm native spring images later
```sh
docker build -t time -f Dockerfile.time .
docker run -it -v $(pwd)/temp:/temp --env FILENAME=hello_world.py time
```
```sh
docker build -t time2 -f Dockerfile.time2 .
docker run -it -v $(pwd)/temp:/temp --env FILENAME=hello_world.py time2
```
the verbose format of `time`  on alpine is:
```text

 Command being timed: "python temp/hello_world.py"
  User time (seconds): 0.01
  System time (seconds): 0.01
  Percent of CPU this job got: 96%
  Elapsed (wall clock) time (h:mm:ss or m:ss): 0m 0.02s
  Average shared text size (kbytes): 0
  Average unshared data size (kbytes): 0
  Average stack size (kbytes): 0
  Average total size (kbytes): 0
  Maximum resident set size (kbytes): 22640
  Average resident set size (kbytes): 0
  Major (requiring I/O) page faults: 0
  Minor (reclaiming a frame) page faults: 707
  Voluntary context switches: 1
  Involuntary context switches: 10
  Swaps: 0
  File system inputs: 0
  File system outputs: 0
  Socket messages sent: 0
  Socket messages received: 0
  Signals delivered: 0
  Page size (bytes): 4096
  Exit status: 0


```
### Clean Up
```sh
docker-compose rm -f
docker container prune -f
docker image prune -f
docker image rm time time2
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
