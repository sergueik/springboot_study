### Info

this directory contains example using ANSI color and extra build step to validate build arguments

```sh
docker build -f Dockerfile .
```
will fail with the message about the exit code
```text
returned a non-zero code: 1
```
NOTE: the instruction message
```sh
if [ ! "${ARG1}" ]
then
  ERROR_MESSAGE="ARG1 must be passed as --build-arg ARG1=..."
  echo "${COLOR_RED}${ERR}${COLOR_RESET}"
fi
```

will not even be displayed - this code is effectively useless

proviging `DOCKER_BUILDKIT=1` argument does not help much
the error message after the run would become
```text
failed to solve with frontend dockerfile.v0: failed to build LLB: executor failed running [/bin/sh -c if [ ! "${ARG1}" ]; then      ERROR_MESSAGE="ARG1 must be passed as --build-arg ARG1=...";ERROR=1; fi;    COLOR_RED='\033[0;31m';COLOR_RESET='\033[0m';    if [ ! -z "$ERROR" ]; then echo "${COLOR_RED}${ERR}${COLOR_RESET}"; else echo "${COLOR_GREEN}build ARG1=\"$ARG1\"${COLOR_RESET}"; fi;     if [ ! -z "$ERROR" ]; then  exit 1; fi]: runc did not terminate sucessfully
```
```sh
docker build --build-arg ARG1=foo -t example -f Dockerfile .
```
```text
build ARG1="foo"
Removing intermediate container 20d4a19c610b
 ---> c0d7f935a435
Step 8/8 : CMD ["/bin/sh", "-c", "echo ${COLOR_GREEN}build $ARG1${COLOR_RESET}"]
 ---> Running in 4b5eeb97da93
Removing intermediate container 4b5eeb97da93
 ---> 8fa3d81d7a1c
Successfully built 8fa3d81d7a1c
```
```sh
docker run -it example
```
```text
build foo
```
NOTE: the colored output will be present on Debian or RHEL base image based Docker but not on Alpine one
swapping the image in `FROM` and repeating the hppy build command will show plain color
```text
\033[0;32mbuild ARG1="foo"\033[0m
```

### See Also
   * https://superuser.com/questions/270214/how-can-i-change-the-colors-of-my-xterm-using-ansi-escape-sequences
   * https://en.wikipedia.org/wiki/ANSI_escape_code#Unix-like_systems
   * https://tintin.mudhalla.net/info/ansicolor/
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
