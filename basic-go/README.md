### Info

Original multi-stage Dockerfile split into build and run pieces and 
working directory on the host used to transfer the artifacts. Note: the application is showing some issues in execution, but the project is useful as skeleton for others

it contains an elementary example golang program `example1.go`:
```go
package main

import (
  "fmt"
  "log"
  "os"
  "os/exec"
  "time"
)

func main() {
  if len(os.Args) != 2 {
    os.Exit(1)
  }
  logFilePath := os.Args[1]

  log.Println("Reading " + logFilePath)
  // NOTE: no '-f' flag
  //
  cmd := exec.Command( "tail",  "-n 20", logFilePath)
  stdout, err := cmd.CombinedOutput()

  if err != nil {
    log.Fatal("failed to Start: " + err.Error())
  } else {
    fmt.Printf("Started pid=%d\n" , cmd.Process.Pid)
    fmt.Printf(string(stdout))
    time.Sleep(1000 *  time.Millisecond)
  }
}
```
(which is failing presumaby due to permission?)
and basic example from [Go exec command tutorial](https://zetcode.com/golang/exec-command/) that does not do i/o and does not fail:
```go
package main

import (
    "bytes"
    "fmt"
    "log"
    "os/exec"
    "strings"
)

func main() {
    cmd := exec.Command("tr", "a-z", "A-Z")

    cmd.Stdin = strings.NewReader("and old falcon")

    var out bytes.Buffer
    cmd.Stdout = &out

    err := cmd.Run()

    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("translated phrase: %q\n", out.String())
}
```

### Usage
 * build applications:

```sh
export IMAGE=basic-go-build
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile.build .
export NAME=basic-go-build
docker container rm $NAME
docker run -d --name=$NAME $IMAGE
for I in  1 2 3 ; do docker cp $NAME:/app/example$I . ; done
```
* examine the binaries
```sh
file example?
example1: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, stripped
example2: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, stripped
example3: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, stripped
```
* run the regular command in container:
```sh
IMAGE=basic-go-run
IMAGE=basic-go-run
docker build -t $IMAGE -f Dockerfile.run  .
docker run -v /etc:/tmp/etc -it $IMAGE /usr/bin/tail -10 /tmp/etc/lsb-release
```
this will print the file contents
```sh
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=18.04
DISTRIB_CODENAME=bionic
DISTRIB_DESCRIPTION="Ubuntu 18.04.5 LTS"
```
can repeat with local file in container:
```sh
docker run -v /etc:/tmp/etc -it $IMAGE /usr/bin/tail -10 /etc/alpine-release
```
this will print
```sh
3.9.5
```


```sh
docker run -v /etc:/tmp/etc -it $IMAGE /example1 /tmp/etc/lsb-release
```
```sh
2021/01/24 22:33:28 Reading /tmp/etc/lsb-release
2021/01/24 22:33:28 failed to Start: exit status 1
```

```sh
docker run -v /etc:/tmp/etc -it $IMAGE /example2 /tmp/etc/lsb-release
```
```sh
2021/01/24 21:53:13 Reading /tmp/etc/lsb-release
2021/01/24 21:53:13 scanner exit
2021/01/24 21:53:13 Waiting for command to finish...
2021/01/24 21:53:13 cmd.Wait error: exit status 1
```
for comparison the textbook example works:
```sh
docker run -v /etc:/tmp/etc -it $IMAGE /example3
```
```sh
translated phrase: "AND OLD FALCON"
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


