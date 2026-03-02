### Usage 

```sh
docker image pull gcr.io/distroless/java11-debian11
docker image pull maven:3.9.3-eclipse-temurin-11-alpine 
```
```sh
IMAGE=basic-distroless 
docker build -t $IMAGE .
```
```sh
docker run --name $IMAGE -p 8080:8080 $IMAGE
```
or run volume mounted
```sh
docker run -e UID=$(id -u) -e GID=$(id -g) -p 8080:8080 \
    -v $(pwf)/data:/app/data $IMAGE
```
> NOTE: There is no shell (`sh`, `bash`, `dash`). There are no package managers, no editors, no system utilities. The image only contains the Java runtime, minimal libc, and the necessary files to run Java apps. That why in `Dockerfile` one needs JSON array variation of the `RUN` clause.

> NOTE: Official "distroless" images provided by Google Container Tools use a Debian-based minimal runtime, no "gcr.io/distroless/java11-alpine" exist primarily due to compatibility issues with `glibc` versus Alpine's `musl` libc library

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
