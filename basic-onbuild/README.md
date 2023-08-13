### Manual Build in Steps
build with `Dockerfile.builder` / `Dockerifile.runner`
```sh
docker build -t basic-builder -f Dockerfile.builder .
```
this will print
```text
Step 1/2 : FROM alpine:3.9.5
 ---> 82f67be598eb
Step 2/2 : ONBUILD RUN echo 'test' >&2
 ---> Running in 21ee48d404bd
Removing intermediate container 21ee48d404bd
 ---> f5ec55674f5f
Successfully built f5ec55674f5f
Successfully tagged basic-builder:latest

```
```sh
docker build -t basic-runner -f Dockerfile.runner .
```
this will print
```text
Step 1/1 : FROM basic-builder
# Executing 1 build trigger
 ---> Running in 5ab9a59f0c8f
test
Removing intermediate container 5ab9a59f0c8f
 ---> a28a4343c4f9
Successfully built a28a4343c4f9
Successfully tagged basic-runner:latest
```


### Manual Build in Steps
build with `Dockerfile.file-builder` / `Dockerifile.file-builder-downstream`
```sh
docker build -t basic-file-builder -f Dockerfile.file-builder .
```
this will print
```text

```
Step 1/4 : FROM alpine:3.9.5
 ---> 82f67be598eb
Step 2/4 : WORKDIR /app
 ---> Running in 832e78168fa1
Removing intermediate container 832e78168fa1
 ---> 23dc29d1f42f
Step 3/4 : ADD a.txt ./
 ---> d0440ab6918c
Step 4/4 : ONBUILD RUN echo 'test' >&2
 ---> Running in 597de5dfedef
Removing intermediate container 597de5dfedef
 ---> 90bc254fb58d
Successfully built 90bc254fb58d
Successfully tagged basic-file-builder:latest
```sh
docker build -t basic-file-builder-downstream -f Dockerfile.file-builder-downstream .
```
this will print
```text
Step 1/3 : FROM alpine:3.9.5
 ---> 82f67be598eb
Step 2/3 : WORKDIR /app
 ---> Using cache
 ---> 23dc29d1f42f
Step 3/3 : COPY --from=basic-file-builder /app/a.txt ./
 ---> 84751d5a2a46
Successfully built 84751d5a2a46
Successfully tagged file-builder-downstream:latest
```
The `ONBUILD` trigger will not be exercises

### Cleanup
```sh
docker image rm basic-file-builder basic-file-builder-downstream basic-builder basic-runner	
```
### See Also
  * https://stackoverflow.com/questions/34863114/dockerfile-onbuild-instruction
  * https://www.howtogeek.com/devops/how-to-use-dockerfile-onbuild-to-run-triggers-on-downstream-builds/


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
