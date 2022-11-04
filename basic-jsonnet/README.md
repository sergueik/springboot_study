### Info

customized version of original implementation GCC [google/jsonnet](https://github.com/google/jsonnet) `Dockerfile` combined with [leprechaun/docker-grafonnet](https://github.com/leprechaun) `Dockerfile`

### Usage

* optionally update the workspace directory with grafonnet libraries:
```sh
git clone --depth 1 https://github.com/grafana/grafonnet-lib.git ./grafonnet-lib
```
and jsonnet source

```sh
git clone --depth 1 https://github.com/google/jsonnet
```
```sh
IMAGE=alpine-jsonnet
docker build -f Dockerfile -t $IMAGE .
```
### Compile jsonnet File

create a source e.g. `test.jsonnet` in local `data` directory
```sh
docker run --rm -v $(pwd)/data:/opt/staging $IMAGE /opt/staging/test.jsonnet
```

* NOTE: avoid using `.` for current work dir:
```
docker run -v .:/opt/staging $IMAGE staging/test.jsonnet
```
```text
docker: Error response from daemon: create .:
volume name is too short, names should be at least two alphanumeric characters.
```

### Cleanup

* docker container
```sh
docker container ls -a | grep alpine-jsonnet | cut -f 1 -d ' ' | xargs -IX docker container rm X
```

* external repositores
```sh
rm -fr jsonnet grafonnet-lib
```
### See Also

 * [golnbg build binary releases](https://github.com/google/go-jsonnet/releases)




### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
