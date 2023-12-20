### Info

this directory contains a replica of alpine-based
[docker wix](https://github.com/dactivllc/docker-wix) with some modifications regarding the `UID` and pinneed software versions

### Usage

```sh
sed -i "s|UID=[0-9][0-9]*|UID=$UID|g" Dockerfile.new
IMAGE=basic-wix
docker build -f Dockerfile.new -t $IMAGE .
```
followed by
```sh
docker run -v $(pwd)/sample:/wix $IMAGE candle sample.wxs
```
this wll create `sample.wixobj` in the project `sample` directory

```sh
docker run -v $(pwd)/sample:/wix $IMAGE light sample.wixobj -sval
```
this wll create `sample.msi` in the project `sample` directory


NOTE: the custom actions have not been tried yet (`WixNetFxExtension.dll`, `WixUIExtension.dll`)
### Cleanup


```sh
docker container prune -f
docker image prune -f
```

### TODO

suppress Docker warning
```text
[Warning] The requested image's platform (linux/386) does not match the detected host platform (linux/amd64) and no specific platform was requested

```
### See Also

  * fedora based
    +[docker wixtoolset](https://github.com/vtavernier/docker-wixtoolset)
  * debian based
    +[docker wixtoolset](https://github.com/utilitywarehouse/docker-wixtoolset)
  * ubuntu based
    + [docker wixtoolset](https://github.com/identakid/wix) 
    + [docker wixtoolset](https://github.com/felfert/docker-wix) 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
