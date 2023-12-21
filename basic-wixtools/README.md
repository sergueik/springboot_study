### Info

this directory contains a Docker project based on alpine
[docker wix](https://github.com/dactivllc/docker-wix) with some modifications regarding the `UID` and pinneed software versions

### Usage

```sh
sed -i "s|UID=[0-9][0-9]*|UID=$UID|g" Dockerfile
wget https://dl.winehq.org/wine/wine-mono/6.0.0/wine-mono-6.0.0-x86.msi -nv -O mono.msi
wget -nv -O wix.zip https://github.com/wixtoolset/wix3/releases/download/wix3112rtm/wix311-binaries.zip
mkdir wix
unzip wix.zip -d wix
```
```sh
IMAGE=basic-wix
docker build -f Dockerfile --platform linux/386 -t $IMAGE .
rm -fr wix
```
followed by

```sh
docker run -w /wix/Setup -v $(pwd):/wix $IMAGE candle Product.wxs
docker run -w /wix/Setup -v $(pwd):/wix $IMAGE light Product.wixobj -ext WixUIExtension -ext WixUtilExtension -ext WixNetFxExtension -sval
```

Alternatively, open an interactive shell in the container to run WixTools commands:
```sh
docker run -v $(pwd):/wix --platform linux/386 -it $IMAGE sh
```
in the shell, change the directory and build MSI package from `Setup`:

```sh
cd Setup
candle Product.wxs
light -ext WixUIExtension -ext WixUtilExtension Product.wixobj -sval
```
this wll create `Product.msi` in the `Setup` directory

NOTE: not providing the full path to `WixNetFxExtension.dll`, `WixUIExtension.dll` etc. to the linker

### Cleanup

```sh
rm Setup/Product.msi Setup/Product.wix*
```

```sh
docker container prune -f
docker image prune -f
```

### TODO

suppress Docker warning
```text
[Warning] The requested image's platform (linux/386) does not match the detected host platform (linux/amd64) and no specific platform was requested
```

### Note

the image size is big compared to released:
```text
basic-wix                                       latest                  5977c2a0581a   4 minutes ago    971MB
dactiv/wix                                      latest                  0081f46494b8   2 years ago      691MB
```
### See Also

  * fedora based
    + [vtavernier/docker-wixtoolset](https://github.com/vtavernier/docker-wixtoolset)
  * debian based
    + [utilitywarehouse/docker-wixtoolset](https://github.com/utilitywarehouse/docker-wixtoolset)
  * ubuntu based
    + [identakid/wix](https://github.com/identakid/wix)
    + [felfert/docker-wix](https://github.com/felfert/docker-wix)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
