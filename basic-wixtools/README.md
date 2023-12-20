### Info

this directory contains a Docker project based on alpine
[docker wix](https://github.com/dactivllc/docker-wix) with some modifications regarding the `UID` and pinneed software versions

### Usage

```sh
sed -i "s|UID=[0-9][0-9]*|UID=$UID|g" Dockerfile
IMAGE=basic-wix
docker build -f Dockerfile -t $IMAGE .
```
followed by
```sh
docker run -v $(pwd):/wix $IMAGE candle Setup/Product.wxs
```
this will create `Product.wixobj` in the project directory

```sh
docker run -v $(pwd):/wix $IMAGE light Product.wixobj -ext WixUIExtension -ext WixUtilExtension -sval
```
this will fail with
```text
Z:\wix\Setup\Product.wxs(16) : error LGHT0103 : The system cannot find the file '..\Program\sample.txt'.
```

the path issue can be remediated by running docker from `Setup` directory:

```sh
docker run -w /wix/Setup -v $(pwd):/wix $IMAGE candle Product.wxs
docker run -w /wix/Setup -v $(pwd):/wix $IMAGE light Product.wixobj -ext WixUIExtension -ext WixUtilExtension -ext WixNetFxExtension -sval
```

NOTE: without the this `-ext` flags the command will fail with
```text
Z:\wix\Setup\Product.wxs(10) : error LGHT0094 : Unresolved reference to symbol 'WixUI:WixUI_Minimal' in section 'Product:{EA524AD9-6920-4D46-B03E-7DA72F46F89B}'.
Z:\wix\Setup\Product.wxs(25) : error LGHT0094 : Unresolved reference to symbol 'Dialog:ExitDialog' in section 'Product:{EA524AD9-6920-4D46-B03E-7DA72F46F89B}'.
Z:\wix\Setup\Product.wxs(29) : error LGHT0094 : Unresolved reference to symbol 'Binary:WixCA' in section 'Product:{EA524AD9-6920-4D46-B03E-7DA72F46F89B}'.
```

Alternatively, open an interactive shell in the container to run WixTools commands:
```sh
docker run -v $(pwd):/wix -it $IMAGE sh
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
