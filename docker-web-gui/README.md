### Info

This directory contains a close replica of the

[rakibtg/docker-web-gui](https://github.com/rakibtg/docker-web-gui) app -  node.js alpine docker hosted web based GUI for managing Docker containers and images

### Modifications to the Project

* pinned the base image `node:alpine` to `node:8.12-alpine`
* added `python` and `docker` dependencies install - the host `docker` will not be runnable in container to allow reading the host inventory through socket which is exported volume mounted to the container during the run

### Usage

* build the image:
```sh
NAME=docker-web-gui
docker build -t $NAME -f Dockerfile .
```
* run the image:
```sh
docker container rm $NAME

docker run -p 3230:3230 --name $NAME -v /var/run/docker.sock:/var/run/docker.sock $NAME
```
NOTE: the original project was providing an invalid run command which attempts to map both the docker socket and the binary from the host into the container. The socker is the correct way to let docker process running in container access host inventory, but the binary is not compabile
We install client locally in the contianer and only mount volume for socket

### Result

* images
![images](https://github.com/sergueik/springboot_study/blob/master/docker-web-gui/screenshots/capture-images.png)

* containers
![containers](https://github.com/sergueik/springboot_study/blob/master/docker-web-gui/screenshots/capture-containers.png)

### Cleanup

```sh
docker container stop $IMAGE
docker container rm $IMAGE
docker image rm $IMAGE
```
### Notes

switching to `node:16.12.0-alpine3.11` with intent to install just `docker-cli`, which is not possible in earlier releases of alpine than __3.11__ leads to error bulding node modules from source after finding no pre-built binaries for the platform:
```text
npm ERR! node-pre-gyp WARN Pre-built binaries not found for sqlite3@4.2.0 and node@16.12.0 (node-v93 ABI, musl) (falling back to source compile with node-gyp)
npm ERR! node-pre-gyp http 403 status code downloading tarball https://mapbox-node-binary.s3.amazonaws.com/sqlite3/v4.2.0/node-v93-linux-x64.tar.gz
npm ERR! gyp info it worked if it ends with ok
npm ERR! gyp info using node-gyp@8.2.0
npm ERR! gyp info using node@16.12.0 | linux | x64
npm ERR! gyp info ok
npm ERR! gyp info it worked if it ends with ok
npm ERR! gyp info using node-gyp@8.2.0
npm ERR! gyp info using node@16.12.0 | linux | x64
npm ERR! gyp info find Python using Python version 3.8.10 found at "/usr/bin/python3"
npm ERR! gyp info spawn /usr/bin/python3
npm ERR! gyp info spawn args [
npm ERR! gyp info spawn args   '/usr/local/lib/node_modules/npm/node_modules/node-gyp/gyp/gyp_main.py',
npm ERR! gyp info spawn args   'binding.gyp',
npm ERR! gyp info spawn args   '-f',
npm ERR! gyp info spawn args   'make',
npm ERR! gyp info spawn args   '-I',
npm ERR! gyp info spawn args   '/src/backend/node_modules/sqlite3/build/config.gypi',
npm ERR! gyp info spawn args   '-I',
npm ERR! gyp info spawn args   '/usr/local/lib/node_modules/npm/node_modules/node-gyp/addon.gypi',
npm ERR! gyp info spawn args   '-I',
npm ERR! gyp info spawn args   '/root/.cache/node-gyp/16.12.0/include/node/common.gypi',
npm ERR! gyp info spawn args   '-Dlibrary=shared_library',
npm ERR! gyp info spawn args   '-Dvisibility=default',
npm ERR! gyp info spawn args   '-Dnode_root_dir=/root/.cache/node-gyp/16.12.0',
npm ERR! gyp info spawn args   '-Dnode_gyp_dir=/usr/local/lib/node_modules/npm/node_modules/node-gyp',
npm ERR! gyp info spawn args   '-Dnode_lib_file=/root/.cache/node-gyp/16.12.0/<(target_arch)/node.lib',
npm ERR! gyp info spawn args   '-Dmodule_root_dir=/src/backend/node_modules/sqlite3',
npm ERR! gyp info spawn args   '-Dnode_engine=v8',
npm ERR! gyp info spawn args   '--depth=.',
npm ERR! gyp info spawn args   '--no-parallel',
npm ERR! gyp info spawn args   '--generator-output',
npm ERR! gyp info spawn args   'build',
npm ERR! gyp info spawn args   '-Goutput_dir=.'
npm ERR! gyp info spawn args ]
npm ERR! gyp info ok
npm ERR! gyp info it worked if it ends with ok
npm ERR! gyp info using node-gyp@8.2.0
npm ERR! gyp info using node@16.12.0 | linux | x64
npm ERR! gyp info spawn make
npm ERR! gyp info spawn args [ 'BUILDTYPE=Release', '-C', 'build' ]
npm ERR! /bin/sh: python: not found
npm ERR! make: *** [deps/action_before_build.target.mk:13: Release/obj/gen/sqlite-autoconf-3310100/sqlite3.c] Error 127
npm ERR! gyp ERR! build error

```
a workaround for this is to install both `python` and `python3`. This can be improved further by splitting the build of the app into separate *build* container and only install `docker-cli` and `python3` in the application container

### Original Documentations

  * [Backend API](https://github.com/rakibtg/$IMAGE/tree/master/backend)
  * [Client](https://github.com/rakibtg/$IMAGE/tree/master/client)

### See Also

  * [top 6 GUI tools for managing Docker environments](https://www.upnxtblog.com/index.php/2018/01/17/top-6-gui-tools-for-managing-docker-environments/) - note the piblication is from 2018
  * [WPF UI to docker running on Windows](https://github.com/sonujose/docker-soul) - not very funcional
  * __PR__ [45](https://github.com/rakibtg/$IMAGE/pull/45/commits/a245814b2cd3ac30925b092a09f368d471e9d22b) to the original project solving the same problem but instead of switching to python image, install python via `apk`.  this way the image is size 489 MB versus 691MB with python based, and it is about the same time to build - 15 minute on low end developer machine
 * [setup Docker for Windows withut Docker Desktop](https://github.com/nebojsa-simic/docker-ftw)

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
