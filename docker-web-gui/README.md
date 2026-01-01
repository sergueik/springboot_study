### Info

This directory contains a close replica of the [rakibtg/docker-web-gui](https://github.com/rakibtg/docker-web-gui) app - a `node.js` alpine docker hosted web based GUI for managing Docker containers and images

### Modifications to the Project

* pinned the base image `node:alpine` to `node:8.12-alpine` and later to `node18/alpine:18.19.0`
* added `python` and `docker` dependencies install - the host `docker` application will not be runnable in musl container to allow reading the host inventory through socket which is exported volume mounted to the container during the run

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

* optionally run using host `docker`:
```sh
docker run -p 3230:3230 --name $NAME -v /usr/local/bin/docker:/usr/local/bin/docker -v /var/run/docker.sock:/var/run/docker.sock $NAME
```

```sh
pushd ../docker-web-gui
docker pull node18/alpine:18.19.0
docker-compose up --build --detach
```

> NOTE: the original project was providing an invalid run command which attempts to map both the docker socket and the binary from the host into the container. The socker is the correct way to let docker process running in container access host inventory, but the binary is not compatible
We install client locally in the contianer and only mount volume for socket

### Result

* images

![images](https://github.com/sergueik/springboot_study/blob/master/docker-web-gui/screenshots/capture-images.png)

* containers

![containers](https://github.com/sergueik/springboot_study/blob/master/docker-web-gui/screenshots/capture-containers.png)

### Cleanup

```sh
NAME=docker-web-gui
docker container stop $NAME
docker container rm $NAME
docker image rm $NAME
docker image rm node18/alpine:18.19.0
docker system prune -f
```

or
```sh
NAME=docker-web-gui
docker-compose stop
docker-compose rm -f
docker image ls | grep -i $NAME | awk '{print $3}' | cut -c1-4 | xargs docker image rm
docker system prune -f
```

### Troubleshooting

```txt
ERROR: for 179bbc654e53_docker-web-gui  'ContainerConfig'

ERROR: for docker-web-gui  'ContainerConfig'
Traceback (most recent call last):
  File "/usr/bin/docker-compose", line 33, in <module>
    sys.exit(load_entry_point('docker-compose==1.29.2', 'console_scripts', 'docker-compose')())
  File "/usr/lib/python3/dist-packages/compose/cli/main.py", line 81, in main
    command_func()
  File "/usr/lib/python3/dist-packages/compose/cli/main.py", line 203, in perform_command
    handler(command, command_options)
  File "/usr/lib/python3/dist-packages/compose/metrics/decorator.py", line 18, in wrapper
    result = fn(*args, **kwargs)
  File "/usr/lib/python3/dist-packages/compose/cli/main.py", line 1186, in up
    to_attach = up(False)
  File "/usr/lib/python3/dist-packages/compose/cli/main.py", line 1166, in up
    return self.project.up(
  File "/usr/lib/python3/dist-packages/compose/project.py", line 697, in up
    results, errors = parallel.parallel_execute(
  File "/usr/lib/python3/dist-packages/compose/parallel.py", line 108, in parallel_execute
    raise error_to_reraise
  File "/usr/lib/python3/dist-packages/compose/parallel.py", line 206, in producer
    result = func(obj)
  File "/usr/lib/python3/dist-packages/compose/project.py", line 679, in do
    return service.execute_convergence_plan(
  File "/usr/lib/python3/dist-packages/compose/service.py", line 579, in execute_convergence_plan
    return self._execute_convergence_recreate(
  File "/usr/lib/python3/dist-packages/compose/service.py", line 499, in _execute_convergence_recreate
    containers, errors = parallel_execute(
  File "/usr/lib/python3/dist-packages/compose/parallel.py", line 108, in parallel_execute
    raise error_to_reraise
  File "/usr/lib/python3/dist-packages/compose/parallel.py", line 206, in producer
    result = func(obj)
  File "/usr/lib/python3/dist-packages/compose/service.py", line 494, in recreate
    return self.recreate_container(
  File "/usr/lib/python3/dist-packages/compose/service.py", line 612, in recreate_container
    new_container = self.create_container(
  File "/usr/lib/python3/dist-packages/compose/service.py", line 330, in create_container
    container_options = self._get_container_create_options(
  File "/usr/lib/python3/dist-packages/compose/service.py", line 921, in _get_container_create_options
    container_options, override_options = self._build_container_volume_options(
  File "/usr/lib/python3/dist-packages/compose/service.py", line 960, in _build_container_volume_options
    binds, affinity = merge_volume_bindings(
  File "/usr/lib/python3/dist-packages/compose/service.py", line 1548, in merge_volume_bindings
    old_volumes, old_mounts = get_container_data_volumes(
  File "/usr/lib/python3/dist-packages/compose/service.py", line 1579, in get_container_data_volumes
    container.image_config['ContainerConfig'].get('Volumes') or {}
KeyError: 'ContainerConfig'
```

```sh
docker-compose ps
```
```text
     Name                   Command               State                    Ports                  
--------------------------------------------------------------------------------------------------
docker-web-gui   docker-entrypoint.sh node  ...   Up      0.0.0.0:3230->3230/tcp,:::3230->3230/tcp                                                          >3230/tcp
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

### Update `package-lock.json`

```sh
docker exec -it $NAME sh
```
```sh
cd /src/client/
npm i --package-lock-only
```	
```sh
docker cp $NAME:/src/client/package-lock.json ./client
```
### Repro Details of the Upstream Project

* this is copied from the PR [49](https://github.com/rakibtg/docker-web-gui/pull/49)
+ host os ubuntu 
```sh
lsb_release -a
```
```text
No LSB modules are available.
Distributor ID: Ubuntu
Description:    Ubuntu 18.04.6 LTS
Release:        18.04
Codename:       bionic
```

+ attempt to map host `docker` binary into container
```sh
which docker
```
```text
/usr/bin/docker	
```
+ run pinned version node alpine container
```sh
docker run -it -v /usr/bin/docker:/usr/bin/docker node:8.12-alpine sh
```
```text
Unable to find image 'node:8.12-alpine' locally
8.12-alpine: Pulling from library/node
4fe2ade4980c: Already exists
eeb7d76f44e7: Pull complete
e35f88fcc259: Pull complete
Digest: sha256:d75742c5fd41261113ed4706f961a21238db84648c825a5126ada373c361f46e
Status: Downloaded newer image for node:8.12-alpine
Digest: sha256:d75742c5fd41261113ed4706f961a21238db84648c825a5126ada373c361f46e
Status: Downloaded newer image for node:8.12-alpine
```
+ in the container see if `docker` is runnable
```sh
/ #
/ # ldd /usr/bin/docker
```
```text
 /lib64/ld-linux-x86-64.so.2 (0x7fd0646eb000)
 libpthread.so.0 => /lib64/ld-linux-x86-64.so.2 (0x7fd0646eb000)
 libdl.so.2 => /lib64/ld-linux-x86-64.so.2 (0x7fd0646eb000)
 libc.so.6 => /lib64/ld-linux-x86-64.so.2 (0x7fd0646eb000)
Error relocating /usr/bin/docker: __vfprintf_chk: symbol not found
Error relocating /usr/bin/docker: __fprintf_chk: symbol not found
```
+ try to run it anyway
```sh
/usr/bin/docker -v
```
```text
sh: /usr/bin/docker: not found
```
+ install `docker` via `apk`

NOTE: one has to stop the the container which has the host's `docker` mapped

```sh
rm -f /usr/bin/docker
```
```text
rm: can't remove '/usr/bin/docker': Resource busy
``` 
```sh
exit
```
+ run with mapped docker socket from host
```sh
docker run -it -v /var/run/docker.sock:/var/run/docker.sock node:8.12-alpine sh
```
+ in the container install docker:
```sh
/ # apk add docker
```

```text
fetch http://dl-cdn.alpinelinux.org/alpine/v3.8/main/x86_64/APKINDEX.tar.gz
fetch http://dl-cdn.alpinelinux.org/alpine/v3.8/community/x86_64/APKINDEX.tar.gz
(1/9) Installing ca-certificates (20191127-r2)
(2/9) Installing libmnl (1.0.4-r0)
(3/9) Installing jansson (2.11-r0)
(4/9) Installing libnftnl-libs (1.1.1-r0)
(5/9) Installing iptables (1.6.2-r0)
(6/9) Installing device-mapper-libs (2.02.178-r0)
(7/9) Installing libltdl (2.4.6-r5)
(8/9) Installing libseccomp (2.4.2-r2)
(9/9) Installing docker (18.06.1-r0)
Executing docker-18.06.1-r0.pre-install
Executing busybox-1.28.4-r1.trigger
Executing ca-certificates-20191127-r2.trigger
OK: 183 MiB in 24 packages
```
+  and list hosts images
```sh
docker container ls
```
```text
CONTAINER ID        IMAGE               COMMAND             CREATED              STATUS              PORTS               NAMES
78ac98862051        node:8.12-alpine    "sh"                About a minute ago   Up About a minute 
```

### Original Documentation

  * [Backend API](https://github.com/rakibtg/$IMAGE/tree/master/backend)
  * [Client](https://github.com/rakibtg/$IMAGE/tree/master/client)

### See Also

  * [top 6 GUI tools for managing Docker environments](https://www.upnxtblog.com/index.php/2018/01/17/top-6-gui-tools-for-managing-docker-environments/) - note the piblication is from 2018
  * [WPF UI to docker running on Windows](https://github.com/sonujose/docker-soul) - not very funcional
  * __PR__ [45](https://github.com/rakibtg/$IMAGE/pull/45/commits/a245814b2cd3ac30925b092a09f368d471e9d22b) to the original project solving the same problem but instead of switching to python image, install python via `apk`.  this way the image is size 489 MB versus 691MB with python based, and it is about the same time to build - 15 minute on low end developer machine
 * [setup Docker for Windows withut Docker Desktop](https://github.com/nebojsa-simic/docker-ftw)

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
