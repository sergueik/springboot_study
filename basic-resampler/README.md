### Info
project to test newly released Linux packages of [jniemann66/ReSampler](https://github.com/jniemann66/ReSampler)

### Usage

```sh
docker pull debian:bullseye
```

```sh
VERSION=2.1.2
```
```sh
IMAGE=basic-resampler
docker build --build-arg VERSION=2.1.0 -t $IMAGE -f Dockerfile.debian .
```
```text
Step 1/13 : FROM debian:bullseye
 ---> d89d848cffea
Step 2/13 : ENV  DEBIAN_FRONTEND=noninteractive
 ---> Using cache
 ---> c9feb6e9af26
Step 3/13 : ENV CODENAME=bullseye
 ---> Using cache
 ---> d5ef22c614cb
Step 4/13 : ARG VERSION=2.1.2
 ---> Using cache
 ---> 68b4f4622497
Step 5/13 : ENV URL="https://github.com/jniemann66/ReSampler/releases/download/v${VERSION}/ReSampler-${VERSION}-Linux.deb"
 ---> Using cache
 ---> eeed406e7b08
Step 6/13 : RUN grep -rl -E 'debian-security|bullseye-security'     /etc/apt/sources.list /etc/apt/sources.list.d/ /dev/null     | xargs -IX sed -i '/\(debian-security\|bullseye-security\)/d' X
 ---> Using cache
 ---> aace0881d273
Step 7/13 : RUN (grep -qrl -E 'debian-security|bullseye-security'     /etc/apt/sources.list /etc/apt/sources.list.d/ && echo 'still have';  exit 1) || exit 0
 ---> Using cache
 ---> 110572ddc374
Step 8/13 : RUN apt-get update
 ---> Using cache
 ---> d36363d3fa23
Step 9/13 : RUN apt-get install -y --no-install-recommends     ca-certificates curl libfftw3-double3 libsndfile1
 ---> Using cache
 ---> 34c50520693c
Step 10/13 : RUN curl --write-out 'HTTP %{http_code}\n' --fail --silent --output /dev/null $URL
 ---> Running in 6ecd41fc9b9c
HTTP 404
The command '/bin/sh -c curl --write-out 'HTTP %{http_code}\n' --fail --silent --output /dev/null $URL' returned a non-zero code: 22
```
the first version Linux releases were created is __2.1.2__
```sh
docker build --no-cache --progress=plain -t $IMAGE -f Dockerfile.debian .
```
```text
Sending build context to Docker daemon    150kB
Step 1/11 : FROM debian:bullseye
 ---> d89d848cffea
Step 2/11 : ENV  DEBIAN_FRONTEND=noninteractive
 ---> Running in b0c48d5125c5
Removing intermediate container b0c48d5125c5
 ---> e15a26d3a51a
Step 3/11 : ENV CODENAME=bullseye
 ---> Running in 3d182b6b2a22
Removing intermediate container 3d182b6b2a22
 ---> 5b08fcd23ec4
Step 4/11 : ARG VERSION=2.1.2
 ---> Running in 302e526d5d78
Removing intermediate container 302e526d5d78
 ---> 32fb407d70b8
Step 5/11 : RUN grep -rl -E 'debian-security|bullseye-security'     /etc/apt/sources.list /etc/apt/sources.list.d/ /dev/null     | xargs -IX sed -i '/\(debian-security\|bullseye-security\)/d' X
 ---> Running in 4652ba9581de
Removing intermediate container 4652ba9581de
 ---> 2f49dbf356cf
Step 6/11 : RUN (grep -qrl -E 'debian-security|bullseye-security'     /etc/apt/sources.list /etc/apt/sources.list.d/ && echo 'still have';  exit 1) || exit 0
 ---> Running in b3204a8d4515
Removing intermediate container b3204a8d4515
 ---> 11483afa36c8
Step 7/11 : RUN apt-get update
 ---> Running in af21b5ec9055
Get:1 http://deb.debian.org/debian bullseye InRelease [75.1 kB]
Get:2 http://deb.debian.org/debian bullseye-updates InRelease [44.0 kB]
Get:3 http://deb.debian.org/debian bullseye/main amd64 Packages [8066 kB]
Get:4 http://deb.debian.org/debian bullseye-updates/main amd64 Packages [18.8 kB]
Fetched 8204 kB in 2s (4260 kB/s)
Reading package lists...
Removing intermediate container af21b5ec9055
 ---> 13c045087f3e
Step 8/11 : RUN apt-get install -y --no-install-recommends     ca-certificates curl libfftw3-double3 libsndfile1
 ---> Running in 499daf7f6ae7
Reading package lists...
Building dependency tree...
Reading state information...
The following additional packages will be installed:
  libbrotli1 libcurl4 libflac8 libgomp1 libldap-2.4-2 libnghttp2-14 libogg0
  libopus0 libpsl5 librtmp1 libsasl2-2 libsasl2-modules-db libssh2-1
  libvorbis0a libvorbisenc2 openssl
Suggested packages:
  libfftw3-bin libfftw3-dev opus-tools
Recommended packages:
  libldap-common publicsuffix libsasl2-modules
The following NEW packages will be installed:
  ca-certificates curl libbrotli1 libcurl4 libfftw3-double3 libflac8 libgomp1
  libldap-2.4-2 libnghttp2-14 libogg0 libopus0 libpsl5 librtmp1 libsasl2-2
  libsasl2-modules-db libsndfile1 libssh2-1 libvorbis0a libvorbisenc2 openssl
0 upgraded, 20 newly installed, 0 to remove and 0 not upgraded.
Need to get 4196 kB of archives.
After this operation, 10.3 MB of additional disk space will be used.
Get:1 http://deb.debian.org/debian bullseye/main amd64 openssl amd64 1.1.1w-0+deb11u1 [859 kB]
Get:2 http://deb.debian.org/debian bullseye/main amd64 ca-certificates all 20210119 [158 kB]
Get:3 http://deb.debian.org/debian bullseye/main amd64 libbrotli1 amd64 1.0.9-2+b2 [279 kB]
Get:4 http://deb.debian.org/debian bullseye/main amd64 libsasl2-modules-db amd64 2.1.27+dfsg-2.1+deb11u1 [69.1 kB]
Get:5 http://deb.debian.org/debian bullseye/main amd64 libsasl2-2 amd64 2.1.27+dfsg-2.1+deb11u1 [106 kB]
Get:6 http://deb.debian.org/debian bullseye/main amd64 libldap-2.4-2 amd64 2.4.57+dfsg-3+deb11u1 [232 kB]
Get:7 http://deb.debian.org/debian bullseye/main amd64 libnghttp2-14 amd64 1.43.0-1+deb11u1 [77.2 kB]
Get:8 http://deb.debian.org/debian bullseye/main amd64 libpsl5 amd64 0.21.0-1.2 [57.3 kB]
Get:9 http://deb.debian.org/debian bullseye/main amd64 librtmp1 amd64 2.4+20151223.gitfa8646d.1-2+b2 [60.8 kB]
Get:10 http://deb.debian.org/debian bullseye/main amd64 libssh2-1 amd64 1.9.0-2+deb11u1 [156 kB]
Get:11 http://deb.debian.org/debian bullseye/main amd64 libcurl4 amd64 7.74.0-1.3+deb11u13 [347 kB]
Get:12 http://deb.debian.org/debian bullseye/main amd64 curl amd64 7.74.0-1.3+deb11u13 [271 kB]
Get:13 http://deb.debian.org/debian bullseye/main amd64 libgomp1 amd64 10.2.1-6 [99.9 kB]
Get:14 http://deb.debian.org/debian bullseye/main amd64 libfftw3-double3 amd64 3.3.8-2 [733 kB]
Get:15 http://deb.debian.org/debian bullseye/main amd64 libogg0 amd64 1.3.4-0.1 [27.3 kB]
Get:16 http://deb.debian.org/debian bullseye/main amd64 libflac8 amd64 1.3.3-2+deb11u2 [112 kB]
Get:17 http://deb.debian.org/debian bullseye/main amd64 libopus0 amd64 1.3.1-0.1 [190 kB]
Get:18 http://deb.debian.org/debian bullseye/main amd64 libvorbis0a amd64 1.3.7-1 [93.0 kB]
Get:19 http://deb.debian.org/debian bullseye/main amd64 libvorbisenc2 amd64 1.3.7-1 [80.6 kB]
Get:20 http://deb.debian.org/debian bullseye/main amd64 libsndfile1 amd64 1.0.31-2 [188 kB]
debconf: delaying package configuration, since apt-utils is not installed
Fetched 4196 kB in 1s (5467 kB/s)
Selecting previously unselected package openssl.
(Reading database ... 6673 files and directories currently installed.)
Preparing to unpack .../00-openssl_1.1.1w-0+deb11u1_amd64.deb ...
Unpacking openssl (1.1.1w-0+deb11u1) ...
Selecting previously unselected package ca-certificates.
Preparing to unpack .../01-ca-certificates_20210119_all.deb ...
Unpacking ca-certificates (20210119) ...
Selecting previously unselected package libbrotli1:amd64.
Preparing to unpack .../02-libbrotli1_1.0.9-2+b2_amd64.deb ...
Unpacking libbrotli1:amd64 (1.0.9-2+b2) ...
Selecting previously unselected package libsasl2-modules-db:amd64.
Preparing to unpack .../03-libsasl2-modules-db_2.1.27+dfsg-2.1+deb11u1_amd64.deb ...
Unpacking libsasl2-modules-db:amd64 (2.1.27+dfsg-2.1+deb11u1) ...
Selecting previously unselected package libsasl2-2:amd64.
Preparing to unpack .../04-libsasl2-2_2.1.27+dfsg-2.1+deb11u1_amd64.deb ...
Unpacking libsasl2-2:amd64 (2.1.27+dfsg-2.1+deb11u1) ...
Selecting previously unselected package libldap-2.4-2:amd64.
Preparing to unpack .../05-libldap-2.4-2_2.4.57+dfsg-3+deb11u1_amd64.deb ...
Unpacking libldap-2.4-2:amd64 (2.4.57+dfsg-3+deb11u1) ...
Selecting previously unselected package libnghttp2-14:amd64.
Preparing to unpack .../06-libnghttp2-14_1.43.0-1+deb11u1_amd64.deb ...
Unpacking libnghttp2-14:amd64 (1.43.0-1+deb11u1) ...
Selecting previously unselected package libpsl5:amd64.
Preparing to unpack .../07-libpsl5_0.21.0-1.2_amd64.deb ...
Unpacking libpsl5:amd64 (0.21.0-1.2) ...
Selecting previously unselected package librtmp1:amd64.
Preparing to unpack .../08-librtmp1_2.4+20151223.gitfa8646d.1-2+b2_amd64.deb ...
Unpacking librtmp1:amd64 (2.4+20151223.gitfa8646d.1-2+b2) ...
Selecting previously unselected package libssh2-1:amd64.
Preparing to unpack .../09-libssh2-1_1.9.0-2+deb11u1_amd64.deb ...
Unpacking libssh2-1:amd64 (1.9.0-2+deb11u1) ...
Selecting previously unselected package libcurl4:amd64.
Preparing to unpack .../10-libcurl4_7.74.0-1.3+deb11u13_amd64.deb ...
Unpacking libcurl4:amd64 (7.74.0-1.3+deb11u13) ...
Selecting previously unselected package curl.
Preparing to unpack .../11-curl_7.74.0-1.3+deb11u13_amd64.deb ...
Unpacking curl (7.74.0-1.3+deb11u13) ...
Selecting previously unselected package libgomp1:amd64.
Preparing to unpack .../12-libgomp1_10.2.1-6_amd64.deb ...
Unpacking libgomp1:amd64 (10.2.1-6) ...
Selecting previously unselected package libfftw3-double3:amd64.
Preparing to unpack .../13-libfftw3-double3_3.3.8-2_amd64.deb ...
Unpacking libfftw3-double3:amd64 (3.3.8-2) ...
Selecting previously unselected package libogg0:amd64.
Preparing to unpack .../14-libogg0_1.3.4-0.1_amd64.deb ...
Unpacking libogg0:amd64 (1.3.4-0.1) ...
Selecting previously unselected package libflac8:amd64.
Preparing to unpack .../15-libflac8_1.3.3-2+deb11u2_amd64.deb ...
Unpacking libflac8:amd64 (1.3.3-2+deb11u2) ...
Selecting previously unselected package libopus0:amd64.
Preparing to unpack .../16-libopus0_1.3.1-0.1_amd64.deb ...
Unpacking libopus0:amd64 (1.3.1-0.1) ...
Selecting previously unselected package libvorbis0a:amd64.
Preparing to unpack .../17-libvorbis0a_1.3.7-1_amd64.deb ...
Unpacking libvorbis0a:amd64 (1.3.7-1) ...
Selecting previously unselected package libvorbisenc2:amd64.
Preparing to unpack .../18-libvorbisenc2_1.3.7-1_amd64.deb ...
Unpacking libvorbisenc2:amd64 (1.3.7-1) ...
Selecting previously unselected package libsndfile1:amd64.
Preparing to unpack .../19-libsndfile1_1.0.31-2_amd64.deb ...
Unpacking libsndfile1:amd64 (1.0.31-2) ...
Setting up libpsl5:amd64 (0.21.0-1.2) ...
Setting up libogg0:amd64 (1.3.4-0.1) ...
Setting up libbrotli1:amd64 (1.0.9-2+b2) ...
Setting up libnghttp2-14:amd64 (1.43.0-1+deb11u1) ...
Setting up libgomp1:amd64 (10.2.1-6) ...
Setting up libflac8:amd64 (1.3.3-2+deb11u2) ...
Setting up libsasl2-modules-db:amd64 (2.1.27+dfsg-2.1+deb11u1) ...
Setting up librtmp1:amd64 (2.4+20151223.gitfa8646d.1-2+b2) ...
Setting up libopus0:amd64 (1.3.1-0.1) ...
Setting up libvorbis0a:amd64 (1.3.7-1) ...
Setting up libfftw3-double3:amd64 (3.3.8-2) ...
Setting up libsasl2-2:amd64 (2.1.27+dfsg-2.1+deb11u1) ...
Setting up libssh2-1:amd64 (1.9.0-2+deb11u1) ...
Setting up openssl (1.1.1w-0+deb11u1) ...
Setting up libvorbisenc2:amd64 (1.3.7-1) ...
Setting up libldap-2.4-2:amd64 (2.4.57+dfsg-3+deb11u1) ...
Setting up ca-certificates (20210119) ...
Updating certificates in /etc/ssl/certs...
129 added, 0 removed; done.
Setting up libcurl4:amd64 (7.74.0-1.3+deb11u13) ...
Setting up curl (7.74.0-1.3+deb11u13) ...
Setting up libsndfile1:amd64 (1.0.31-2) ...
Processing triggers for libc-bin (2.31-13+deb11u14) ...
Processing triggers for ca-certificates (20210119) ...
Updating certificates in /etc/ssl/certs...
0 added, 0 removed; done.
Running hooks in /etc/ca-certificates/update.d...
done.
Removing intermediate container 499daf7f6ae7
 ---> dd6187e17bb8
Step 9/11 : RUN curl -fL -o /tmp/resampler.deb     "https://github.com/jniemann66/ReSampler/releases/download/v${VERSION}/ReSampler-${VERSION}-Linux.deb"
 ---> Running in eb7d6108f09f
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
  0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0
100 10730  100 10730    0     0  18991      0 --:--:-- --:--:-- --:--:-- 18991
Removing intermediate container eb7d6108f09f
 ---> 205e3dbee240
Step 10/11 : RUN apt-get install -y --no-install-recommends /tmp/resampler.deb
 ---> Running in 0f1c5576b61d
Reading package lists...
Building dependency tree...
Reading state information...
The following NEW packages will be installed:
  resampler
0 upgraded, 1 newly installed, 0 to remove and 0 not upgraded.
Need to get 0 B/10.7 kB of archives.
After this operation, 48.1 kB of additional disk space will be used.
Get:1 /tmp/resampler.deb resampler amd64 2.1.2 [10.7 kB]
debconf: delaying package configuration, since apt-utils is not installed
Selecting previously unselected package resampler.
(Reading database ... 7117 files and directories currently installed.)
Preparing to unpack /tmp/resampler.deb ...
Unpacking resampler (2.1.2) ...
Setting up resampler (2.1.2) ...
Removing intermediate container 0f1c5576b61d
 ---> 14490ff98c84
Step 11/11 : RUN dpkg -L resampler     && ldd /usr/bin/ReSampler
 ---> Running in 56afe8acae1d
/usr
/usr/bin
/usr/bin/ReSampler
        linux-vdso.so.1 (0x00007ffdb1bf5000)
        libReSamplerLib.so => not found
        libstdc++.so.6 => /usr/lib/x86_64-linux-gnu/libstdc++.so.6 (0x00007ff56e422000)
        libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x00007ff56e408000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007ff56e234000)
        libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007ff56e0f0000)
        /lib64/ld-linux-x86-64.so.2 (0x00007ff56e5fc000)
Removing intermediate container 56afe8acae1d
 ---> 8db1ab370f15
Successfully built 8db1ab370f15
Successfully tagged basic-resampler:latest
```
```sh
docker run -it  $IMAGE sh
```
```sh
dpkg -L resampler
```
```text
/usr
/usr/bin
/usr/bin/ReSampler
```
```sh
ldd /usr/bin/ReSampler
```
```text
  linux-vdso.so.1 (0x00007ffc207de000)
  libReSamplerLib.so => not found
  libstdc++.so.6 => /usr/lib/x86_64-linux-gnu/libstdc++.so.6 (0x00007f48f2bb2000)
  libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x00007f48f2b98000)
  libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f48f29c4000)
  libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f48f2880000)
  /lib64/ld-linux-x86-64.so.2 (0x00007f48f2d8c000)
```

```sh
find /usr /lib -name 'libReSamplerLib.so*' -print
```
none
```sh
dpkg -s resampler
```
```text
Package: resampler
Status: install ok installed
Priority: optional
Section: sound
Installed-Size: 47
Maintainer: jniemann66
Architecture: amd64
Version: 2.1.2
Depends: libsndfile1 (>= 1.0), libfftw3-double3 (>= 3.3)
Description: High-quality audio sample rate converter
```
```sh
dpkg-deb -c /tmp/ReSampler-Linux.deb
```
```text
drwxrwxr-x root/root         0 2026-04-03 03:22 ./usr/
drwxrwxr-x root/root         0 2026-04-03 03:22 ./usr/bin/
-rwxr-xr-x root/root     39664 2026-04-03 03:22 ./usr/bin/ReSampler
```

```sh
dpkg-deb -I /tmp/ReSampler-Linux.deb
```
```text
new Debian package, version 2.0.
 size 10730 bytes: control archive=349 bytes.
     241 bytes,     9 lines      control
      52 bytes,     1 lines      md5sums
 Architecture: amd64
 Depends: libsndfile1 (>= 1.0), libfftw3-double3 (>= 3.3)
 Description: High-quality audio sample rate converter
 Maintainer: jniemann66
 Package: resampler
 Priority: optional
 Section: sound
 Version: 2.1.2
 Installed-Size: 47
```

```sh
docker pull quay.io/centos/centos:stream9
```
```sh
docker build --build-arg VERSION=2.1.2 -t $IMAGE -f Dockerfile.centos  .
```
```textSending build context to Docker daemon  83.97kB
Step 1/9 : FROM quay.io/centos/centos:stream9
 ---> 7262e51b7bb9
Step 2/9 : ARG VERSION=2.1.2
 ---> Using cache
 ---> 0db3ed8b2367
Step 3/9 : ENV VERSION=${VERSION}
 ---> Using cache
 ---> 413353c31341
Step 4/9 : ENV URL=https://github.com/jniemann66/ReSampler/releases/download/v${VERSION}/ReSampler-${VERSION}-Linux.rpm
 ---> Using cache
 ---> a8439bb90359
Step 5/9 : RUN curl --write-out 'HTTP %{http_code}\n' --fail --silent --output /dev/null "$URL"
 ---> Running in 3e054bddb848
HTTP 302
Removing intermediate container 3e054bddb848
 ---> 59a431154eed
Step 6/9 : RUN dnf install -y     ca-certificates     fftw     fftw-libs-double     libsndfile     rpm-build     cpio     && dnf clean all
 ---> Running in 2db75dacf639
CentOS Stream 9 - BaseOS                        2.2 MB/s | 2.9 MB     00:01    
CentOS Stream 9 - AppStream                     6.2 MB/s |  13 MB     00:02    
CentOS Stream 9 - Extras packages               3.3 kB/s |  22 kB     00:06    
Package ca-certificates-2026.2.92_v9.0.319-91.3.el9.noarch is already installed.
Dependencies resolved.
================================================================================
 Package                      Arch     Version                Repository   Size
================================================================================
Installing:
 cpio                         x86_64   2.13-16.el9            baseos      275 k
 fftw                         x86_64   3.3.8-14.el9           appstream    44 k
 fftw-libs-double             x86_64   3.3.8-14.el9           appstream   909 k
 libsndfile                   x86_64   1.0.31-10.el9          appstream   205 k
 rpm-build                    x86_64   4.16.1.3-42.el9        appstream    65 k
Installing dependencies:
 binutils                     x86_64   2.35.2-72.el9          baseos      4.6 M
 binutils-gold                x86_64   2.35.2-72.el9          baseos      734 k
 bzip2                        x86_64   1.0.8-11.el9           baseos       55 k
 debugedit                    x86_64   5.0-11.el9             appstream    77 k
 diffutils                    x86_64   3.7-12.el9             baseos      397 k
 dwz                          x86_64   0.16-1.el9             appstream   134 k
 ed                           x86_64   1.14.2-12.el9          baseos       75 k
 efi-srpm-macros              noarch   6-4.el9                appstream    21 k
 elfutils                     x86_64   0.197-1.el9            baseos      576 k
 elfutils-debuginfod-client   x86_64   0.197-1.el9            baseos       45 k
 fftw-libs-long               x86_64   3.3.8-14.el9           appstream   503 k
 fftw-libs-quad               x86_64   3.3.8-14.el9           appstream   876 k
 fftw-libs-single             x86_64   3.3.8-14.el9           appstream   935 k
 file                         x86_64   5.39-19.el9            baseos       48 k
 flac-libs                    x86_64   1.3.3-12.el9           appstream   218 k
 fonts-srpm-macros            noarch   1:2.0.5-7.el9.1        appstream    28 k
 gdb-minimal                  x86_64   17.2-4.el9             appstream   4.5 M
 ghc-srpm-macros              noarch   1.5.0-6.el9            appstream   8.8 k
 glibc-gconv-extra            x86_64   2.34-290.el9           baseos      1.7 M
 go-srpm-macros               noarch   3.8.1-2.el9            appstream    26 k
 groff-base                   x86_64   1.22.4-10.el9          baseos      1.1 M
 gsm                          x86_64   1.0.19-6.el9           appstream    34 k
 info                         x86_64   6.7-15.el9             baseos      225 k
 kernel-srpm-macros           noarch   1.0-16.el9             appstream    14 k
 libogg                       x86_64   2:1.3.4-6.el9          appstream    34 k
 libpkgconf                   x86_64   1.7.3-10.el9           baseos       36 k
 libquadmath                  x86_64   11.5.0-16.el9          baseos      181 k
 libvorbis                    x86_64   1:1.3.7-5.el9          appstream   193 k
 lua-srpm-macros              noarch   1-6.el9                appstream   9.5 k
 ncurses                      x86_64   6.2-12.20210508.el9    baseos      406 k
 ocaml-srpm-macros            noarch   6-6.el9                appstream   8.8 k
 openblas-srpm-macros         noarch   2-11.el9               appstream   8.4 k
 opus                         x86_64   1.3.1-10.el9           appstream   200 k
 patch                        x86_64   2.7.6-16.el9           appstream   128 k
 perl-AutoLoader              noarch   5.74-484.el9           appstream    20 k
 perl-B                       x86_64   1.80-484.el9           appstream   179 k
 perl-Carp                    noarch   1.50-460.el9           appstream    30 k
 perl-Class-Struct            noarch   0.66-484.el9           appstream    21 k
 perl-Data-Dumper             x86_64   2.174-462.el9          appstream    56 k
 perl-Digest                  noarch   1.19-4.el9             appstream    26 k
 perl-Digest-MD5              x86_64   2.58-4.el9             appstream    37 k
 perl-Encode                  x86_64   4:3.08-462.el9         appstream   1.7 M
 perl-Errno                   x86_64   1.30-484.el9           appstream    13 k
 perl-Exporter                noarch   5.74-461.el9           appstream    32 k
 perl-Fcntl                   x86_64   1.13-484.el9           appstream    19 k
 perl-File-Basename           noarch   2.85-484.el9           appstream    16 k
 perl-File-Path               noarch   2.18-4.el9             appstream    36 k
 perl-File-Temp               noarch   1:0.231.100-4.el9      appstream    60 k
 perl-File-stat               noarch   1.09-484.el9           appstream    16 k
 perl-FileHandle              noarch   2.03-484.el9           appstream    14 k
 perl-Getopt-Long             noarch   1:2.52-4.el9           appstream    61 k
 perl-Getopt-Std              noarch   1.12-484.el9           appstream    14 k
 perl-HTTP-Tiny               noarch   0.076-462.el9          appstream    54 k
 perl-IO                      x86_64   1.43-484.el9           appstream    88 k
 perl-IO-Socket-IP            noarch   0.41-5.el9             appstream    43 k
 perl-IO-Socket-SSL           noarch   2.073-2.el9            appstream   217 k
 perl-IPC-Open3               noarch   1.21-484.el9           appstream    21 k
 perl-MIME-Base64             x86_64   3.16-4.el9             appstream    31 k
 perl-Mozilla-CA              noarch   20200520-6.el9         appstream    13 k
 perl-Net-SSLeay              x86_64   1.94-4.el9             appstream   412 k
 perl-POSIX                   x86_64   1.94-484.el9           appstream    95 k
 perl-PathTools               x86_64   3.78-461.el9           appstream    88 k
 perl-Pod-Escapes             noarch   1:1.07-460.el9         appstream    21 k
 perl-Pod-Perldoc             noarch   3.28.01-461.el9        appstream    87 k
 perl-Pod-Simple              noarch   1:3.42-4.el9           appstream   225 k
 perl-Pod-Usage               noarch   4:2.01-4.el9           appstream    41 k
 perl-Scalar-List-Utils       x86_64   4:1.56-462.el9         appstream    71 k
 perl-SelectSaver             noarch   1.02-484.el9           appstream    10 k
 perl-Socket                  x86_64   4:2.031-4.el9          appstream    56 k
 perl-Storable                x86_64   1:3.21-460.el9         appstream    96 k
 perl-Symbol                  noarch   1.08-484.el9           appstream    13 k
 perl-Term-ANSIColor          noarch   5.01-461.el9           appstream    49 k
 perl-Term-Cap                noarch   1.17-460.el9           appstream    23 k
 perl-Text-ParseWords         noarch   3.30-460.el9           appstream    17 k
 perl-Text-Tabs+Wrap          noarch   2013.0523-460.el9      appstream    24 k
 perl-Time-Local              noarch   2:1.300-7.el9          appstream    34 k
 perl-URI                     noarch   5.09-4.el9             appstream   120 k
 perl-Unicode-Normalize       x86_64   1.27-461.el9           appstream    91 k
 perl-base                    noarch   2.27-484.el9           appstream    15 k
 perl-constant                noarch   1.33-461.el9           appstream    24 k
 perl-if                      noarch   0.60.800-484.el9       appstream    13 k
 perl-interpreter             x86_64   4:5.32.1-484.el9       appstream    69 k
 perl-libnet                  noarch   3.13-4.el9             appstream   130 k
 perl-libs                    x86_64   4:5.32.1-484.el9       appstream   2.2 M
 perl-mro                     x86_64   1.23-484.el9           appstream    27 k
 perl-overload                noarch   1.31-484.el9           appstream    44 k
 perl-overloading             noarch   0.02-484.el9           appstream    11 k
 perl-parent                  noarch   1:0.238-460.el9        appstream    15 k
 perl-podlators               noarch   1:4.14-460.el9         appstream   114 k
 perl-srpm-macros             noarch   1-41.el9               appstream   9.1 k
 perl-subs                    noarch   1.03-484.el9           appstream    10 k
 perl-vars                    noarch   1.05-484.el9           appstream    12 k
 pkgconf                      x86_64   1.7.3-10.el9           baseos       41 k
 pkgconf-m4                   noarch   1.7.3-10.el9           baseos       15 k
 pkgconf-pkg-config           x86_64   1.7.3-10.el9           baseos       11 k
 pyproject-srpm-macros        noarch   1.23.2-1.el9           appstream    15 k
 python-srpm-macros           noarch   3.9-54.el9             appstream    18 k
 qt5-srpm-macros              noarch   5.15.9-1.el9           appstream   8.8 k
 redhat-rpm-config            noarch   211-1.el9              appstream    69 k
 rust-srpm-macros             noarch   17-4.el9               appstream    10 k
 unzip                        x86_64   6.0-59.el9             baseos      182 k
 xz                           x86_64   5.2.5-10.el9           baseos      225 k
 zip                          x86_64   3.0-35.el9             baseos      266 k
 zstd                         x86_64   1.5.5-1.el9            baseos      464 k
Installing weak dependencies:
 perl-NDBM_File               x86_64   1.15-484.el9           appstream    21 k

Transaction Summary
================================================================================
Install  110 Packages

Total download size: 28 M
Installed size: 103 M
Downloading Packages:
(1/110): bzip2-1.0.8-11.el9.x86_64.rpm          141 kB/s |  55 kB     00:00    
(2/110): binutils-gold-2.35.2-72.el9.x86_64.rpm 1.0 MB/s | 734 kB     00:00    
(3/110): cpio-2.13-16.el9.x86_64.rpm            676 kB/s | 275 kB     00:00    
(4/110): ed-1.14.2-12.el9.x86_64.rpm            578 kB/s |  75 kB     00:00    
(5/110): diffutils-3.7-12.el9.x86_64.rpm        1.1 MB/s | 397 kB     00:00    
(6/110): elfutils-debuginfod-client-0.197-1.el9 189 kB/s |  45 kB     00:00    
(7/110): elfutils-0.197-1.el9.x86_64.rpm        1.2 MB/s | 576 kB     00:00    
(8/110): file-5.39-19.el9.x86_64.rpm            299 kB/s |  48 kB     00:00    
(9/110): binutils-2.35.2-72.el9.x86_64.rpm      1.9 MB/s | 4.6 MB     00:02    
(10/110): groff-base-1.22.4-10.el9.x86_64.rpm   1.0 MB/s | 1.1 MB     00:01    
(11/110): libpkgconf-1.7.3-10.el9.x86_64.rpm    303 kB/s |  36 kB     00:00    
(12/110): glibc-gconv-extra-2.34-290.el9.x86_64 1.2 MB/s | 1.7 MB     00:01    
(13/110): info-6.7-15.el9.x86_64.rpm            725 kB/s | 225 kB     00:00    
(14/110): libquadmath-11.5.0-16.el9.x86_64.rpm  1.1 MB/s | 181 kB     00:00    
(15/110): pkgconf-m4-1.7.3-10.el9.noarch.rpm    158 kB/s |  15 kB     00:00    
(16/110): pkgconf-1.7.3-10.el9.x86_64.rpm       248 kB/s |  41 kB     00:00    
(17/110): pkgconf-pkg-config-1.7.3-10.el9.x86_6 102 kB/s |  11 kB     00:00    
(18/110): ncurses-6.2-12.20210508.el9.x86_64.rp 1.1 MB/s | 406 kB     00:00    
(19/110): unzip-6.0-59.el9.x86_64.rpm           799 kB/s | 182 kB     00:00    
(20/110): zip-3.0-35.el9.x86_64.rpm             954 kB/s | 266 kB     00:00    
(21/110): xz-5.2.5-10.el9.x86_64.rpm            574 kB/s | 225 kB     00:00    
(22/110): zstd-1.5.5-1.el9.x86_64.rpm           1.6 MB/s | 464 kB     00:00    
(23/110): efi-srpm-macros-6-4.el9.noarch.rpm     91 kB/s |  21 kB     00:00    
(24/110): debugedit-5.0-11.el9.x86_64.rpm       209 kB/s |  77 kB     00:00    
(25/110): fftw-3.3.8-14.el9.x86_64.rpm          192 kB/s |  44 kB     00:00    
(26/110): fftw-libs-long-3.3.8-14.el9.x86_64.rp 877 kB/s | 503 kB     00:00    
(27/110): dwz-0.16-1.el9.x86_64.rpm             119 kB/s | 134 kB     00:01    
(28/110): fftw-libs-double-3.3.8-14.el9.x86_64. 1.1 MB/s | 909 kB     00:00    
(29/110): flac-libs-1.3.3-12.el9.x86_64.rpm     891 kB/s | 218 kB     00:00    
(30/110): fonts-srpm-macros-2.0.5-7.el9.1.noarc 150 kB/s |  28 kB     00:00    
(31/110): fftw-libs-quad-3.3.8-14.el9.x86_64.rp 1.3 MB/s | 876 kB     00:00    
(32/110): fftw-libs-single-3.3.8-14.el9.x86_64. 1.4 MB/s | 935 kB     00:00    
(33/110): ghc-srpm-macros-1.5.0-6.el9.noarch.rp  83 kB/s | 8.8 kB     00:00    
(34/110): gsm-1.0.19-6.el9.x86_64.rpm           259 kB/s |  34 kB     00:00    
(35/110): kernel-srpm-macros-1.0-16.el9.noarch. 109 kB/s |  14 kB     00:00    
(36/110): go-srpm-macros-3.8.1-2.el9.noarch.rpm  81 kB/s |  26 kB     00:00    
(37/110): libogg-1.3.4-6.el9.x86_64.rpm         231 kB/s |  34 kB     00:00    
(38/110): libsndfile-1.0.31-10.el9.x86_64.rpm   691 kB/s | 205 kB     00:00    
(39/110): libvorbis-1.3.7-5.el9.x86_64.rpm      767 kB/s | 193 kB     00:00    
(40/110): lua-srpm-macros-1-6.el9.noarch.rpm     68 kB/s | 9.5 kB     00:00    
(41/110): ocaml-srpm-macros-6-6.el9.noarch.rpm   68 kB/s | 8.8 kB     00:00    
(42/110): openblas-srpm-macros-2-11.el9.noarch.  52 kB/s | 8.4 kB     00:00    
(43/110): opus-1.3.1-10.el9.x86_64.rpm          950 kB/s | 200 kB     00:00    
(44/110): patch-2.7.6-16.el9.x86_64.rpm         506 kB/s | 128 kB     00:00    
(45/110): perl-AutoLoader-5.74-484.el9.noarch.r  99 kB/s |  20 kB     00:00    
(46/110): gdb-minimal-17.2-4.el9.x86_64.rpm     2.7 MB/s | 4.5 MB     00:01    
(47/110): perl-Carp-1.50-460.el9.noarch.rpm     132 kB/s |  30 kB     00:00    
(48/110): perl-B-1.80-484.el9.x86_64.rpm        595 kB/s | 179 kB     00:00    
(49/110): perl-Class-Struct-0.66-484.el9.noarch 152 kB/s |  21 kB     00:00    
(50/110): perl-Data-Dumper-2.174-462.el9.x86_64 347 kB/s |  56 kB     00:00    
(51/110): perl-Digest-1.19-4.el9.noarch.rpm     171 kB/s |  26 kB     00:00    
(52/110): perl-Digest-MD5-2.58-4.el9.x86_64.rpm 278 kB/s |  37 kB     00:00    
(53/110): perl-Errno-1.30-484.el9.x86_64.rpm    116 kB/s |  13 kB     00:00    
(54/110): perl-Exporter-5.74-461.el9.noarch.rpm 243 kB/s |  32 kB     00:00    
(55/110): perl-Fcntl-1.13-484.el9.x86_64.rpm    124 kB/s |  19 kB     00:00    
(56/110): perl-File-Basename-2.85-484.el9.noarc  84 kB/s |  16 kB     00:00    
(57/110): perl-File-Path-2.18-4.el9.noarch.rpm  240 kB/s |  36 kB     00:00    
(58/110): perl-File-Temp-0.231.100-4.el9.noarch 294 kB/s |  60 kB     00:00    
(59/110): perl-File-stat-1.09-484.el9.noarch.rp  87 kB/s |  16 kB     00:00    
(60/110): perl-FileHandle-2.03-484.el9.noarch.r  93 kB/s |  14 kB     00:00    
(61/110): perl-Getopt-Long-2.52-4.el9.noarch.rp 251 kB/s |  61 kB     00:00    
(62/110): perl-Encode-3.08-462.el9.x86_64.rpm   1.8 MB/s | 1.7 MB     00:00    
(63/110): perl-Getopt-Std-1.12-484.el9.noarch.r  82 kB/s |  14 kB     00:00    
(64/110): perl-HTTP-Tiny-0.076-462.el9.noarch.r 350 kB/s |  54 kB     00:00    
(65/110): perl-IO-Socket-IP-0.41-5.el9.noarch.r 147 kB/s |  43 kB     00:00    
(66/110): perl-IO-1.43-484.el9.x86_64.rpm       278 kB/s |  88 kB     00:00    
(67/110): perl-IO-Socket-SSL-2.073-2.el9.noarch 708 kB/s | 217 kB     00:00    
(68/110): perl-IPC-Open3-1.21-484.el9.noarch.rp 165 kB/s |  21 kB     00:00    
(69/110): perl-MIME-Base64-3.16-4.el9.x86_64.rp 193 kB/s |  31 kB     00:00    
(70/110): perl-Mozilla-CA-20200520-6.el9.noarch 114 kB/s |  13 kB     00:00    
(71/110): perl-NDBM_File-1.15-484.el9.x86_64.rp 187 kB/s |  21 kB     00:00    
(72/110): perl-PathTools-3.78-461.el9.x86_64.rp 323 kB/s |  88 kB     00:00    
(73/110): perl-POSIX-1.94-484.el9.x86_64.rpm    310 kB/s |  95 kB     00:00    
(74/110): perl-Pod-Escapes-1.07-460.el9.noarch. 104 kB/s |  21 kB     00:00    
(75/110): perl-Pod-Perldoc-3.28.01-461.el9.noar 195 kB/s |  87 kB     00:00    
(76/110): perl-Net-SSLeay-1.94-4.el9.x86_64.rpm 506 kB/s | 412 kB     00:00    
(77/110): perl-Pod-Usage-2.01-4.el9.noarch.rpm  284 kB/s |  41 kB     00:00    
(78/110): perl-SelectSaver-1.02-484.el9.noarch.  51 kB/s |  10 kB     00:00    
(79/110): perl-Socket-2.031-4.el9.x86_64.rpm    260 kB/s |  56 kB     00:00    
(80/110): perl-Scalar-List-Utils-1.56-462.el9.x 120 kB/s |  71 kB     00:00    
(81/110): perl-Storable-3.21-460.el9.x86_64.rpm 593 kB/s |  96 kB     00:00    
(82/110): perl-Symbol-1.08-484.el9.noarch.rpm    80 kB/s |  13 kB     00:00    
(83/110): perl-Pod-Simple-3.42-4.el9.noarch.rpm 206 kB/s | 225 kB     00:01    
(84/110): perl-Term-ANSIColor-5.01-461.el9.noar 273 kB/s |  49 kB     00:00    
(85/110): perl-Term-Cap-1.17-460.el9.noarch.rpm 152 kB/s |  23 kB     00:00    
(86/110): perl-Text-ParseWords-3.30-460.el9.noa 132 kB/s |  17 kB     00:00    
(87/110): perl-Text-Tabs+Wrap-2013.0523-460.el9 168 kB/s |  24 kB     00:00    
(88/110): perl-Time-Local-1.300-7.el9.noarch.rp 200 kB/s |  34 kB     00:00    
(89/110): perl-Unicode-Normalize-1.27-461.el9.x 569 kB/s |  91 kB     00:00    
(90/110): perl-base-2.27-484.el9.noarch.rpm     111 kB/s |  15 kB     00:00    
(91/110): perl-if-0.60.800-484.el9.noarch.rpm   108 kB/s |  13 kB     00:00    
(92/110): perl-constant-1.33-461.el9.noarch.rpm 174 kB/s |  24 kB     00:00    
(93/110): perl-URI-5.09-4.el9.noarch.rpm        245 kB/s | 120 kB     00:00    
(94/110): perl-interpreter-5.32.1-484.el9.x86_6 401 kB/s |  69 kB     00:00    
(95/110): perl-mro-1.23-484.el9.x86_64.rpm      200 kB/s |  27 kB     00:00    
(96/110): perl-libnet-3.13-4.el9.noarch.rpm     327 kB/s | 130 kB     00:00    
(97/110): perl-overload-1.31-484.el9.noarch.rpm 318 kB/s |  44 kB     00:00    
(98/110): perl-overloading-0.02-484.el9.noarch.  89 kB/s |  11 kB     00:00    
(99/110): perl-parent-0.238-460.el9.noarch.rpm  141 kB/s |  15 kB     00:00    
(100/110): perl-srpm-macros-1-41.el9.noarch.rpm  46 kB/s | 9.1 kB     00:00    
(101/110): perl-podlators-4.14-460.el9.noarch.r 315 kB/s | 114 kB     00:00    
(102/110): perl-subs-1.03-484.el9.noarch.rpm     68 kB/s |  10 kB     00:00    
(103/110): perl-vars-1.05-484.el9.noarch.rpm    114 kB/s |  12 kB     00:00    
(104/110): pyproject-srpm-macros-1.23.2-1.el9.n 146 kB/s |  15 kB     00:00    
(105/110): qt5-srpm-macros-5.15.9-1.el9.noarch.  95 kB/s | 8.8 kB     00:00    
(106/110): python-srpm-macros-3.9-54.el9.noarch 154 kB/s |  18 kB     00:00    
(107/110): redhat-rpm-config-211-1.el9.noarch.r 253 kB/s |  69 kB     00:00    
(108/110): rpm-build-4.16.1.3-42.el9.x86_64.rpm 237 kB/s |  65 kB     00:00    
(109/110): rust-srpm-macros-17-4.el9.noarch.rpm 101 kB/s |  10 kB     00:00    
(110/110): perl-libs-5.32.1-484.el9.x86_64.rpm  402 kB/s | 2.2 MB     00:05    
--------------------------------------------------------------------------------
Total                                           1.7 MB/s |  28 MB     00:16     
CentOS Stream 9 - BaseOS                        1.6 MB/s | 1.6 kB     00:00    
Importing GPG key 0x8483C65D:
 Userid     : "CentOS (CentOS Official Signing Key) <security@centos.org>"
 Fingerprint: 99DB 70FA E1D7 CE22 7FB6 4882 05B5 55B3 8483 C65D
 From       : /etc/pki/rpm-gpg/RPM-GPG-KEY-centosofficial
Key imported successfully
Running transaction check
Transaction check succeeded.
Running transaction test
Transaction test succeeded.
Running transaction
  Preparing        :                                                        1/1 
  Installing       : elfutils-debuginfod-client-0.197-1.el9.x86_64        1/110 
  Installing       : binutils-gold-2.35.2-72.el9.x86_64                   2/110 
  Installing       : binutils-2.35.2-72.el9.x86_64                        3/110 
  Running scriptlet: binutils-2.35.2-72.el9.x86_64                        3/110 
  Installing       : libogg-2:1.3.4-6.el9.x86_64                          4/110 
  Installing       : elfutils-0.197-1.el9.x86_64                          5/110 
  Installing       : gdb-minimal-17.2-4.el9.x86_64                        6/110 
  Installing       : dwz-0.16-1.el9.x86_64                                7/110 
  Installing       : xz-5.2.5-10.el9.x86_64                               8/110 
  Installing       : unzip-6.0-59.el9.x86_64                              9/110 
  Installing       : file-5.39-19.el9.x86_64                             10/110 
  Installing       : zip-3.0-35.el9.x86_64                               11/110 
  Installing       : debugedit-5.0-11.el9.x86_64                         12/110 
  Installing       : flac-libs-1.3.3-12.el9.x86_64                       13/110 
  Installing       : libvorbis-1:1.3.7-5.el9.x86_64                      14/110 
  Installing       : rust-srpm-macros-17-4.el9.noarch                    15/110 
  Installing       : qt5-srpm-macros-5.15.9-1.el9.noarch                 16/110 
  Installing       : perl-srpm-macros-1-41.el9.noarch                    17/110 
  Installing       : opus-1.3.1-10.el9.x86_64                            18/110 
  Installing       : openblas-srpm-macros-2-11.el9.noarch                19/110 
  Installing       : ocaml-srpm-macros-6-6.el9.noarch                    20/110 
  Installing       : lua-srpm-macros-1-6.el9.noarch                      21/110 
  Installing       : gsm-1.0.19-6.el9.x86_64                             22/110 
  Installing       : ghc-srpm-macros-1.5.0-6.el9.noarch                  23/110 
  Installing       : fftw-libs-single-3.3.8-14.el9.x86_64                24/110 
  Installing       : fftw-libs-long-3.3.8-14.el9.x86_64                  25/110 
  Installing       : fftw-libs-double-3.3.8-14.el9.x86_64                26/110 
  Installing       : efi-srpm-macros-6-4.el9.noarch                      27/110 
  Installing       : zstd-1.5.5-1.el9.x86_64                             28/110 
  Installing       : pkgconf-m4-1.7.3-10.el9.noarch                      29/110 
  Installing       : ncurses-6.2-12.20210508.el9.x86_64                  30/110 
  Installing       : libquadmath-11.5.0-16.el9.x86_64                    31/110 
  Installing       : fftw-libs-quad-3.3.8-14.el9.x86_64                  32/110 
  Installing       : libpkgconf-1.7.3-10.el9.x86_64                      33/110 
  Installing       : pkgconf-1.7.3-10.el9.x86_64                         34/110 
  Installing       : pkgconf-pkg-config-1.7.3-10.el9.x86_64              35/110 
  Installing       : info-6.7-15.el9.x86_64                              36/110 
  Installing       : ed-1.14.2-12.el9.x86_64                             37/110 
  Installing       : patch-2.7.6-16.el9.x86_64                           38/110 
  Running scriptlet: groff-base-1.22.4-10.el9.x86_64                     39/110 
  Installing       : groff-base-1.22.4-10.el9.x86_64                     39/110 
  Running scriptlet: groff-base-1.22.4-10.el9.x86_64                     39/110 
  Installing       : perl-Digest-1.19-4.el9.noarch                       40/110 
  Installing       : perl-Digest-MD5-2.58-4.el9.x86_64                   41/110 
  Installing       : perl-B-1.80-484.el9.x86_64                          42/110 
  Installing       : perl-FileHandle-2.03-484.el9.noarch                 43/110 
  Installing       : perl-Unicode-Normalize-1.27-461.el9.x86_64          44/110 
  Installing       : perl-Data-Dumper-2.174-462.el9.x86_64               45/110 
  Installing       : perl-libnet-3.13-4.el9.noarch                       46/110 
  Installing       : perl-AutoLoader-5.74-484.el9.noarch                 47/110 
  Installing       : perl-base-2.27-484.el9.noarch                       48/110 
  Installing       : perl-URI-5.09-4.el9.noarch                          49/110 
  Installing       : perl-Mozilla-CA-20200520-6.el9.noarch               50/110 
  Installing       : perl-if-0.60.800-484.el9.noarch                     51/110 
  Installing       : perl-IO-Socket-IP-0.41-5.el9.noarch                 52/110 
  Installing       : perl-Time-Local-2:1.300-7.el9.noarch                53/110 
  Installing       : perl-File-Path-2.18-4.el9.noarch                    54/110 
  Installing       : perl-IO-Socket-SSL-2.073-2.el9.noarch               55/110 
  Installing       : perl-Net-SSLeay-1.94-4.el9.x86_64                   56/110 
  Installing       : perl-Pod-Escapes-1:1.07-460.el9.noarch              57/110 
  Installing       : perl-Text-Tabs+Wrap-2013.0523-460.el9.noarch        58/110 
  Installing       : perl-Class-Struct-0.66-484.el9.noarch               59/110 
  Installing       : perl-POSIX-1.94-484.el9.x86_64                      60/110 
  Installing       : perl-Term-ANSIColor-5.01-461.el9.noarch             61/110 
  Installing       : perl-IPC-Open3-1.21-484.el9.noarch                  62/110 
  Installing       : perl-subs-1.03-484.el9.noarch                       63/110 
  Installing       : perl-File-Temp-1:0.231.100-4.el9.noarch             64/110 
  Installing       : perl-HTTP-Tiny-0.076-462.el9.noarch                 65/110 
  Installing       : perl-Term-Cap-1.17-460.el9.noarch                   66/110 
  Installing       : perl-Pod-Simple-1:3.42-4.el9.noarch                 67/110 
  Installing       : perl-Socket-4:2.031-4.el9.x86_64                    68/110 
  Installing       : perl-SelectSaver-1.02-484.el9.noarch                69/110 
  Installing       : perl-Symbol-1.08-484.el9.noarch                     70/110 
  Installing       : perl-File-stat-1.09-484.el9.noarch                  71/110 
  Installing       : perl-podlators-1:4.14-460.el9.noarch                72/110 
  Installing       : perl-Pod-Perldoc-3.28.01-461.el9.noarch             73/110 
  Installing       : perl-Fcntl-1.13-484.el9.x86_64                      74/110 
  Installing       : perl-Text-ParseWords-3.30-460.el9.noarch            75/110 
  Installing       : perl-mro-1.23-484.el9.x86_64                        76/110 
  Installing       : perl-IO-1.43-484.el9.x86_64                         77/110 
  Installing       : perl-overloading-0.02-484.el9.noarch                78/110 
  Installing       : perl-Pod-Usage-4:2.01-4.el9.noarch                  79/110 
  Installing       : perl-Errno-1.30-484.el9.x86_64                      80/110 
  Installing       : perl-File-Basename-2.85-484.el9.noarch              81/110 
  Installing       : perl-Getopt-Std-1.12-484.el9.noarch                 82/110 
  Installing       : perl-MIME-Base64-3.16-4.el9.x86_64                  83/110 
  Installing       : perl-Scalar-List-Utils-4:1.56-462.el9.x86_64        84/110 
  Installing       : perl-constant-1.33-461.el9.noarch                   85/110 
  Installing       : perl-Storable-1:3.21-460.el9.x86_64                 86/110 
  Installing       : perl-overload-1.31-484.el9.noarch                   87/110 
  Installing       : perl-parent-1:0.238-460.el9.noarch                  88/110 
  Installing       : perl-vars-1.05-484.el9.noarch                       89/110 
  Installing       : perl-Getopt-Long-1:2.52-4.el9.noarch                90/110 
  Installing       : perl-Carp-1.50-460.el9.noarch                       91/110 
  Installing       : perl-Exporter-5.74-461.el9.noarch                   92/110 
  Installing       : perl-NDBM_File-1.15-484.el9.x86_64                  93/110 
  Installing       : perl-PathTools-3.78-461.el9.x86_64                  94/110 
  Installing       : perl-Encode-4:3.08-462.el9.x86_64                   95/110 
  Installing       : perl-libs-4:5.32.1-484.el9.x86_64                   96/110 
  Installing       : perl-interpreter-4:5.32.1-484.el9.x86_64            97/110 
  Installing       : kernel-srpm-macros-1.0-16.el9.noarch                98/110 
  Installing       : diffutils-3.7-12.el9.x86_64                         99/110 
  Installing       : cpio-2.13-16.el9.x86_64                            100/110 
  Installing       : bzip2-1.0.8-11.el9.x86_64                          101/110 
  Installing       : fonts-srpm-macros-1:2.0.5-7.el9.1.noarch           102/110 
  Installing       : go-srpm-macros-3.8.1-2.el9.noarch                  103/110 
  Installing       : python-srpm-macros-3.9-54.el9.noarch               104/110 
  Installing       : redhat-rpm-config-211-1.el9.noarch                 105/110 
  Installing       : rpm-build-4.16.1.3-42.el9.x86_64                   106/110 
  Installing       : pyproject-srpm-macros-1.23.2-1.el9.noarch          107/110 
  Installing       : fftw-3.3.8-14.el9.x86_64                           108/110 
  Installing       : libsndfile-1.0.31-10.el9.x86_64                    109/110 
  Installing       : glibc-gconv-extra-2.34-290.el9.x86_64              110/110 
  Running scriptlet: glibc-gconv-extra-2.34-290.el9.x86_64              110/110 
  Verifying        : binutils-2.35.2-72.el9.x86_64                        1/110 
  Verifying        : binutils-gold-2.35.2-72.el9.x86_64                   2/110 
  Verifying        : bzip2-1.0.8-11.el9.x86_64                            3/110 
  Verifying        : cpio-2.13-16.el9.x86_64                              4/110 
  Verifying        : diffutils-3.7-12.el9.x86_64                          5/110 
  Verifying        : ed-1.14.2-12.el9.x86_64                              6/110 
  Verifying        : elfutils-0.197-1.el9.x86_64                          7/110 
  Verifying        : elfutils-debuginfod-client-0.197-1.el9.x86_64        8/110 
  Verifying        : file-5.39-19.el9.x86_64                              9/110 
  Verifying        : glibc-gconv-extra-2.34-290.el9.x86_64               10/110 
  Verifying        : groff-base-1.22.4-10.el9.x86_64                     11/110 
  Verifying        : info-6.7-15.el9.x86_64                              12/110 
  Verifying        : libpkgconf-1.7.3-10.el9.x86_64                      13/110 
  Verifying        : libquadmath-11.5.0-16.el9.x86_64                    14/110 
  Verifying        : ncurses-6.2-12.20210508.el9.x86_64                  15/110 
  Verifying        : pkgconf-1.7.3-10.el9.x86_64                         16/110 
  Verifying        : pkgconf-m4-1.7.3-10.el9.noarch                      17/110 
  Verifying        : pkgconf-pkg-config-1.7.3-10.el9.x86_64              18/110 
  Verifying        : unzip-6.0-59.el9.x86_64                             19/110 
  Verifying        : xz-5.2.5-10.el9.x86_64                              20/110 
  Verifying        : zip-3.0-35.el9.x86_64                               21/110 
  Verifying        : zstd-1.5.5-1.el9.x86_64                             22/110 
  Verifying        : debugedit-5.0-11.el9.x86_64                         23/110 
  Verifying        : dwz-0.16-1.el9.x86_64                               24/110 
  Verifying        : efi-srpm-macros-6-4.el9.noarch                      25/110 
  Verifying        : fftw-3.3.8-14.el9.x86_64                            26/110 
  Verifying        : fftw-libs-double-3.3.8-14.el9.x86_64                27/110 
  Verifying        : fftw-libs-long-3.3.8-14.el9.x86_64                  28/110 
  Verifying        : fftw-libs-quad-3.3.8-14.el9.x86_64                  29/110 
  Verifying        : fftw-libs-single-3.3.8-14.el9.x86_64                30/110 
  Verifying        : flac-libs-1.3.3-12.el9.x86_64                       31/110 
  Verifying        : fonts-srpm-macros-1:2.0.5-7.el9.1.noarch            32/110 
  Verifying        : gdb-minimal-17.2-4.el9.x86_64                       33/110 
  Verifying        : ghc-srpm-macros-1.5.0-6.el9.noarch                  34/110 
  Verifying        : go-srpm-macros-3.8.1-2.el9.noarch                   35/110 
  Verifying        : gsm-1.0.19-6.el9.x86_64                             36/110 
  Verifying        : kernel-srpm-macros-1.0-16.el9.noarch                37/110 
  Verifying        : libogg-2:1.3.4-6.el9.x86_64                         38/110 
  Verifying        : libsndfile-1.0.31-10.el9.x86_64                     39/110 
  Verifying        : libvorbis-1:1.3.7-5.el9.x86_64                      40/110 
  Verifying        : lua-srpm-macros-1-6.el9.noarch                      41/110 
  Verifying        : ocaml-srpm-macros-6-6.el9.noarch                    42/110 
  Verifying        : openblas-srpm-macros-2-11.el9.noarch                43/110 
  Verifying        : opus-1.3.1-10.el9.x86_64                            44/110 
  Verifying        : patch-2.7.6-16.el9.x86_64                           45/110 
  Verifying        : perl-AutoLoader-5.74-484.el9.noarch                 46/110 
  Verifying        : perl-B-1.80-484.el9.x86_64                          47/110 
  Verifying        : perl-Carp-1.50-460.el9.noarch                       48/110 
  Verifying        : perl-Class-Struct-0.66-484.el9.noarch               49/110 
  Verifying        : perl-Data-Dumper-2.174-462.el9.x86_64               50/110 
  Verifying        : perl-Digest-1.19-4.el9.noarch                       51/110 
  Verifying        : perl-Digest-MD5-2.58-4.el9.x86_64                   52/110 
  Verifying        : perl-Encode-4:3.08-462.el9.x86_64                   53/110 
  Verifying        : perl-Errno-1.30-484.el9.x86_64                      54/110 
  Verifying        : perl-Exporter-5.74-461.el9.noarch                   55/110 
  Verifying        : perl-Fcntl-1.13-484.el9.x86_64                      56/110 
  Verifying        : perl-File-Basename-2.85-484.el9.noarch              57/110 
  Verifying        : perl-File-Path-2.18-4.el9.noarch                    58/110 
  Verifying        : perl-File-Temp-1:0.231.100-4.el9.noarch             59/110 
  Verifying        : perl-File-stat-1.09-484.el9.noarch                  60/110 
  Verifying        : perl-FileHandle-2.03-484.el9.noarch                 61/110 
  Verifying        : perl-Getopt-Long-1:2.52-4.el9.noarch                62/110 
  Verifying        : perl-Getopt-Std-1.12-484.el9.noarch                 63/110 
  Verifying        : perl-HTTP-Tiny-0.076-462.el9.noarch                 64/110 
  Verifying        : perl-IO-1.43-484.el9.x86_64                         65/110 
  Verifying        : perl-IO-Socket-IP-0.41-5.el9.noarch                 66/110 
  Verifying        : perl-IO-Socket-SSL-2.073-2.el9.noarch               67/110 
  Verifying        : perl-IPC-Open3-1.21-484.el9.noarch                  68/110 
  Verifying        : perl-MIME-Base64-3.16-4.el9.x86_64                  69/110 
  Verifying        : perl-Mozilla-CA-20200520-6.el9.noarch               70/110 
  Verifying        : perl-NDBM_File-1.15-484.el9.x86_64                  71/110 
  Verifying        : perl-Net-SSLeay-1.94-4.el9.x86_64                   72/110 
  Verifying        : perl-POSIX-1.94-484.el9.x86_64                      73/110 
  Verifying        : perl-PathTools-3.78-461.el9.x86_64                  74/110 
  Verifying        : perl-Pod-Escapes-1:1.07-460.el9.noarch              75/110 
  Verifying        : perl-Pod-Perldoc-3.28.01-461.el9.noarch             76/110 
  Verifying        : perl-Pod-Simple-1:3.42-4.el9.noarch                 77/110 
  Verifying        : perl-Pod-Usage-4:2.01-4.el9.noarch                  78/110 
  Verifying        : perl-Scalar-List-Utils-4:1.56-462.el9.x86_64        79/110 
  Verifying        : perl-SelectSaver-1.02-484.el9.noarch                80/110 
  Verifying        : perl-Socket-4:2.031-4.el9.x86_64                    81/110 
  Verifying        : perl-Storable-1:3.21-460.el9.x86_64                 82/110 
  Verifying        : perl-Symbol-1.08-484.el9.noarch                     83/110 
  Verifying        : perl-Term-ANSIColor-5.01-461.el9.noarch             84/110 
  Verifying        : perl-Term-Cap-1.17-460.el9.noarch                   85/110 
  Verifying        : perl-Text-ParseWords-3.30-460.el9.noarch            86/110 
  Verifying        : perl-Text-Tabs+Wrap-2013.0523-460.el9.noarch        87/110 
  Verifying        : perl-Time-Local-2:1.300-7.el9.noarch                88/110 
  Verifying        : perl-URI-5.09-4.el9.noarch                          89/110 
  Verifying        : perl-Unicode-Normalize-1.27-461.el9.x86_64          90/110 
  Verifying        : perl-base-2.27-484.el9.noarch                       91/110 
  Verifying        : perl-constant-1.33-461.el9.noarch                   92/110 
  Verifying        : perl-if-0.60.800-484.el9.noarch                     93/110 
  Verifying        : perl-interpreter-4:5.32.1-484.el9.x86_64            94/110 
  Verifying        : perl-libnet-3.13-4.el9.noarch                       95/110 
  Verifying        : perl-libs-4:5.32.1-484.el9.x86_64                   96/110 
  Verifying        : perl-mro-1.23-484.el9.x86_64                        97/110 
  Verifying        : perl-overload-1.31-484.el9.noarch                   98/110 
  Verifying        : perl-overloading-0.02-484.el9.noarch                99/110 
  Verifying        : perl-parent-1:0.238-460.el9.noarch                 100/110 
  Verifying        : perl-podlators-1:4.14-460.el9.noarch               101/110 
  Verifying        : perl-srpm-macros-1-41.el9.noarch                   102/110 
  Verifying        : perl-subs-1.03-484.el9.noarch                      103/110 
  Verifying        : perl-vars-1.05-484.el9.noarch                      104/110 
  Verifying        : pyproject-srpm-macros-1.23.2-1.el9.noarch          105/110 
  Verifying        : python-srpm-macros-3.9-54.el9.noarch               106/110 
  Verifying        : qt5-srpm-macros-5.15.9-1.el9.noarch                107/110 
  Verifying        : redhat-rpm-config-211-1.el9.noarch                 108/110 
  Verifying        : rpm-build-4.16.1.3-42.el9.x86_64                   109/110 
  Verifying        : rust-srpm-macros-17-4.el9.noarch                   110/110 

Installed:
  binutils-2.35.2-72.el9.x86_64                                                 
  binutils-gold-2.35.2-72.el9.x86_64                                            
  bzip2-1.0.8-11.el9.x86_64                                                     
  cpio-2.13-16.el9.x86_64                                                       
  debugedit-5.0-11.el9.x86_64                                                   
  diffutils-3.7-12.el9.x86_64                                                   
  dwz-0.16-1.el9.x86_64                                                         
  ed-1.14.2-12.el9.x86_64                                                       
  efi-srpm-macros-6-4.el9.noarch                                                
  elfutils-0.197-1.el9.x86_64                                                   
  elfutils-debuginfod-client-0.197-1.el9.x86_64                                 
  fftw-3.3.8-14.el9.x86_64                                                      
  fftw-libs-double-3.3.8-14.el9.x86_64                                          
  fftw-libs-long-3.3.8-14.el9.x86_64                                            
  fftw-libs-quad-3.3.8-14.el9.x86_64                                            
  fftw-libs-single-3.3.8-14.el9.x86_64                                          
  file-5.39-19.el9.x86_64                                                       
  flac-libs-1.3.3-12.el9.x86_64                                                 
  fonts-srpm-macros-1:2.0.5-7.el9.1.noarch                                      
  gdb-minimal-17.2-4.el9.x86_64                                                 
  ghc-srpm-macros-1.5.0-6.el9.noarch                                            
  glibc-gconv-extra-2.34-290.el9.x86_64                                         
  go-srpm-macros-3.8.1-2.el9.noarch                                             
  groff-base-1.22.4-10.el9.x86_64                                               
  gsm-1.0.19-6.el9.x86_64                                                       
  info-6.7-15.el9.x86_64                                                        
  kernel-srpm-macros-1.0-16.el9.noarch                                          
  libogg-2:1.3.4-6.el9.x86_64                                                   
  libpkgconf-1.7.3-10.el9.x86_64                                                
  libquadmath-11.5.0-16.el9.x86_64                                              
  libsndfile-1.0.31-10.el9.x86_64                                               
  libvorbis-1:1.3.7-5.el9.x86_64                                                
  lua-srpm-macros-1-6.el9.noarch                                                
  ncurses-6.2-12.20210508.el9.x86_64                                            
  ocaml-srpm-macros-6-6.el9.noarch                                              
  openblas-srpm-macros-2-11.el9.noarch                                          
  opus-1.3.1-10.el9.x86_64                                                      
  patch-2.7.6-16.el9.x86_64                                                     
  perl-AutoLoader-5.74-484.el9.noarch                                           
  perl-B-1.80-484.el9.x86_64                                                    
  perl-Carp-1.50-460.el9.noarch                                                 
  perl-Class-Struct-0.66-484.el9.noarch                                         
  perl-Data-Dumper-2.174-462.el9.x86_64                                         
  perl-Digest-1.19-4.el9.noarch                                                 
  perl-Digest-MD5-2.58-4.el9.x86_64                                             
  perl-Encode-4:3.08-462.el9.x86_64                                             
  perl-Errno-1.30-484.el9.x86_64                                                
  perl-Exporter-5.74-461.el9.noarch                                             
  perl-Fcntl-1.13-484.el9.x86_64                                                
  perl-File-Basename-2.85-484.el9.noarch                                        
  perl-File-Path-2.18-4.el9.noarch                                              
  perl-File-Temp-1:0.231.100-4.el9.noarch                                       
  perl-File-stat-1.09-484.el9.noarch                                            
  perl-FileHandle-2.03-484.el9.noarch                                           
  perl-Getopt-Long-1:2.52-4.el9.noarch                                          
  perl-Getopt-Std-1.12-484.el9.noarch                                           
  perl-HTTP-Tiny-0.076-462.el9.noarch                                           
  perl-IO-1.43-484.el9.x86_64                                                   
  perl-IO-Socket-IP-0.41-5.el9.noarch                                           
  perl-IO-Socket-SSL-2.073-2.el9.noarch                                         
  perl-IPC-Open3-1.21-484.el9.noarch                                            
  perl-MIME-Base64-3.16-4.el9.x86_64                                            
  perl-Mozilla-CA-20200520-6.el9.noarch                                         
  perl-NDBM_File-1.15-484.el9.x86_64                                            
  perl-Net-SSLeay-1.94-4.el9.x86_64                                             
  perl-POSIX-1.94-484.el9.x86_64                                                
  perl-PathTools-3.78-461.el9.x86_64                                            
  perl-Pod-Escapes-1:1.07-460.el9.noarch                                        
  perl-Pod-Perldoc-3.28.01-461.el9.noarch                                       
  perl-Pod-Simple-1:3.42-4.el9.noarch                                           
  perl-Pod-Usage-4:2.01-4.el9.noarch                                            
  perl-Scalar-List-Utils-4:1.56-462.el9.x86_64                                  
  perl-SelectSaver-1.02-484.el9.noarch                                          
  perl-Socket-4:2.031-4.el9.x86_64                                              
  perl-Storable-1:3.21-460.el9.x86_64                                           
  perl-Symbol-1.08-484.el9.noarch                                               
  perl-Term-ANSIColor-5.01-461.el9.noarch                                       
  perl-Term-Cap-1.17-460.el9.noarch                                             
  perl-Text-ParseWords-3.30-460.el9.noarch                                      
  perl-Text-Tabs+Wrap-2013.0523-460.el9.noarch                                  
  perl-Time-Local-2:1.300-7.el9.noarch                                          
  perl-URI-5.09-4.el9.noarch                                                    
  perl-Unicode-Normalize-1.27-461.el9.x86_64                                    
  perl-base-2.27-484.el9.noarch                                                 
  perl-constant-1.33-461.el9.noarch                                             
  perl-if-0.60.800-484.el9.noarch                                               
  perl-interpreter-4:5.32.1-484.el9.x86_64                                      
  perl-libnet-3.13-4.el9.noarch                                                 
  perl-libs-4:5.32.1-484.el9.x86_64                                             
  perl-mro-1.23-484.el9.x86_64                                                  
  perl-overload-1.31-484.el9.noarch                                             
  perl-overloading-0.02-484.el9.noarch                                          
  perl-parent-1:0.238-460.el9.noarch                                            
  perl-podlators-1:4.14-460.el9.noarch                                          
  perl-srpm-macros-1-41.el9.noarch                                              
  perl-subs-1.03-484.el9.noarch                                                 
  perl-vars-1.05-484.el9.noarch                                                 
  pkgconf-1.7.3-10.el9.x86_64                                                   
  pkgconf-m4-1.7.3-10.el9.noarch                                                
  pkgconf-pkg-config-1.7.3-10.el9.x86_64                                        
  pyproject-srpm-macros-1.23.2-1.el9.noarch                                     
  python-srpm-macros-3.9-54.el9.noarch                                          
  qt5-srpm-macros-5.15.9-1.el9.noarch                                           
  redhat-rpm-config-211-1.el9.noarch                                            
  rpm-build-4.16.1.3-42.el9.x86_64                                              
  rust-srpm-macros-17-4.el9.noarch                                              
  unzip-6.0-59.el9.x86_64                                                       
  xz-5.2.5-10.el9.x86_64                                                        
  zip-3.0-35.el9.x86_64                                                         
  zstd-1.5.5-1.el9.x86_64                                                       

Complete!
17 files removed
Removing intermediate container 2db75dacf639
 ---> 6afce58c7659
Step 7/9 : RUN curl -fLsSo /tmp/resampler.rpm "$URL"
 ---> Running in 40215a6fc557
Removing intermediate container 40215a6fc557
 ---> 647fe9a20350
Step 8/9 : RUN rpm -qpi /tmp/resampler.rpm     && rpm -qpl /tmp/resampler.rpm     && rpm -qp --provides /tmp/resampler.rpm     && rpm -qp --requires /tmp/resampler.rpm     && rpm -qp --scripts /tmp/resampler.rpm
 ---> Running in b9efd1a94c93
Name        : resampler
Version     : 2.1.2
Release     : 1
Architecture: x86_64
Install Date: (not installed)
Group       : Applications/Multimedia
Size        : 39664
License     : LGPL-2.1
Signature   : (none)
Source RPM  : resampler-2.1.2-1.src.rpm
Build Date  : Fri Apr  3 03:22:00 2026
Build Host  : localhost
Relocations : /usr 
Vendor      : jniemann66
Summary     : High-quality audio sample rate converter
Description :
DESCRIPTION
===========

This is an installer created using CPack (https://cmake.org). No additional installation instructions provided.
/usr/bin/ReSampler
/usr/lib/.build-id
/usr/lib/.build-id/11
/usr/lib/.build-id/11/a3602310e64c32366b1baaf98ff7847cf20f06
resampler = 2.1.2-1
resampler(x86-64) = 2.1.2-1
fftw >= 3.3
libReSamplerLib.so()(64bit)
libc.so.6()(64bit)
libc.so.6(GLIBC_2.14)(64bit)
libc.so.6(GLIBC_2.2.5)(64bit)
libc.so.6(GLIBC_2.4)(64bit)
libgcc_s.so.1()(64bit)
libgcc_s.so.1(GCC_3.0)(64bit)
libsndfile >= 1.0
libstdc++.so.6()(64bit)
libstdc++.so.6(CXXABI_1.3)(64bit)
libstdc++.so.6(GLIBCXX_3.4)(64bit)
libstdc++.so.6(GLIBCXX_3.4.21)(64bit)
rpmlib(CompressedFileNames) <= 3.0.4-1
rpmlib(FileDigests) <= 4.6.0-1
rpmlib(PayloadFilesHavePrefix) <= 4.0-1
rtld(GNU_HASH)
Removing intermediate container b9efd1a94c93
 ---> fb25865893fa
Step 9/9 : RUN if dnf install -y /tmp/resampler.rpm; then         echo "RPM installation succeeded";         rpm -ql resampler;         rpm -V resampler;         ldd /usr/bin/ReSampler;     else         echo "RPM installation failed; inspecting uninstalled payload";         mkdir -p /tmp/resampler-root;         cd /tmp/resampler-root;         rpm2cpio /tmp/resampler.rpm | cpio -idm --quiet;         find . -type f -print;         ldd ./usr/bin/ReSampler;     fi
 ---> Running in 88030231dc40
CentOS Stream 9 - BaseOS                        179 kB/s | 2.9 MB     00:16    
CentOS Stream 9 - AppStream                     4.4 MB/s |  13 MB     00:02    
CentOS Stream 9 - Extras packages               739  B/s |  22 kB     00:31    
Error: 
 Problem: conflicting requests
  - nothing provides libReSamplerLib.so()(64bit) needed by resampler-2.1.2-1.x86_64 from @commandline
(try to add '--skip-broken' to skip uninstallable packages or '--nobest' to use not only best candidate packages)
RPM installation failed; inspecting uninstalled payload
./usr/bin/ReSampler
	linux-vdso.so.1 (0x00007ffdf1996000)
	libReSamplerLib.so => not found
	libstdc++.so.6 => /lib64/libstdc++.so.6 (0x00007f87755a8000)
	libgcc_s.so.1 => /lib64/libgcc_s.so.1 (0x00007f877558b000)
	libc.so.6 => /lib64/libc.so.6 (0x00007f8775380000)
	libm.so.6 => /lib64/libm.so.6 (0x00007f87752a1000)
	/lib64/ld-linux-x86-64.so.2 (0x00007f87757df000)
Removing intermediate container 88030231dc40
 ---> 3f428dfdbc15
Successfully built 3f428dfdbc15
Successfully tagged basic-resampler:latest
```
```sh
docker run -u root -it centos/perl-524-centos7 sh
```
```sh
curl -skLo /tmp/resampler.rpm https://github.com/jniemann66/ReSampler/releases/download/v2.1.2/ReSampler-2.1.2-Linux.rpm
``` 
```sh
rpm -i /tmp/resampler.rpm
```
```text
error: Failed dependencies:
        fftw >= 3.3 is needed by resampler-2.1.2-1.x86_64
        libReSamplerLib.so()(64bit) is needed by resampler-2.1.2-1.x86_64
        libsndfile >= 1.0 is needed by resampler-2.1.2-1.x86_64
        libstdc++.so.6(GLIBCXX_3.4.21)(64bit) is needed by resampler-2.1.2-1.x86_64
```
```sh
rpm -qpl /tmp/resampler.rpm
```
```text
/usr/bin/ReSampler
/usr/lib/.build-id
/usr/lib/.build-id/11
/usr/lib/.build-id/11/a3602310e64c32366b1baaf98ff7847cf20f06
```
```sh
rpm -qp --provides /tmp/resampler.rpm
```
```text
resampler = 2.1.2-1
resampler(x86-64) = 2.1.2-1
```
```sh
rpm -qp --requires /tmp/resampler.rpm
```
```text
fftw >= 3.3
libReSamplerLib.so()(64bit)
libc.so.6()(64bit)
libc.so.6(GLIBC_2.14)(64bit)
libc.so.6(GLIBC_2.2.5)(64bit)
libc.so.6(GLIBC_2.4)(64bit)
libgcc_s.so.1()(64bit)
libgcc_s.so.1(GCC_3.0)(64bit)
libsndfile >= 1.0
libstdc++.so.6()(64bit)
libstdc++.so.6(CXXABI_1.3)(64bit)
libstdc++.so.6(GLIBCXX_3.4)(64bit)
libstdc++.so.6(GLIBCXX_3.4.21)(64bit)
rpmlib(CompressedFileNames) <= 3.0.4-1
rpmlib(FileDigests) <= 4.6.0-1
rpmlib(PayloadFilesHavePrefix) <= 4.0-1
rtld(GNU_HASH)
``` 
#### NOTE

avoid `bitnami/minideb:buster`


```
apt-get update
```
will fail with
```text
E: The repository 'http://security.debian.org/debian-security buster/updates Release' does not have a Release file.
E: The repository 'http://deb.debian.org/debian buster Release' does not have a Release file.
```
and withut update a plain
```sh
apt-get istall wget
```
will fail with

```text
E: Unable to locate package wget
```

but with the `debian:bullseye-slim` the

```
apt-get insall wget
```
```text
E: Failed to fetch http://deb.debian.org/debian-security/pool/updates/main/o/openssl/openssl_1.1.1w-0%2bdeb11u8_amd64.deb  404  Not Found [IP: 199.232.94.132 80]
E: Failed to fetch http://deb.debian.org/debian-security/pool/updates/main/c/ca-certificates/ca-certificates_20250419%7edeb12u1%7edeb11u1_all.deb  404  Not Found [IP: 199.232.94.132 80]
E: Failed to fetch http://deb.debian.org/debian-security/pool/updates/main/n/nghttp2/libnghttp2-14_1.43.0-1%2bdeb11u3_amd64.deb  404  Not Found [IP: 199.232.94.132 80]
E: Failed to fetch http://deb.debian.org/debian-security/pool/updates/main/c/curl/libcurl4_7.74.0-1.3%2bdeb11u16_amd64.deb  404  Not Found [IP: 199.232.94.132 80]
E: Failed to fetch http://deb.debian.org/debian-security/pool/updates/main/c/curl/curl_7.74.0-1.3%2bdeb11u16_amd64.deb  404  Not Found [IP: 199.232.94.132 80]
E: Unable to fetch some archives, maybe run apt-get update or try with --fix-missing?
```

#### Defect Issue Draft

# ReSampler 2.1.2 Linux package investigation

## Finding

Both published Linux packages appear to omit a shared library required by the executable: `libReSamplerLib.so`.

## Debian package

Package: `ReSampler-2.1.2-Linux.deb`

- `dpkg-deb -c` lists only `/usr/bin/ReSampler`.
- `dpkg -L resampler` lists only `/usr/bin/ReSampler`.
- `ldd /usr/bin/ReSampler` reports:

```text
libReSamplerLib.so => not found
```

## RPM package

Package: `ReSampler-2.1.2-Linux.rpm`

- `rpm -qpl` lists `/usr/bin/ReSampler` and build-ID metadata only.
- `rpm -qp --provides` does not list `libReSamplerLib.so`.
- `rpm -qp --requires` explicitly lists:

```text
libReSamplerLib.so()(64bit)
```

## Additional compatibility observation

The RPM requires:

```text
libstdc++.so.6(GLIBCXX_3.4.21)(64bit)
```

This requirement is not satisfied by the stock CentOS 7 C++ runtime.

## Conclusion

The Linux package payloads appear incomplete. The executable has a shared-library dependency that is not present in either package.

The missing library should be investigated separately from the older CentOS 7 C++ runtime compatibility issue.


```sh
 unzip -ql ~/Downloads/ReSampler-2_1_0-windowsBinaries.zip
  Length      Date    Time    Name
---------  ---------- -----   ----
  2228302  2014-03-16 08:26   win32/libfftw3-3.dll
  1443328  2017-02-21 13:52   win32/libsndfile-1.dll
   884224  2020-09-12 00:06   win32/ReSampler.exe
  2228302  2014-03-16 08:26   win32-noSSE/libfftw3-3.dll
  1443328  2017-02-21 13:51   win32-noSSE/libsndfile-1.dll
   883712  2020-09-12 00:06   win32-noSSE/ReSampler.exe
  2545034  2014-03-16 08:29   win64/libfftw3-3.dll
  1522688  2017-02-21 13:52   win64/libsndfile-1.dll
  1101824  2020-09-12 00:06   win64/ReSampler.exe
  2545034  2014-03-16 08:29   win64-avx/libfftw3-3.dll
  1522688  2017-02-21 13:52   win64-avx/libsndfile-1.dll
  1102336  2020-09-12 00:07   win64-avx/ReSampler.exe
  2545034  2017-05-26 14:17   win64-avx-mingw-w64-compiler/libfftw3-3.dll
    74240  2020-01-06 21:20   win64-avx-mingw-w64-compiler/libgcc_s_seh-1.dll
  1522688  2017-05-26 14:17   win64-avx-mingw-w64-compiler/libsndfile-1.dll
  1425920  2020-01-06 21:20   win64-avx-mingw-w64-compiler/libstdc++-6.dll
    52224  2020-01-06 21:20   win64-avx-mingw-w64-compiler/libwinpthread-1.dll
      172  2017-05-26 14:17   win64-avx-mingw-w64-compiler/minGW-readme.txt
  1143436  2020-09-12 01:05   win64-avx-mingw-w64-compiler/ReSampler.exe
  2545034  2017-05-26 14:17   win64-mingw-w64-compiler/libfftw3-3.dll
    74240  2020-01-06 21:20   win64-mingw-w64-compiler/libgcc_s_seh-1.dll
   326144  2018-04-07 18:10   win64-mingw-w64-compiler/libquadmath-0.dll
  1522688  2017-05-26 14:17   win64-mingw-w64-compiler/libsndfile-1.dll
  1425920  2020-01-06 21:20   win64-mingw-w64-compiler/libstdc++-6.dll
    52224  2020-01-06 21:20   win64-mingw-w64-compiler/libwinpthread-1.dll
      172  2017-05-26 14:17   win64-mingw-w64-compiler/minGW-readme.txt
      508  2018-12-01 00:15   win64-mingw-w64-compiler/QuadMath-readme.txt
  1129382  2020-09-12 00:59   win64-mingw-w64-compiler/ReSampler.exe
  1140965  2020-09-12 11:03   win64-mingw-w64-compiler/ReSampler-QuadMath.exe
---------                     -------
 34431791                     29 files

```

```cmd
"c:\Program Files\Microsoft Visual Studio 10.0\VC\bin\dumpbin.exe" /imports c:\tools\resampler.exe

```
```
Microsoft (R) COFF/PE Dumper Version 10.00.30319.01
Copyright (C) Microsoft Corporation.  All rights reserved.


Dump of file c:\tools\resampler.exe

File Type: EXECUTABLE IMAGE

  Section contains the following imports:

    libsndfile-1.dll
                4AC1F8 Import Address Table
                4CB344 Import Name Table
                     0 time date stamp
                     0 Index of first forwarder reference

                      Ordinal    10
                      Ordinal     2
                      Ordinal    40
                      Ordinal    60
                      Ordinal     3
                      Ordinal    50
                      Ordinal     4
                      Ordinal     7
                      Ordinal    61
                      Ordinal    23
                      Ordinal     1
                      Ordinal    11
                      Ordinal    39
                      Ordinal    24

    libfftw3-3.dll
                4AC1E8 Import Address Table
                4CB334 Import Name Table
                     0 time date stamp
                     0 Index of first forwarder reference

                  2E9 fftw_execute
                  2D5 fftw_destroy_plan
                  373 fftw_plan_dft_1d

    KERNEL32.dll
                4AC000 Import Address Table
                4CB14C Import Name Table
                     0 time date stamp
                     0 Index of first forwarder reference

                   FB CreateTimerQueueTimer
                  510 SetEndOfFile
                  611 WriteConsoleW
                   CB CreateFileW
                  54A SetStdHandle
                  2B4 GetProcessHeap
                  514 SetEnvironmentVariableW
                  1AA FreeEnvironmentStringsW
                  237 GetEnvironmentStringsW
                  297 GetOEMCP
                  1B2 GetACP
                  38B IsValidCodePage
                  18C FindNextFileW
                  17B FindFirstFileExW
                  175 FindClose
                  30E GetTimeZoneInformation
                  470 ReadConsoleW
                  473 ReadFile
                  2F6 GetTempPathW
                  2F4 GetTempFileNameW
                  115 DeleteFileW
                  261 GetLastError
                  5FE WideCharToMultiByte
                  462 RaiseException
                  21C GetCurrentThreadId
                  3EF MultiByteToWideChar
                  44D QueryPerformanceCounter
                  44E QueryPerformanceFrequency
                  131 EnterCriticalSection
                  3BD LeaveCriticalSection
                  5A7 TryEnterCriticalSection
                  110 DeleteCriticalSection
                   86 CloseHandle
                  5D8 WaitForSingleObjectEx
                  57D Sleep
                  587 SwitchToThread
                  23D GetExitCodeThread
                  532 SetLastError
                  35F InitializeCriticalSectionAndSpinCount
                   BF CreateEventW
                  59E TlsAlloc
                  5A0 TlsGetValue
                  5A1 TlsSetValue
                  59F TlsFree
                  2E9 GetSystemTimeAsFileTime
                  307 GetTickCount
                  278 GetModuleHandleW
                  2AE GetProcAddress
                  12D EncodePointer
                  109 DecodePointer
                  1C1 GetCPInfo
                   9B CompareStringW
                  3B1 LCMapStringW
                  265 GetLocaleInfoW
                  2D7 GetStringTypeW
                  516 SetEvent
                  4C6 ResetEvent
                  5AD UnhandledExceptionFilter
                  56D SetUnhandledExceptionFilter
                  217 GetCurrentProcess
                  58C TerminateProcess
                  386 IsProcessorFeaturePresent
                  37F IsDebuggerPresent
                  2D0 GetStartupInfoW
                  218 GetCurrentProcessId
                  363 InitializeSListHead
                   FA CreateTimerQueue
                  57B SignalObjectAndWait
                   F3 CreateThread
                  55E SetThreadPriority
                  301 GetThreadPriority
                  269 GetLogicalProcessorInformation
                   78 ChangeTimerQueueTimer
                  11A DeleteTimerQueueTimer
                  289 GetNumaHighestNodeNumber
                  2AF GetProcessAffinityMask
                  553 SetThreadAffinityMask
                  4A9 RegisterWaitForSingleObject
                  5B6 UnregisterWait
                  21B GetCurrentThread
                  305 GetThreadTimes
                  1AB FreeLibrary
                  1AC FreeLibraryAndExitThread
                  274 GetModuleFileNameW
                  275 GetModuleHandleA
                  3C3 LoadLibraryExW
                  31B GetVersionExW
                  5C6 VirtualAlloc
                  5CC VirtualProtect
                  5C9 VirtualFree
                  12B DuplicateHandle
                  4B4 ReleaseSemaphore
                  36E InterlockedPopEntrySList
                  36F InterlockedPushEntrySList
                  36C InterlockedFlushSList
                  443 QueryDepthSList
                  5B7 UnregisterWaitEx
                  3C4 LoadLibraryW
                  4D3 RtlUnwind
                  15F ExitThread
                  277 GetModuleHandleExW
                  15E ExitProcess
                  2D2 GetStdHandle
                  612 WriteFile
                  1D6 GetCommandLineA
                  1D7 GetCommandLineW
                  349 HeapFree
                  345 HeapAlloc
                  24E GetFileType
                  34C HeapReAlloc
                  34E HeapSize
                  24C GetFileSizeEx
                  523 SetFilePointerEx
                  19F FlushFileBuffers
                  1EA GetConsoleCP
                  1FC GetConsoleMode
                  221 GetDateFormatW
                  30C GetTimeFormatW
                  38D IsValidLocale
                  312 GetUserDefaultLCID
                  154 EnumSystemLocalesW

  Summary

        7000 .data
       20000 .rdata
        9000 .reloc
        1000 .rsrc
       AB000 .text

```
