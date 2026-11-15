### Info

### Usage

```sh
docker pull debian:bullseye
```

```sh
VERSION=2.1.2
```
```sh
IMAGE=basic-resampler
docker build -t  $IMAGE -f Dockerfile .
```
```text
Sending build context to Docker daemon   16.9kB
Step 1/13 : FROM debian:bullseye
 ---> d89d848cffea
Step 2/13 : ENV  DEBIAN_FRONTEND=noninteractive
 ---> Using cache
 ---> 9121b8c2c17e
Step 3/13 : ENV CODENAME=bullseye
 ---> Using cache
 ---> 623ce4707005
Step 4/13 : RUN echo deb http://deb.debian.org/debian/ $CODENAME main contrib non-free | tee /etc/apt/sources.list
 ---> Using cache
 ---> b9e6db615ba4
Step 5/13 : RUN echo deb http://deb.debian.org/debian/ $CODENAME-updates main contrib non-free | tee -a /etc/apt/sources.list
 ---> Using cache
 ---> 5b80cd7f797b
Step 6/13 : RUN apt-get update     && apt-get install  -q -y  apt-utils curl
 ---> Using cache
 ---> 447e36d8f48f
Step 7/13 : RUN apt-get install  -q -y libfftw3-double3
 ---> Running in 8f0c62ae5117
Reading package lists...
Building dependency tree...
Reading state information...
The following additional packages will be installed:
  libgomp1
Suggested packages:
  libfftw3-bin libfftw3-dev
The following NEW packages will be installed:
  libfftw3-double3 libgomp1
0 upgraded, 2 newly installed, 0 to remove and 0 not upgraded.
Need to get 832 kB of archives.
After this operation, 2507 kB of additional disk space will be used.
Get:1 http://deb.debian.org/debian bullseye/main amd64 libgomp1 amd64 10.2.1-6 [99.9 kB]
Get:2 http://deb.debian.org/debian bullseye/main amd64 libfftw3-double3 amd64 3.3.8-2 [733 kB]
Fetched 832 kB in 0s (1778 kB/s)
Selecting previously unselected package libgomp1:amd64.
(Reading database ... 7193 files and directories currently installed.)
Preparing to unpack .../libgomp1_10.2.1-6_amd64.deb ...
Unpacking libgomp1:amd64 (10.2.1-6) ...
Selecting previously unselected package libfftw3-double3:amd64.
Preparing to unpack .../libfftw3-double3_3.3.8-2_amd64.deb ...
Unpacking libfftw3-double3:amd64 (3.3.8-2) ...
Setting up libgomp1:amd64 (10.2.1-6) ...
Setting up libfftw3-double3:amd64 (3.3.8-2) ...
Processing triggers for libc-bin (2.31-13+deb11u14) ...
Removing intermediate container 8f0c62ae5117
 ---> a9000abdc998
Step 8/13 : RUN apt-get install  -q -y libsndfile1 --fix-missing
 ---> Running in a8ff768259e2
Reading package lists...
Building dependency tree...
Reading state information...
The following additional packages will be installed:
  libflac8 libogg0 libopus0 libvorbis0a libvorbisenc2
Suggested packages:
  opus-tools
The following NEW packages will be installed:
  libflac8 libogg0 libopus0 libsndfile1 libvorbis0a libvorbisenc2
0 upgraded, 6 newly installed, 0 to remove and 0 not upgraded.
Need to get 691 kB of archives.
After this operation, 2244 kB of additional disk space will be used.
Get:1 http://deb.debian.org/debian bullseye/main amd64 libogg0 amd64 1.3.4-0.1 [27.3 kB]
Get:2 http://deb.debian.org/debian bullseye/main amd64 libflac8 amd64 1.3.3-2+deb11u2 [112 kB]
Get:3 http://deb.debian.org/debian bullseye/main amd64 libopus0 amd64 1.3.1-0.1 [190 kB]
Get:4 http://deb.debian.org/debian bullseye/main amd64 libvorbis0a amd64 1.3.7-1 [93.0 kB]
Get:5 http://deb.debian.org/debian bullseye/main amd64 libvorbisenc2 amd64 1.3.7-1 [80.6 kB]
Get:6 http://deb.debian.org/debian bullseye/main amd64 libsndfile1 amd64 1.0.31-2 [188 kB]
Fetched 691 kB in 1s (1309 kB/s)
Selecting previously unselected package libogg0:amd64.
(Reading database ... 7208 files and directories currently installed.)
Preparing to unpack .../0-libogg0_1.3.4-0.1_amd64.deb ...
Unpacking libogg0:amd64 (1.3.4-0.1) ...
Selecting previously unselected package libflac8:amd64.
Preparing to unpack .../1-libflac8_1.3.3-2+deb11u2_amd64.deb ...
Unpacking libflac8:amd64 (1.3.3-2+deb11u2) ...
Selecting previously unselected package libopus0:amd64.
Preparing to unpack .../2-libopus0_1.3.1-0.1_amd64.deb ...
Unpacking libopus0:amd64 (1.3.1-0.1) ...
Selecting previously unselected package libvorbis0a:amd64.
Preparing to unpack .../3-libvorbis0a_1.3.7-1_amd64.deb ...
Unpacking libvorbis0a:amd64 (1.3.7-1) ...
Selecting previously unselected package libvorbisenc2:amd64.
Preparing to unpack .../4-libvorbisenc2_1.3.7-1_amd64.deb ...
Unpacking libvorbisenc2:amd64 (1.3.7-1) ...
Selecting previously unselected package libsndfile1:amd64.
Preparing to unpack .../5-libsndfile1_1.0.31-2_amd64.deb ...
Unpacking libsndfile1:amd64 (1.0.31-2) ...
Setting up libogg0:amd64 (1.3.4-0.1) ...
Setting up libflac8:amd64 (1.3.3-2+deb11u2) ...
Setting up libopus0:amd64 (1.3.1-0.1) ...
Setting up libvorbis0a:amd64 (1.3.7-1) ...
Setting up libvorbisenc2:amd64 (1.3.7-1) ...
Setting up libsndfile1:amd64 (1.0.31-2) ...
Processing triggers for libc-bin (2.31-13+deb11u14) ...
Removing intermediate container a8ff768259e2
 ---> 2cb6f3909edf
Step 9/13 : ARG VERSION=2.1.2
 ---> Running in be2df68a094a
Removing intermediate container be2df68a094a
 ---> 1789c1155e75
Step 10/13 : ENV VERSION=$VERSION
 ---> Running in 83c6bbfccbf2
Removing intermediate container 83c6bbfccbf2
 ---> 70da0a963d96
Step 11/13 : RUN curl -skLo /tmp/ReSampler-Linux.deb https://github.com/jniemann66/ReSampler/releases/download/v${VERSION}/ReSampler-${VERSION}-Linux.deb
 ---> Running in 166820fa71e5
Removing intermediate container 166820fa71e5
 ---> 9c45c77bb918
Step 12/13 : ADD ReSampler-Linux.deb /tmp
 ---> c8ffc24f9d4a
Step 13/13 : RUN dpkg -i /tmp/ReSampler-Linux.deb
 ---> Running in e48e2afd148b
Selecting previously unselected package resampler.
(Reading database ... 7243 files and directories currently installed.)
Preparing to unpack /tmp/ReSampler-Linux.deb ...
Unpacking resampler (2.1.2) ...
Setting up resampler (2.1.2) ...
Removing intermediate container e48e2afd148b
 ---> 842affb6db9f
Successfully built 842affb6db9f
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
