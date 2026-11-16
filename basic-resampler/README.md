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
```sh
docker build --no-cache --progress=plain -t $IMAGE -f Dockerfile .
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
