### Info

Pure Perl module with dependencies installed on top of Alpine image
and with modified and trimmed Crypt::PBE installed into workdir for debugging

### Usage

* build

```sh
IMAGE1=basic-perl-lwp
docker build -t $IMAGE1 -f Dockerfile.build .
IMAGE=basic-perl-lwp-client
docker build -t $IMAGE -f Dockerfile .
```
* NOTE: the `basic-perl-lwp` build is more than somewhat time-consuming

```sh
NAME=example-perl-lwp-client
docker stop $NAME
docker container rm $NAME
docker run --name $NAME --link basic-perl-cgi -it $IMAGE sh
```
* run test in the container
```sh
HOST=basic-perl-cgi
perl client1.pl  -url "http://$HOST:80/cgi-bin/file_hash_status.cgi?inputfile=example_config.json" -output a.json
```
```text
Status: 200
```
```sh
~ # ls -l
```
```text
total 8
-rw-r--r--    1 root     root            18 Oct 18 22:21 a.json
-rwxr-xr-x    1 root     root          1304 Oct 18 22:20 test.pl
```
```sh
md5sum a.json
```
```text
9f8377db38593544a5e994006fe4e9e4  a.json
```
* repeat with the hash. Remove the file 
```sh
HOST=basic-perl-cgi
perl client1.pl  -url "http://$HOST:80/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4" -output b.json
```
```text
Status: 304
```
Confirm the file was not be downoaded:

```sh
test -f b.json
echo $?
```
```text
1
```
```sh

HOST=basic-perl-cgi
perl client2.pl  -url "http://$HOST:80/cgi-bin/file_hash_status.cgi?inputfile=example_config.json" -output a.json

```
```text
Status: 200
```

```sh
HOST=basic-perl-cgi
perl client2.pl  -url "http://$HOST:80/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4" -output b.json
```
```text
Status: 304
```
### See Also

  * CGI file upload [document](https://www.sitepoint.com/uploading-files-cgi-perl/)
  * `mirror` [method](https://metacpan.org/pod/HTTP::Tiny#mirror) in `HTTP::Tiny`
  * `mirror` [method](https://metacpan.org/pod/LWP::Simple#mirror) in `LWP::Simple`
  * `getstore` [method](https://metacpan.org/pod/LWP::Simple#getstore) in `LWP::Simple`


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
