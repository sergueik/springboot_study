### Info

### Usage

```sh
sudo rm -fr .ash_history .cpanm
```

* bring up mysql server
```sh
NAME1='mysql-server-alpine'
docker container start $NAME1
```
buld basic Perl image (commands installing dbi are to be run interactively for this example):
```sh
IMAGE2=alpine-perl
docker build -t $IMAGE2 -f Dockerfile.$IMAGE2 .
```
```sh
NAME2=alpine-perl
docker container rm $NAME2
docker run -it --link $NAME1 --name $NAME2 -v $(pwd):/root $IMAGE2 sh
```

in the root shell
```sh
#
```
install dependencies using `apt` instead of `cpan`, but insyall `cpanm`  to validate:

```sh
apk add perl-app-cpanminus make gcc perl-dbi perl-dbd-mysql
```
validate
```sh
perl -MDBI -e 'print $DBI::VERSION'
```

```text
1.643
```
alternatively
```sh
cpanm --no-wget DBI
```
```text
DBI is up to date. (1.643)
```
```sh
cpanm --no-wget DBD::mysql
```
```text
DBD::mysql is up to date. (4.050)
```
* confirm connectivity
```sh
nc -z mysql-server-alpine 3306
echo $?
```
```text
0
```
```sh
export DBHOST='mysql-server-alpine'
perl dbi_test.pl 2>&1
```
```text
mysql
test
database is up
10.3.25-MariaDB-log

```

### See Also
   * https://zetcode.com/perl/dbi/
   * https://www.perltutorial.org/perl-dbi/

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
