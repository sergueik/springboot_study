### Info

Pure Perl module with dependencies installed on top of Alpine image

### Usage

#### Build

```sh
IMAGE=basic-perl-crypt-jasypt
docker build -t $IMAGE -f Dockerfile .
```
* NOTE: build will be time comsuming

#### Verify Application

```sh
NAME=example-perl-jasypt
docker run --name $NAME -it $IMAGE sh
```
* run test in the container
```sh
perl test.pl -value message -secret apple -operation encrypt
```
```text
x5p9WNNzxLAqGwt7zDkx1A==
```
```sh
perl test.pl -value 'x5p9WNNzxLAqGwt7zDkx1A==' -secret apple
```
```text
message
```
### See Also

  * `Crypt::PBE` [module](https://metacpan.org/pod/Crypt::PBE)
  * [discussion](https://www.perlmonks.org/?dislaytype=print;node_id=845861;replies=1) about implementing Jasypt in Perl
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
