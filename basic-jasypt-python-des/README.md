### Info

This repository contains a modified of [jasypt4py](https://github.com/fareliner/jasypt4py/tree/master/jasypt4py) repo on __Python 2.7.17__ [Alpine image](https://hub.docker.com/layers/library/python/2.7.17-alpine/images/sha256-9498bbbbf0bc567b2b729853f0a1eaac025401d1a089ee96cd237f58da2e7400)
for PBKD password-based key derivation function and DES encryption compatible with older [Jasypt](http://www.jasypt.org) examples based on the release `1.9.3` when M5D5 DES was default encrption
and .Net implementation [jasypt-csharp](https://github.com/sergueik/powershell_samples/tree/master/csharp/jasypt-csharp)

### NOTE 
This works on Python __2.7__ but fails on Python __3.8.2__ wuith 
```text
TypeError: Object type <class 'str'> cannot be passed to C code
```
in `encryptor.py`

during `encrypt`

and 
```text
key = password + salt
TypeError: can only concatenate str (not "bytes") to str
```
during `decrypt`.
### Usage

* build image in stages
```sh
export IMAGE=basic-jasypt-python-des
docker image build -t $IMAGE1 -f Dockerfile.build .
```

```sh
export IMAGE=basic-jasypt-python-des
docker image build -t $IMAGE -f Dockerfile  .
```
```sh
export NAME=basic-jasypt-python
docker run --name $NAME -it $IMAGE sh
```

in the container

```sh
python app.py --value test --password password
```
```text
bda9aI5SDLaKkRVBobkm2w==
```
```sh
python app.py --operation decrypt --value bda9aI5SDLaKkRVBobkm2w== --password password
```
```text
test
```
the `MD5` `DES` encryption appears to be compatible with Perl, C#, Java: 

encrypt sample test with .Net app:
```powershell
Program\bin\Debug\Program.exe -value:something -password:password
```


```sh
python app.py --operation decrypt --value 'LWOxm4m1O+f/Zrn6WDfrPiHmZl+IugiE' --password password
```
```text
something
```
### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
docker image prune -f
```
### See Also 
  * `PBEWithMD5AndDES` in Python [example](https://github.com/lemonprogis/python-jasypt)
  * https://github.com/hei1233212000/python-jasypt-pbewithmd5anddes-poc

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
