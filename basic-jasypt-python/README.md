### Info

This repository contaians a modified of [jasypt4py](https://github.com/fareliner/jasypt4py/tree/master/jasypt4py) repo. This runs on Python __2.7__ (on  Python __3.8.2__ fails with

```text
TypeError: Object type <class 'str'> cannot be passed to C code
```
in `encryptor.py`

### Usage

```sh
export IMAGE=basic-jasypt-python
docker image build -t $IMAGE -f Dockerfile  .
```
```sh
export NAME=basic-jasypt-python
docker run --name $NAME -it $IMAGE sh
```

in the container
```sh
python app.py
```
```text
algorithm: PBEWITHSHA256AND256BITAES-CBC
N417ozOd+LpLotOsxLoirOgBzrfLbVu3dV9obc0Vsck=
test
```
```text
algorithm: PBEWITHSHA512AND256BITAES-CBC
kuyvA9EAYwwussMaD4Lvwan4s4dq5jRGkMO4NP4kqhc=
test
```

NOTE: the `SHA512` example is not valid. the `SHA256` example was not verified yet - turns out to not be valid either

```sh
python jasypt_md5_des_ex.py --value test --password password
```
```text
6QavZfkiUlAqQNmFiP0E0g==
```
```sh
python jasypt_md5_des_ex.py --operation decrypt --value 6QavZfkiUlAqQNmFiP0E0g== --password password
```
```text
test
```
the `MD5` `DES` encryption appears to be compatible with Perl, C#, Java

### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
```
### See Also 
  * `PBEWithMD5AndDES` in Python [example](https://github.com/lemonprogis/python-jasypt)
  * https://github.com/hei1233212000/python-jasypt-pbewithmd5anddes-poc

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
