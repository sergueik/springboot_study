### Info

This repository contaians a modified `PBEWithMD5AndDES` in Python [example](https://github.com/lemonprogis/python-jasypt)

NOTE: The `MD5`/`DES` example can only run on Python __2.7__ . 
On  Python __3.8.2__ fails with

```text
TypeError: Object type <class 'str'> cannot be passed to C code
```
and 
```text
key = password + salt
TypeError: can only concatenate str (not "bytes") to str
```
during `encrypt`
and 

```text
 TypeError: can only concatenate str (not "bytes") to str
```

during `decrypt`.

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
python app3.py --value test --password password
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

  * https://github.com/hei1233212000/python-jasypt-pbewithmd5anddes-poc
  * https://onboardbase.com/blog/aes-encryption-decryption/ 
  * https://cryptobook.nakov.com/symmetric-key-ciphers/aes-encrypt-decrypt-examples
  * https://www.quickprogrammingtips.com/python/aes-256-encryption-and-decryption-in-python.html
  * https://stackoverflow.com/questions/14179784/python-encrypting-with-pycrypto-aes
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
