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
export IMAGE1=basic-python-crypto
docker image build -t $IMAGE1 -f Dockerfile.build .
export IMAGE=basic-jasypt-python
docker image build -t $IMAGE -f Dockerfile .
```
```sh
export NAME=basic-jasypt-python
docker container stop $NAME
docker container rm $NAME
docker run --name $NAME -it $IMAGE sh
```
#### AES Encryption
in the container
```sh
python app3.py --value test --password password
```
```text
salt (encrypt): f55cae7681a9081369c877b7bc7fb077
key (encrypt): e59f5596fbf97f06ff335bc42d106f2c6f1c2c8f032aaeadb3b5fd3bf7277a3a
iv (encrypt): ce8fee6a30164659f4e23f8917ed572b
encrypted: 9VyudoGpCBNpyHe3vH+wd86P7mowFkZZ9OI/iRftVyuwCbMe+3K6kjZuCCNLNJkl

```
* NOTE: one can provide `salt` argument too:
```sh
python app3.py --value test --password password --salt f55cae7681a9081369c877b7bc7fb077
```

```text
salt (encrypt): f55cae7681a9081369c877b7bc7fb077
key (encrypt): e59f5596fbf97f06ff335bc42d106f2c6f1c2c8f032aaeadb3b5fd3bf7277a3a
iv (encrypt): ce8fee6a30164659f4e23f8917ed572b
encrypted: 9VyudoGpCBNpyHe3vH+wd86P7mowFkZZ9OI/iRftVyuwCbMe+3K6kjZuCCNLNJkl
```

```sh
python app3.py --value '9VyudoGpCBNpyHe3vH+wd86P7mowFkZZ9OI/iRftVyuwCbMe+3K6kjZuCCNLNJkl' --password password --debug --operation decrypt
```

```text
running debug mode
salt (decrypt): f55cae7681a9081369c877b7bc7fb077
key (decrypt): e59f5596fbf97f06ff335bc42d106f2c6f1c2c8f032aaeadb3b5fd3bf7277a3a
iv (decrypt): ce8fee6a30164659f4e23f8917ed572b
decrypted: test
```
The `SHA512` `AES256` encryption appears to be compatible with Perl, C#, Java

#### DES Encryption

* NOTE: will need to use `python:2.7.17`-based images.
```sh
python app1.py --operation decrypt --value 6QavZfkiUlAqQNmFiP0E0g== --password password
```
```text
test
```
the `MD5` `DES` encryption appears to be compatible with Perl, C#, Java

### Missing Module
```sh
curl -O https://files.pythonhosted.org/packages/02/c0/6a2376ae81beb82eda645a091684c0b0becb86b972def7849ea9066e3d5e/pbkdf2-1.3.tar.gz
tar xzvf pbkdf2-1.3.tar.gz
```

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
  * https://cryptobook.nakov.com/mac-and-key-derivation/pbkdf2
  * https://stackoverflow.com/questions/12524994/encrypt-and-decrypt-using-pycrypto-aes-256
  * https://pypi.org/project/pbkdf2/#files
  * https://stackoverflow.com/questions/1112618/import-python-package-from-local-directory-into-interpreter
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

