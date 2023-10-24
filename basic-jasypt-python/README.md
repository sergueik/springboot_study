### Info

This repository contains a modified   AES Encrytion Example in Python [gist](https://gist.github.com/wowkin2/a2b234c87290f6959c815d3c21336278) combined with

Pypi [module](https://pypi.org/project/pbkdf2) for PBKDF2 password-based key derivation function
packaged for __Python 3.8.2__ on [alpine container](https://hub.docker.com/layers/library/python/3.8.2-alpine/images/sha256-85d79ea7f22dd6eefb1101753129b5f681af7622c92f1e74f5cd58c18fb5dabd)
demonstrating SHA512 AES256 encryption compatible with [Jasypt](http://www.jasypt.org)
and .Net implementation [pbkdf2-csharp](https://github.com/sergueik/powershell_samples/tree/master/csharp/pbkdf2-csharp) of PBKDF2 / AES

### Usage

* build image in stages
```sh
export IMAGE1=basic-python-crypto
docker image build -t $IMAGE1 -f Dockerfile.build .
```
```sh
export IMAGE=basic-jasypt-python
docker image build -t $IMAGE -f Dockerfile .
```
```sh
export NAME=basic-jasypt-python
docker container stop $NAME
docker container rm $NAME
docker run --name $NAME -it $IMAGE sh
```

in the container
```sh
python app.py --value test --password password
```
```text
salt (encrypt): f55cae7681a9081369c877b7bc7fb077
key (encrypt): e59f5596fbf97f06ff335bc42d106f2c6f1c2c8f032aaeadb3b5fd3bf7277a3a
iv (encrypt): ce8fee6a30164659f4e23f8917ed572b
encrypted: 9VyudoGpCBNpyHe3vH+wd86P7mowFkZZ9OI/iRftVyuwCbMe+3K6kjZuCCNLNJkl

```
* NOTE: one can provide `salt` argument too:
```sh
python app.py --value test --password password --salt f55cae7681a9081369c877b7bc7fb077
```

```text
salt (encrypt): f55cae7681a9081369c877b7bc7fb077
key (encrypt): e59f5596fbf97f06ff335bc42d106f2c6f1c2c8f032aaeadb3b5fd3bf7277a3a
iv (encrypt): ce8fee6a30164659f4e23f8917ed572b
encrypted: 9VyudoGpCBNpyHe3vH+wd86P7mowFkZZ9OI/iRftVyuwCbMe+3K6kjZuCCNLNJkl
```
* NOTE: with `salt` and `password` provided, the `encrypted` value will remain the same between runs

```sh
python app.py --value '9VyudoGpCBNpyHe3vH+wd86P7mowFkZZ9OI/iRftVyuwCbMe+3K6kjZuCCNLNJkl' --password password --debug --operation decrypt
```

```text
running debug mode
salt (decrypt): f55cae7681a9081369c877b7bc7fb077
key (decrypt): e59f5596fbf97f06ff335bc42d106f2c6f1c2c8f032aaeadb3b5fd3bf7277a3a
iv (decrypt): ce8fee6a30164659f4e23f8917ed572b
decrypted: test
```
The `SHA512` `AES256` encryption appears to be compatible with Perl, C#, Java

### Testing Pad

```sh
python pad.py --value test
```
```text
pad: 746573740c0c0c0c0c0c0c0c0c0c0c0c
```
```sh
python pad.py --value '746573740c0c0c0c0c0c0c0c0c0c0c0c' --operation unpad
```
```text
raw: 746573740c0c0c0c0c0c0c0c0c0c0c0c
raw result: 74657374
result: "test"
```
### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
docker image prune -f
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


