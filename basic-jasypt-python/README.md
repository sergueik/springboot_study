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
python app3.py --value test --password password --salt 781549FD8B328283F0
DD61937BF4F27F --debug
```
```text
running debug mode
salt (encrypt): 781549fd8b328283f0dd61937bf4f27f
32
key (encrypt): 165e6f969d8dc72a3f5e1739c49fd6bf1fa9c47d9ccfcd2201f81f41b18b99cb
iv (encrypt): 980f2342e2ce49f07fe57307e8f069bc
encrypted: eBVJ/YsygoPw3WGTe/Tyf/ahtOzRz9CfJ4nQFutw7pw=
```

```sh
python app3.py --value 'eBVJ/YsygoPw3WGTe/Tyf/ahtOzRz9CfJ4nQFutw7pw=' --p
assword password --salt 781549FD8B328283F0DD61937BF4F27F --debug --operation dec
rypt
```

```text
running debug mode
salt (decrypt): 781549fd8b328283f0dd61937bf4f27f
key (decrypt): 165e6f969d8dc72a3f5e1739c49fd6bf1fa9c47d9ccfcd2201f81f41b18b99cb
iv (decrypt): 980f2342e2ce49f07fe57307e8f069bc
decrypted: test
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
  * https://cryptobook.nakov.com/mac-and-key-derivation/pbkdf2
  * https://stackoverflow.com/questions/12524994/encrypt-and-decrypt-using-pycrypto-aes-256
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
