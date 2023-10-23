### Info 

https://cryptography.io/en/latest/hazmat/primitives/key-derivation-functions/

### Usage

* build image in stages
```sh
export IMAGE1=basic-jasypt-python-aes-2
docker image build -t $IMAGE1 -f Dockerfile.build .
```

```sh
export IMAGE=basic-jasypt-python-aes
docker image build -t $IMAGE -f Dockerfile  .
```
```sh
export NAME=basic-jasypt-python-aes
docker run --name $NAME -it $IMAGE sh
```
or
```sh
export NAME=basic-jasypt-python-aes
docker start $NAME
docker exec -it $NAME sh
```
in the container

```sh
python app.py
```
```text
salt: cb4743d5d7b83b21f38fa10c7b689c83
key: 7de7f736fb44a3650f3361ad019d5cb6b1a2dd5fdac2b1ab7adb7f75772dea20
iv: 593643e1ae1d2b25489ec2d6df95c147
enc: 26a56f13dbf83a75e0b019528c99007b
encrypted: y0dD1de4OyHzj6EMe2icg1k2Q+GuHSslSJ7C1t+VwUcmpW8T2/g6deCwGVKMmQB7
```

then in the `basic-jasypt-python` container

```sh
python app.py --value '1Rk/oslwF0uRGekY9QWPM2L5T9RiyWozrMw/dk4AdcyApF/KoezPo73Lkzo621oO' --password password --operation decrypt --salt cb4743d5d7b83b21f38fa10c7b689c83
salt (decrypt): d5193fa2c970174b9119e918f5058f33
enc: 80a45fcaa1eccfa3bdcb933a3adb5a0e
key (decrypt): 98b7066808116f3c1d2fa0419dbb1debe7e78d61397fff04512271a198349500
iv (decrypt): 62f94fd462c96a33accc3f764e0075cc
dec: 74657374746573747465737474657374
dec(2): testtesttesttest
dec(3): b'testtesttesttest'
decrypted:
```


```sh
python app.py --value testtesttesttest --password password
```
```text
salt (encrypt): 9633e155827c2b4a35c78fc8a2481348
key (encrypt): d7f128b66c80a3af9b9aea4c8448364922e405e27eae093012f5cea5f3454268
iv (encrypt): 75867233419ac3a2fe110c558f776a11
encrypted: ljPhVYJ8K0o1x4/IokgTSHWGcjNBmsOi/hEMVY93ahEWsdwh0BWQBunQ39wiWmDf0hS/azeZyw9YvRaGAflkFg==
```
```sh
python app.py --value 'ljPhVYJ8K0o1x4/IokgTSHWGcjNBmsOi/hEMVY93ahEWsdwh0BWQBunQ39wiWmDf0hS/azeZyw9YvRaGAflkFg==' --password password --operation decrypt
```
```text
```
### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
docker image prune -f
```
### See Also 
  * https://cryptography.io/en/latest/hazmat/primitives/key-derivation-functions/
  * https://github.com/pyca/cryptography/issues/1958
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
