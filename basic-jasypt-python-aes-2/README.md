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
python app.py --operation encrypt --password secret --value 'test message'
```
```text
salt: abd0086b798ec085d229beb3520b08e0
key: 98fd29a6e5957ad9590cd678773afcec0f60c492401970ee7bd06f949f329883
iv: c2de92211004ec5f8d7bdf4a86a2136d
len(raw): 16
enc: 983da31b00784e9935ea5613a086cd73
encrypted: q9AIa3mOwIXSKb6zUgsI4MLekiEQBOxfjXvfSoaiE22YPaMbAHhOmTXqVhOghs1z
```


```sh
python app.py --operation encrypt --salt ce0636d05a87d2e1130ca19af99023c1 --password secret --value 'test message'
```
```text
salt: ce0636d05a87d2e1130ca19af99023c1
key: 88c876fafce6b54d4708bc32cb37cd7f23797cdad63401079a4504e3248e4121
iv: a468c35cba0fff8a7ff5acf39a81185f
len(raw): 16
enc: c22b088988c6bdeee2a5b7ec2e673668
encrypted: zgY20FqH0uETDKGa+ZAjwaRow1y6D/+Kf/Ws85qBGF/CKwiJiMa97uKlt+wuZzZo
```
```sh
python app.py --operation decrypt --salt ce0636d05a87d2e1130ca19af99023c1 --password secret --value 'zgY20FqH0uETDKGa+ZAjwaRow1y6D/+Kf/Ws85qBGF/CKwiJiMa97uKlt+wuZzZo'
```
```text
salt: ce0636d05a87d2e1130ca19af99023c1
key: 88c876fafce6b54d4708bc32cb37cd7f23797cdad63401079a4504e3248e4121
iv: a468c35cba0fff8a7ff5acf39a81185f
enc: c22b088988c6bdeee2a5b7ec2e673668
dec: 74657374206d65737361676504040404
dec(2): "test message"
decrypted: test message
```
the `salt` argument is not required (can be used for debugging)
```sh
python app.py --operation decrypt --password secret --value 'zgY20FqH0uETDKGa+ZAjwaRow1y6D/+Kf/Ws85qBGF/CKwiJiMa97uKlt+wuZzZo'
```
```text
salt: ce0636d05a87d2e1130ca19af99023c1
key: 88c876fafce6b54d4708bc32cb37cd7f23797cdad63401079a4504e3248e4121
iv: a468c35cba0fff8a7ff5acf39a81185f
enc: c22b088988c6bdeee2a5b7ec2e673668
dec: 74657374206d65737361676504040404
dec(2): "test message"
decrypted: test message

```
then in the `basic-jasypt-python` container

```sh
python app.py --password secret --value 'zgY20FqH0uETDKGa+ZAjwaRow1y6D/+Kf/Ws85qBGF/CKwiJiMa97uKlt+wuZzZo' --operation decrypt
```
```text
salt (decrypt): ce0636d05a87d2e1130ca19af99023c1
enc: c22b088988c6bdeee2a5b7ec2e673668
key (decrypt): 88c876fafce6b54d4708bc32cb37cd7f23797cdad63401079a4504e3248e4121
iv (decrypt): a468c35cba0fff8a7ff5acf39a81185f
dec: 74657374206d65737361676504040404
dec(2): "test message"
decrypted: test message
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
  * https://stackoverflow.com/questions/31569339/importerror-no-module-named-cryptography-hazmat-backends-boxsdk-on-mac


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
