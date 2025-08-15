```
http://192.168.99.100:8080/encrypt?password=secret&value=test
```
```json
{"result":"jLMUcMfHHDctVdW06D1W9st6iIJIv1Obcd7jymV/y4yqo9Phspn0KxbVskVRHJaw","original":"test"}
```
```
http://192.168.99.100:8080/decrypt?password=secret&value=jLMUcMfHHDctVdW06D1W9st6iIJIv1Obcd7jymV%2Fy4yqo9Phspn0KxbVskVRHJaw
```json
{"result":"test","original":"jLMUcMfHHDctVdW06D1W9st6iIJIv1Obcd7jymV/y4yqo9Phspn0KxbVskVRHJaw"}
```
```sh
export IMAGE=vault
docker build --build-arg "GID=$(id -g)" --build-arg "UID=$(id -u)" -t $IMAGE -f Dockerfile .
```
* NOTE: on a Windows host the values returned by `id` will be big integer numbers like `197609`. On a Linux host you will posibly see numbers like `1000`

* run the container interactively, override the entry point, and exposing the default port, to execute vault commands in the foreground
```sh
NAME=vault
docker run -d --name $NAME -p 8200:8200 $IMAGE
```
```sh
docker logs $NAME  2>1| grep Token:
```
```text
XXXX XXXXX: XXX.XXXXXXX0XXXXXXXX9X4XXX1X
```
```sh
docker run --rm --name $NAME --entrypoint "" -p 8200:8200 -it $IMAGE sh
```
```sh
vault -version
```
```text
Vault v1.12.2 (415e1fe3118eebd5df6cb60d13defdc01aa17b03), built 2022-11-23T12:53:46Z
```
  * start dev server with options
  * lookup token

```sh
export VAULT_ADDR='http://0.0.0.0:8200'
TOKEN=$(echo 'vault token' | base64 -)
echo $TOKEN
vault server -dev -dev-listen-address=0.0.0.0:8200 -dev-root-token-id=$TOKEN
```
this will print to console
```text
The unseal key and root token are displayed below in case you want to
seal/unseal the Vault or re-authenticate.

Unseal Key: t1Pz89JEGcc4p/tYrJYlKEFFVLbHQ4kD+diNMQ6f2LE=
Root Token: dmF1bHQgdG9rZW4K

Development mode should NOT be used in production installations!
```
