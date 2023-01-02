### Info

https://github.com/nodejs/docker-node/tree/main/18/alpine3.15

### Usage

```sh
IMAGE=nodejs-alpine
docker build -f Dockerfile -t $IMAGE . 
```
### Note

* cannot install modules:
```sh
docker run -it nodejs-alpine sh
```
```sh
/ # node -v
```
```text
v18.10.0
```
```sh
/ # npm -v
```
```text
8.19.2
```

```sh
/ # npm install selenium-webdriver
```
```text
npm ERR! Tracker "idealTree" already exists

npm ERR! A complete log of this run can be found in:
npm ERR!     /root/.npm/_logs/2023-01-02T23_19_58_488Z-debug-0.log

```
