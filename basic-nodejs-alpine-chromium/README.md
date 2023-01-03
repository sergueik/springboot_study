### Info


### Usage
 * build
```sh
IMAGE=nodejs-alpine-chromium
docker build -t $IMAGE -f Dockerfile .
```
* run
```sh
docker run -it $IMAGE
```
```
Google Search button text: Google Search
```
### Note

hosting on alpine gives the following diskspace savings
```text
nodejs-alpine-chromium             latest                  7fa9903ce467   56 seconds ago       564MB
nodejs-alpine                      latest                  7398f39ff5e1   8 minutes ago        250MB
basic-nodejs-selenium              latest                  ae3e5076ed9e   3 hours ago          1.68GB
node                               latest                  548714e444f4   12 days ago          998MB
```
