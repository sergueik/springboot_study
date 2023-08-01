### Usage
```sh
docker build -t python-config -f Dockerfile  .
```
```sh
docker run -p 6000:6000 -d python-config
```
```sh
curl http://192.168.99.100:6000/config
```
```json
{"sergueik119":{},"sergueik53":{"PORTS":[22,443,3306]},"sergueik71":{"PORTS":[54
32]}}
```
