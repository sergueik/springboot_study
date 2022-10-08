### Info

Container for [alpine](https://www.alpinelinux.org) hosted [uru](https://rubyinstaller.org/add-ons/uru.html) environment created based on [uru_serverspec](https://github.com/sergueik/uru_serverspec) project

### Usage
```sh
docker build -f Dockerfile -t basic-uru .
docker run -it basic-uru
``` 
```sh
docker exec -it $(docker ps -q) sh
```
### Cleanup
```sh
docker container prune -f
docker image prune -f
docker image rm -f basic-uru
```
### Note
Docker recursive copy is time-consuming (20 minute)

### See Also
  * [Docker image that can be used to run docker-in-docker and has ruby installed](https://github.com/sirech/dind-ruby)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
