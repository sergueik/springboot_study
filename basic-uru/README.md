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
