### Info

Docker project with some repository hosted in the Docker image based on [Dockerising a Perl application](https://robn.io/docker-perl/) exploring the latest committed hash in the target repository to trigger git pull into the image converted to run on alpine openjdk jre base image, with the conditional git apk update

### Usage

* Build the container 2 times
```sh
docker build -f Dockerfile -t basic-workspace .
```
the second iteration will skip every step with `Using cache` verdict
and creating a potentially stale state of the repository workspace burned into the image.

Adding the following argument and passing the latest hash via build arg command line option does not force Docker to re-run the step in question

```sh
PROJECT=springboot_study
APPS_DIR=/$HOME/workspace
LATEST_HASH=$(git --git-dir=${APPS_DIR}/${PROJECT}/.git rev-parse --short HEAD)
```
this aparenrently ignores the hash change:
```sh
docker build -f Dockerfile --build-arg "LATEST_HASH=$(git --git-dir=${APPS_DIR}/${PROJECT}/.git rev-parse --short HEAD)" -t basic-workspace .
```

the only way seems to update the `Dockerfile` directly:
```sh
sed -i "s|LATEST_HASH=\".*\"|LATEST_HASH=\"${LATEST_HASH}\"|" Dockerfile
docker build -f Dockerfile -t basic-workspace .
```
This will lead to the line

```sh

ARG LATEST_HASH=""
RUN echo "latest hash is ${LATEST_HASH}"
RUN echo "Pulling hash ${LATEST_HASH}" && cd ${APPS_DIR} && git clone https://github.com/sergueik/${PROJECT}
```
be reappled
```sh
Step 10/11 : RUN echo "Pulling hash ${LATEST_HASH}" &&     cd ${APPS_DIR} &&     git clone https://github.com/sergueik/${PROJECT}
 ---> Running in 5dc6d4df9f40
Pulling hash abcd
Cloning into 'springboot_study'...
Removing intermediate container 5dc6d4df9f40
 ---> 89623d0015f0
```
Note: separating the lines
```sh
ARG LATEST_HASH=""
RUN echo "latest hash is ${LATEST_HASH}"
RUN cd ${APPS_DIR} && git clone https://github.com/sergueik/${PROJECT}
```

is also possible:
```sh
LATEST_HASH='efgh'
sed -i "s|LATEST_HASH=\".*\"|LATEST_HASH=\"${LATEST_HASH}\"|" Dockerfile
docker build -f Dockerfile -t basic-workspace .
```
leads
```sh
Step 8/11 : ARG LATEST_HASH="efgh"
 ---> Running in 8fd9e2a64364
Removing intermediate container 8fd9e2a64364
 ---> 26d0ae873524
Step 9/11 : RUN echo "latest hash is ${LATEST_HASH}"
 ---> Running in 585e6e09dbef
latest hash is efgh
Removing intermediate container 585e6e09dbef
 ---> e8bc081e0890
Step 10/11 : RUN cd ${APPS_DIR} && git clone https://github.com/sergueik/${PROJECT}
 ---> Running in 07aad4d43910
Cloning into 'springboot_study'...
Removing intermediate container 07aad4d43910
 ---> e8fdcf455178
```
the container can be exported or used per its original purpose:

```sh
docker run basic-workspace
```

and used for one command like
```sh
IMAGE_NAME='basic-workspace'
PROJECT='springboot_study'
USER=...
PASSWORD=...
# TODO: url encode
docker exec -it $(docker container ls | grep $IMAGE_NAME | head -1 | awk '{print $1}') sh -c "cd /opt/apps/$PROJECT ; git pull https://${USER}:${PASSWORD}@github.com/${PROJECT}"
```
alternatively use the shell script variation to specify 
an arbitrary branch `$BRANCH`:
```sh
git remote remove origin
git remove add origin "https://${USER}:${PASSWORD}@github.com/${PROJECT}"
git pull origin $BRANCH
git remove remove origin
```
(all concatenated into shell argument) 

### Use Github API
* generate a new token through __Settings/Developer settings__ [Personal access tokens](https://github.com/settings/tokens)

*  use it with `curl`:
```sh
curl -O -J -L -u ${TOKEN}:x-oauth-basic https://github.com/$USER/${PROJECT}/archive/${LATEST_HASH}.zip
```
will download the selected commit of the workspace.
NOTE: it will save under a name it chooses, ignoring the `-O` argument e.g.:
```sh
curl: Saved to filename 'springboot_study-4cc172c5cb4bc7d72641ff9466b8b909a78773ed.zip'
```
NOTE: the variant
```sh
USER=...
PASSWORD=...

LATEST_HASH=$(git --git-dir=${APPS_DIR}/${PROJECT}/.git rev-parse --short HEAD)
curl -O -J -L -u "${USER}:${PASSWORD}" https://github.com/$USER/${PROJECT}/archive/${LATEST_HASH}.zip
```
will lead to 
```text
Deprecated authentication method. Create a Personal Access Token to access: https://github.com/settings/token
```
be written into
`${LATEST_HASH}.zip`
### Cleanup
destroy all orphaned images afterwards
```sh
docker image prune -f
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
