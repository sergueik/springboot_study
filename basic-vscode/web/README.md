### Info
 this directory demonstrates installing  Visual Studio Code server silently in a light Docker container and also attempts to pre-install [Bruno](https://www.usebruno.com/) [API Client](https://www.usebruno.com/downloads) and [VS Code Extension](https://marketplace.visualstudio.com/search?term=bruno&target=VSCode&category=All%20categories&sortBy=Relevance)

### Testing

* Download extension

![VS Code Bruno Download Extension](screenshots/vdownload-extension.png "VS Code Downloading Bruno Extenison VSIX - via VS Code")

* build the image

```sh
docker pull ruanbekker/vscode-server:slim
```
```sh
docker build -t vscode-web -f Dockerfile .
```
logging (first run)
```txt
DEPRECATED: The legacy builder is deprecated and will be removed in a future release.
            Install the buildx component to build images with BuildKit:
            https://docs.docker.com/go/buildx/

Sending build context to Docker daemon  33.16MB
Step 1/17 : FROM ruanbekker/vscode-server:slim
 ---> 03d71236aa80
Step 2/17 : ARG VERSION=4.4.0
 ---> Using cache
 ---> e84f0a3f41b3
Step 3/17 : ARG VSCODEUSER=coder
 ---> Using cache
 ---> b2ff4201667e
Step 4/17 : ENV HOME=/home/${VSCODEUSER}
 ---> Using cache
 ---> fe69cda8e187
Step 5/17 : USER root
 ---> Using cache
 ---> 84ea5b98c10d
Step 6/17 : RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*
 ---> Using cache
 ---> 52e291666671
Step 7/17 : WORKDIR /tmp
 ---> Using cache
 ---> 4bdb2973fd9c
Step 8/17 : ADD bruno-api-client.bruno-${VERSION}.vsix .
 ---> Using cache
 ---> 5e162b50f852
Step 9/17 : RUN mkdir -p ${HOME}/Files     && mv bruno-api-client.bruno-${VERSION}.vsix ${HOME}/Files/bruno-api-client.bruno.vsix
 ---> Using cache
 ---> 96653fbe622f
Step 10/17 : RUN mkdir -p "${HOME}/.local/share/code-server/User"
 ---> Using cache
 ---> efa7f458eb43
Step 11/17 : COPY Files/settings.json "${HOME}/.local/share/code-server/User/settings.json"
 ---> 30cfe9c9e25a
Step 12/17 : COPY Files/keybindings.json "${HOME}/.local/share/code-server/User/keybindings.json"
 ---> 4f5006f2f86a
Step 13/17 : RUN chown -R ${VSCODEUSER}:${VSCODEUSER} ${HOME}
 ---> Running in 486ef8b472ae
 ---> Removed intermediate container 486ef8b472ae
 ---> 9c3078d93a3f
Step 14/17 : USER ${VSCODEUSER}
 ---> Running in 670cce5651f3
 ---> Removed intermediate container 670cce5651f3
 ---> 0a29579fd3e6
Step 15/17 : RUN code-server --install-extension ${HOME}/Files/bruno-api-client.bruno.vsix --force
 ---> Running in 07fd8ab6d034
[2025-12-20T01:55:32.941Z] info  Wrote default config file to ~/.config/code-server/config.yaml
Installing extensions...
Extension 'bruno-api-client.bruno.vsix' was successfully installed.
 ---> Removed intermediate container 07fd8ab6d034
 ---> 8b559a615e63
Step 16/17 : EXPOSE 8080
 ---> Running in 45b85e27c2b1
 ---> Removed intermediate container 45b85e27c2b1
 ---> 128fa1673140
Step 17/17 : HEALTHCHECK --interval=30s --timeout=5s --start-period=10s CMD curl -f http://localhost:8080/ || exit 1
 ---> Running in 5b307e3832b5
 ---> Removed intermediate container 5b307e3832b5
 ---> 3ffdf4cfb9fa
Successfully built 3ffdf4cfb9fa
Successfully tagged vscode-web:latest
```
```sh
docker inspect ruanbekker/vscode-server:slim | jq '.[0].Config.Env'
```
```json
[
  "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
  "CODE_SERVER_VERSION=4.9.1",
  "HTTPS_ENABLED=false",
  "APP_BIND_HOST=0.0.0.0",
  "APP_PORT=8080",
  "USER=coder",
  "UID=1000",
  "GID=",
  "LANG=en_US.UTF-8",
  "SHELL=/bin/bash"
]
```


if the `USER` changes from `coder`, update `Dockerfile` accordingly
```sh
docker image ls vscode-web:latest
```
```text
REPOSITORY   TAG       IMAGE ID       CREATED          SIZE
vscode-web   latest    3ffdf4cfb9fa   36 seconds ago   899MB
```

link vscode container to some basic container
```sh
docker pull clue/json-server
```
```sh
echo '{ "posts": [ { "id": 1, "title": "json-server", "author": "typicode" } ], "comments": [ { "id": 1, "body": "some comment", "postId": 1 } ], "profile": { "name": "typicode" } }' > ./db.json
```
```sh
APP='app'
docker run --name $APP -d -p 80:80 -v "$(pwd)/db.json:/data/db.json" clue/json-server
```
```sh
docker run -d -p 8080:8080 --link $APP vscode-web:latest
```
```text
52e9555e54fe3fad93dd7b67ad1a3472f102d7d7cf6b0dc4dee30d700d18b258
```
```sh
ID=$(docker ps --format '{{.ID}} {{.Image}}' | grep vscode-web:latest | cut -f 1 -d ' ')
```
```sh
echo $ID
```
```text
39732f250fec
```
```sh
docker exec -t $ID whoami
```

```text
coder
```


wait...

```sh
docker container ls
```
```text
CONTAINER ID   IMAGE               COMMAND                  CREATED          STATUS                             PORTS                                         NAMES
39732f250fec   vscode-web:latest   "/usr/bin/entrypoint…"   36 seconds ago   Up 35 seconds (health: starting)   0.0.0.0:8080->8080/tcp, [::]:8080->8080/tcp   romantic_jennings
```

until
```sh
docker container ls
```
```text
CONTAINER ID   IMAGE               COMMAND                  CREATED          STATUS                             PORTS                                         NAMES
39732f250fec   vscode-web:latest   "/usr/bin/entrypoint…"   36 seconds ago   Up 35 seconds (health: starting)   0.0.0.0:8080->8080/tcp, [::]:8080->8080/tcp   romantic_jennings
```
alternatively

```sh
until [ "$(docker inspect -f {{.State.Status}} $APP)" == "running" ]; do
  echo "Waiting for container ${APP} to be running..."
  sleep 2
done
```
> Note: there is no `.State.Health.Status`:
```text
template parsing error: template: :1:8: executing "" 
at <.State.Health.Status>: map has no entry for key "Health"
```

![VS Code Login Screen](screenshots/login.png "login screen")

```sh
docker logs $ID
```
```text
[2025-12-20T02:04:20.392Z] info  code-server 4.9.1 f7989a4dfcf21085e52157a01924d79d708bcc05
[2025-12-20T02:04:20.393Z] info  Using user-data-dir ~/.local/share/code-server
[2025-12-20T02:04:20.408Z] info  Using config file ~/.config/code-server/config.yaml
[2025-12-20T02:04:20.408Z] info  HTTP server listening on http://0.0.0.0:8080/ 
[2025-12-20T02:04:20.408Z] info    - Authentication is enabled
[2025-12-20T02:04:20.408Z] info      - Using password from ~/.config/code-server/config.yaml
[2025-12-20T02:04:20.408Z] info    - Not serving HTTPS 
```

```sh
docker exec $ID cat /home/coder/.config/code-server/config.yaml
```
```text
bind-addr: 127.0.0.1:8080
auth: password
password: 0328d7c6d4e071b54ab175e0
cert: false
```
Log in

![VS Code IDE Screen](screenshots/ide.png "IDE screen")


alternatively, use provided `docker-compose.yaml` and run

```sh
docker-compose up --build --detach
```
> NOTE: the old `docker-compose` version does not recognize `--wait`

```sh
DEPRECATED: The legacy builder is deprecated and will be removed in a future release.
            Install the buildx component to build images with BuildKit:
            https://docs.docker.com/go/buildx/

Sending build context to Docker daemon  33.36MB
Step 1/17 : FROM ruanbekker/vscode-server:slim
 ---> 03d71236aa80
Step 2/17 : ARG VERSION=4.4.0
 ---> Running in 7ba1481d3e39
 ---> Removed intermediate container 7ba1481d3e39
...
Successfully built 8f563d7c4dfa
Successfully tagged web_vscode-web:latest
Creating vscode-web ... done
Attaching to vscode-web
vscode-web    | [2025-12-20T02:56:07.996Z] info  code-server 4.9.1 f7989a4dfcf21085e52157a01924d79d708bcc05
vscode-web    | [2025-12-20T02:56:07.997Z] info  Using user-data-dir ~/.local/share/code-server
vscode-web    | [2025-12-20T02:56:08.013Z] info  Using config file ~/.config/code-server/config.yaml
vscode-web    | [2025-12-20T02:56:08.014Z] info  HTTP server listening on http://0.0.0.0:8080/ 
vscode-web    | [2025-12-20T02:56:08.014Z] info    - Authentication is enabled
vscode-web    | [2025-12-20T02:56:08.014Z] info      - Using password from ~/.config/code-server/config.yaml
vscode-web    | [2025-12-20T02:56:08.014Z] info    - Not serving HTTPS 
vscode-web    | [02:56:08] 
vscode-web    | 

```
```sh
docker-compose ps
```
```text
   Name             Command              State                  Ports           
--------------------------------------------------------------------------------
vscode-web   /usr/bin/entrypoint.sh   Up (healthy)   0.0.0.0:8080-              
                                                     >8080/tcp,:::8080->8080/tcp
```

```sh
docker-compose exec vscode-web cat /home/coder/.config/code-server/config.yaml
```

```text
bind-addr: 127.0.0.1:8080
auth: password
password: c518a1a61691354a9792e0e8
cert: false
```
Navigate to `http://localhost:8080/login` in the browser and provide password.  If seeing 

![VS Code Bruno Plugin Reload Required](screenshots/reload-required.png "VS Code Bruno Plugin Reload Required")


do reload

![VS Code Bruno Local Screen](screenshots/vscode-bruno-local.png "VS Code with Bruno Operation - Local")

#### NOTE

if seeing 
```text
docker.errors.DockerException: Error while fetching server API version: Not supported URL scheme http+docker
```
upgrade/downgrade Python stack
```sh
pip uninstall docker -y
pip install docker 
```
```sh
pip list|grep requests
```
```text
requests                     2.32.4
```
```sh
pip install requests==2.31.0
```

### See Also

  * Visual Studio Code Server release announcement [blog](https://code.visualstudio.com/blogs/2022/07/07/vscode-server)
  * Visual Studio Code Server [documentation](https://code.visualstudio.com/docs/remote/vscode-server)

  * __Bruno__ git-friendly Opensource API client with collections version contro[docker image](https://hub.docker.com/r/alpine/bruno/tags)
  *  __Bruno__ [positions](https://www.usebruno.com/) it an Open source reinvented alternative to Postman with offline-first design and not a platform 
