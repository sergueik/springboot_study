### Usage

  * build cluster

```sh
docker compose up -d
```
Wait a few minutes for GitLab to fully initialize (you can tail logs using docker logs -f gitlab).

  * access GitLab `http://localhost:8929`

```sh
Username: root

Password: rootpassword
```
or as specified in  `docker-compose.yaml`

optionally change the credentials later through GitLab UI.
 
   * Register GitLab Runner manually:




```sh
docker exec -it gitlab-runner gitlab-runner register

```
You’ll be prompted:
|                      |              |
|Prompt	               | Answer       |
|----------------------|--------------|
|GitLab instance URL   |	http://gitlab:8929|
|Token	| Found in GitLab UI __Admin__ &gt; __Runners__|
|Description	  | `java-runner`|
|Tags	| java|
|Executor |	`docker`|
|Default Docker image |	`my/java-runner:latest`|


* in the project build file  `.gitlab-ci.yml` reference the runner image
```yaml
---
build-java:
  image: my/java-runner:latest
  script:
    - java -version
    - mvn clean package

```

