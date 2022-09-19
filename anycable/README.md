### Info

Found that some Docker images on dockerhub notbly the [anycable-go](https://hub.docker.com/layers/anycable/anycable-go/1.2/images/sha256-74e4819072d9b7e873a457d25f6a8615c08b9dbf2fb3f012527d959e3049c383?context=explore) are pruned from root account

### Usage
#### With Alpine Container

* pull Alpine based image using relevant tag

```sh
docker pull anycable/anycable-go:1-alpine
```
* run
```sh
docker run --rm --entrypoint ash -it anycable/anycable-go:1-alpine
```
* in container
```sh
cat /etc/passwd
```
```text
nobody:x:65534:65534:nobody:/home:/bin/false
```
* no `root` user in the container

* run as user `id` `0`:

```sh
docker run --user 0 --name test --entrypoint ash -it anycable/anycable-go:1-alpine
```
* in the container
```sh
/ # whoami
```
```text
whoami: unknown uid 0
```
* cleanup
```sh
docker container rm test
docker image rm anycable/anycable-go:1-alpine
```

#### With "Latest" Container
* pull 
```sh
docker pull anycable/anycable-go
```
* Note: the `inspect` command does not show much

```sh
docker inspect anycable/anycable-go
```
```json
[
    {
        "Id": "sha256:db3fcdf18a2e80113bdecb35f0f3c0394a019d8201026446d29367314c68eb28",
        "RepoTags": [
            "anycable/anycable-go:latest"
        ],
        "RepoDigests": [
            "anycable/anycable-go@sha256:74e4819072d9b7e873a457d25f6a8615c08b9dbf2fb3f012527d959e3049c383"
        ],
        "Parent": "",
        "Comment": "buildkit.dockerfile.v0",
        "Created": "2022-08-10T23:31:29.055302929Z",
        "Container": "",
        "ContainerConfig": {
            "Hostname": "",
            "Domainname": "",
            "User": "",
            "AttachStdin": false,
            "AttachStdout": false,
            "AttachStderr": false,
            "Tty": false,
            "OpenStdin": false,
            "StdinOnce": false,
            "Env": null,
            "Cmd": null,
            "Image": "",
            "Volumes": null,
            "WorkingDir": "",
            "Entrypoint": null,
            "OnBuild": null,
            "Labels": null
        },
        "DockerVersion": "",
        "Author": "",
        "Config": {
            "Hostname": "",
            "Domainname": "",
            "User": "nobody",
            "AttachStdin": false,
            "AttachStdout": false,
            "AttachStderr": false,
            "ExposedPorts": {
                "8080/tcp": {}
            },
            "Tty": false,
            "OpenStdin": false,
            "StdinOnce": false,
            "Env": [
                "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
            ],
            "Cmd": null,
            "Image": "",
            "Volumes": null,
            "WorkingDir": "/",
            "Entrypoint": [
                "/usr/local/bin/anycable-go"
            ],
            "OnBuild": null,
            "Labels": null
        },
        "Architecture": "amd64",
        "Os": "linux",
        "Size": 11295658,
        "VirtualSize": 11295658,
        "GraphDriver": {
            "Data": {
                "LowerDir": "/var/lib/docker/overlay2/c9d0daa174cd365569cbbc578aeb731550b8e3cbac9c41ecebbf96a125319237/diff:/var/lib/docker/overlay2/ced14a6b877b6d1a5f8826ef8bbcf39fb1879ed7cb2cb969c3f86da2ad7b38ee/diff",
                "MergedDir": "/var/lib/docker/overlay2/189294db9793f57bd3b7461ecd27ed283c2d1070f1e33ed0054023385f00add8/merged",
                "UpperDir": "/var/lib/docker/overlay2/189294db9793f57bd3b7461ecd27ed283c2d1070f1e33ed0054023385f00add8/diff",
                "WorkDir": "/var/lib/docker/overlay2/189294db9793f57bd3b7461ecd27ed283c2d1070f1e33ed0054023385f00add8/work"
            },
            "Name": "overlay2"
        },
        "RootFS": {
            "Type": "layers",
            "Layers": [
                "sha256:d3cc3df94bd0beeac857b58a16ce177b0dfe3dfb028744c8a5b3677590884943",
                "sha256:4cc76d2d22d06a6bf84e6fade30f61c2385c94bb0e741725e4d355a6705f3f51",
                "sha256:8b312693a7ccc1dd532586f147c148ff76a963e1ab342e86d4a07f318af6fef1"
            ]
        },
        "Metadata": {
            "LastTagTime": "0001-01-01T00:00:00Z"
        }
    }
]

```
* through trial and error found that neither `bash` nor `sh` nor `ash` is accepted as entrypoint.
the error looks like:

```sh
docker run --user 0  --entrypoint 'sh' -it anycable/anycable-go:latest
```
```text
docker: Error response from daemon: OCI runtime create failed: runc create failed: unable to start container process: exec: "sh": executable file not found in $PATH: unknown.
```
```text
ERRO[0001] error waiting for container: context canceled
```
the image is based on `scratch` [image](https://hub.docker.com/_/scratch). As a result, shell access is not possible. It appears that without creating a new container based on `anycable` there is no way to explore it

### See Also

  * remove root user from image at Docker image build time [discussion](https://stackoverflow.com/questions/53411914/docker-remove-root-user-from-image-at-build-time)
  * Docker rootless environment [documentation](https://docs.docker.com/engine/security/rootless/)
  * Dockerlabs converting images to run without root [step by step](https://birthday.play-with-docker.com/run-as-user/) - notably explains everything but does not recommend removl of the root from `/etc/passwd`
  * the original forum [discussion](https://qna.habr.com/q/1201372?e=13071466) (in Russian)
  * [override entrypoint](https://phoenixnap.com/kb/docker-run-override-entrypoint)
  * [exploring a Docker container filesystem from the host](https://www.baeldung.com/ops/docker-container-filesystem)
  * [managing data in Docker](https://docs.docker.com/storage/)


  

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
