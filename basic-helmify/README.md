### Info

replica of [arttor/helmify](https://github.com/arttor/helmify) - CLI tool that creates Helm chart from Kubernetes yaml

### Backgound

### Usage
```sh
docker pull golang:1.21-alpine
```

```sh
IMAGE=basic-builder
docker build -f Dockerfile.builder -t $IMAGE .
```
```text
[+] Building 9.6s (6/6) FINISHED
 => [internal] load build definition from Dockerfile.builder               2.4s
 => => transferring dockerfile: 165B                                       0.0s
 => [internal] load .dockerignore                                          2.4s
 => => transferring context: 2B                                            0.0s
 => [internal] load metadata for docker.io/library/golang:1.21-alpine      0.0s
 => [1/2] FROM docker.io/library/golang:1.21-alpine                        0.4s
 => [2/2] RUN apk update && apk add -U --no-cache ca-certificates git      6.1s
 => exporting to image                                                     0.4s
 => => exporting layers                                                    0.4s
 => => writing image sha256:ff90745fa9e3b3834a6e0cf6abce7c7b1a49e8f1c12d9  0.0s
 => => naming to docker.io/library/basic-builder                           0.0s
```
```sh
IMAGE=basic-build
docker build -f Dockerfile.build -t $IMAGE .
```
> NOTE: *very* time consuming:
> ```text
> [7/8] RUN go mod download                                            128.5s
> ```


```text
[+] Building 9.6s (6/6) FINISHED
 => [internal] load build definition from Dockerfile.builder               2.4s
 => => transferring dockerfile: 165B                                       0.0s
 => [internal] load .dockerignore                                          2.4s
 => => transferring context: 2B                                            0.0s
 => [internal] load metadata for docker.io/library/golang:1.21-alpine      0.0s
 => [1/2] FROM docker.io/library/golang:1.21-alpine                        0.4s
 => [2/2] RUN apk update && apk add -U --no-cache ca-certificates git      6.1s
 => exporting to image                                                     0.4s
 => => exporting layers                                                    0.4s
 => => writing image sha256:ff90745fa9e3b3834a6e0cf6abce7c7b1a49e8f1c12d9  0.0s
 => => naming to docker.io/library/basic-builder                           0.0s
sergueik@sergueik172:~/src/springboot_study/basic-helmify$ IMAGE=basic-build
sergueik@sergueik172:~/src/springboot_study/basic-helmify$ docker build -f Dockerfile.build -t $IMAGE .
[+] Building 240.0s (11/12)
[+] Building 240.1s (11/12)
[+] Building 265.3s (13/13) FINISHED
 => [internal] load .dockerignore                                          0.1s
 => => transferring context: 2B                                            0.0s
 => [internal] load build definition from Dockerfile.build                 0.1s
 => => transferring dockerfile: 495B                                       0.0s
 => [internal] load metadata for docker.io/library/basic-builder:latest    0.0s
 => [1/8] FROM docker.io/library/basic-builder                             0.3s
 => [internal] load build context                                          0.3s
 => => transferring context: 322.54kB                                      0.0s
 => [2/8] WORKDIR /build                                                   0.4s
 => [3/8] COPY go.mod .                                                    0.4s
 => [4/8] COPY go.sum .                                                    0.2s
 => [5/8] COPY pkg ./pkg/                                                  0.3s
 => [6/8] COPY cmd ./cmd/                                                  0.2s
 => [7/8] RUN go mod download                                            129.2s
 => [8/8] RUN go build -ldflags "-s -w -X main.Version=$VERSION -X main  126.0s
 => exporting to image                                                     8.1s
 => => exporting layers                                                    8.0s
 => => writing image sha256:1aeda48939b39405bf101bff2bc385f82bb2a1c1a1831  0.0s
 => => naming to docker.io/library/basic-build                             0.0s
 
```
```sh
export IMAGE=basic-helmify
docker build -f Dockerfile.run -t $IMAGE .
```
```
[+] Building 15.1s (9/9) FINISHED
 => [internal] load build definition from Dockerfile.run                   0.9s
 => => transferring dockerfile: 149B                                       0.0s
 => [internal] load .dockerignore                                          1.1s
 => => transferring context: 2B                                            0.0s
 => [internal] load metadata for docker.io/library/basic-build:latest      0.0s
 => [internal] load metadata for docker.io/library/alpine:3.12.1           2.6s
 => [stage-0 1/3] FROM docker.io/library/alpine:3.12.1@sha256:c0e9560cda1  8.1s
 => => resolve docker.io/library/alpine:3.12.1@sha256:c0e9560cda118f9ec63  1.7s
 => => sha256:c0e9560cda118f9ec63ddefb4a173a2b2a0347082d7 1.64kB / 1.64kB  0.0s
 => => sha256:d7342993700f8cd7aba8496c2d0e57be0666e80b4c44192 528B / 528B  0.0s
 => => sha256:d6e46aa2470df1d32034c6707c8041158b652f38d2a 1.51kB / 1.51kB  0.0s
 => => sha256:188c0c94c7c576fff0792aca7ec73d67a2f7f4cb3a6 2.80MB / 2.80MB  1.9s
 => => extracting sha256:188c0c94c7c576fff0792aca7ec73d67a2f7f4cb3a6e53a8  0.2s
 => FROM docker.io/library/basic-build                                     8.2s
 => [stage-0 2/3] WORKDIR /work                                            1.0s
 => [stage-0 3/3] COPY --from=basic-build /build/helmify /usr/bin          1.8s
 => exporting to image                                                     0.3s
 => => exporting layers                                                    0.2s
 => => writing image sha256:960be7c44edf2fee08bf8672786986280ecdd4baa1721  0.0s
 => => naming to docker.io/library/basic-helmify                           0.0s
```
```sh
docker image ls $IMAGE
```
```text
REPOSITORY      TAG       IMAGE ID       CREATED          SIZE
basic-helmify   latest    960be7c44edf   37 seconds ago   21.9MB
```
```sh
docker run --rm $IMAGE -help
```

```text
Helmify parses kubernetes resources from std.in and converts it to a Helm chart.

Example 1: 'kustomize build <kustomize_dir> | helmify mychart'
  - will create 'mychart' directory with Helm chart from kustomize output.

Example 2: 'cat my-app.yaml | helmify mychart'
  - will create 'mychart' directory with Helm chart from yaml file.

Example 3: 'helmify -f ./test_data/dir  mychart'
  - will scan directory ./test_data/dir for files with k8s manifests and create 'mychart' directory with Helm chart.

Example 4: 'helmify -f ./test_data/dir -r  mychart'
  - will scan directory ./test_data/dir recursively and  create 'mychart' directory with Helm chart.

Example 5: 'helmify -f ./test_data/dir -f ./test_data/sample-app.yaml -f ./test_data/dir/another_dir  mychart'
  - will scan provided multiple files and directories and  create 'mychart' directory with Helm chart.

Example 6: 'awk 'FNR==1 && NR!=1  {print "---"}{print}' /my_directory/*.yaml | helmify mychart'
  - will create 'mychart' directory with Helm chart from all yaml files in my_directory directory.

Usage:
  helmify [flags] CHART_NAME  -  CHART_NAME is optional. Default is 'chart'. Can be a directory, e.g. 'deploy/charts/mychart'.

Flags:
  -add-webhook-option
        Allows the user to add webhook option in values.yaml.
  -cert-manager-as-subchart
        Allows the user to add cert-manager as a subchart
  -cert-manager-install-crd
        Allows the user to install cert-manager CRD. Only useful with cert-manager-as-subchart. (default true)
  -cert-manager-version string
        Allows the user to specify cert-manager subchart version. Only useful with cert-manager-as-subchart. (default "v1.12.2")
  -crd-dir
        Enable crd install into 'crds' directory. (cannot be used with 'optional-crds').
        Warning: CRDs placed in 'crds' directory will not be templated by Helm.
        See https://helm.sh/docs/chart_best_practices/custom_resource_definitions/#some-caveats-and-explanations
        Example: helmify -crd-dir
  -f value
        File or directory containing k8s manifests.
  -generate-defaults
        Allows the user to add empty placeholders for typical customization options in values.yaml. Currently covers: topology constraints, node selectors, tolerances
  -h    Print help. Example: helmify -h
  -help
        Print help. Example: helmify -help
  -image-pull-secrets
        Allows the user to use existing secrets as imagePullSecrets in values.yaml.
  -optional-crds
        Enable optional CRD installation through values. (cannot be used with 'crd-dir')
  -original-name
        Use the object's original name instead of adding the chart's release name as the common prefix.
  -preserve-ns
        Use the object's original namespace instead of adding all the resources to a common namespace.
  -r    Scan dirs from -f option recursively
  -v    Enable verbose output (print WARN & INFO). Example: helmify -v
  -version
        Print helmify version. Example: helmify -version
  -vv
        Enable very verbose output. Same as verbose but with DEBUG. Example: helmify -vv
```


```sh
awk 'FNR==1 && NR!=1  {print "---"}{print}' ../basic-argocd/probe-example/*.yaml | tee data.yaml
```
> *echoes to console*
```text
---
apiVersion: v1
kind: ConfigMap
metadata:
  name: health-config
data:
  APP_READYNESS: "down"
  APP_LIVENESS: "up"
  MAX_CONN: "100"
binaryData:
  # "up" encoded in base64
  APP_DO_NOT_USE: dXA=
---
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: probe-example
  labels:
    app: probe-example
spec:
  replicas: 1
  selector:
    matchLabels:
      app: app
  template:
    metadata:
      labels:
        app: app
    spec:
      containers:
      - name: app-container
        image: docker.io/library/status-server:latest
        imagePullPolicy: IfNotPresent
        ports:
        - containerPort: 8080
        startupProbe:
          httpGet:
            path: /
            port: 8080
          failureThreshold: 30
          periodSeconds: 10
        livenessProbe:
          httpGet:
            path: /api/live
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 5
          timeoutSeconds: 3
          failureThreshold: 3
        readinessProbe:
          httpGet:
            path: /api/ready
            port: 8080
          periodSeconds: 5
        env:
        - name: READY
          valueFrom:
            configMapKeyRef:
              name: health-config
              key: APP_READYNESS
        - name: LIVE
          valueFrom:
            configMapKeyRef:
              name: health-config
              key: APP_LIVENESS

---
---
apiVersion: v1
kind: Service
metadata:
  name: probe-example
spec:
  ports:
  - port: 9090
    targetPort: 8080
  selector:
    app: probe-example

```
> NOTE: on Windows can use `/c/Users/$USERNAME` instead of `~`
```sh
docker run --rm -v ~/test:/work -w /work  -i $IMAGE  < data.yaml
```
```text
time="2026-10-29T15:00:14Z" level=info msg="Chart name is not set. Using default name 'chart"
```
```sh
find ~/test/chart/ -type f
```

```text
/c/Users/kouzm/test/chart/.helmignore
/c/Users/kouzm/test/chart/Chart.yaml
/c/Users/kouzm/test/chart/templates/deployment.yaml
/c/Users/kouzm/test/chart/templates/health-config.yaml
/c/Users/kouzm/test/chart/templates/probe-example.yaml
/c/Users/kouzm/test/chart/templates/_helpers.tpl
/c/Users/kouzm/test/chart/values.yaml
```
> NOTE: on Linux currently have to use `sudo`:
```sh
sudo find   ~/test/chart/ -type f
``` 
```text
/home/sergueik/test/chart/.helmignore
/home/sergueik/test/chart/Chart.yaml
/home/sergueik/test/chart/values.yaml
/home/sergueik/test/chart/templates/deployment.yaml
/home/sergueik/test/chart/templates/probe-example.yaml
/home/sergueik/test/chart/templates/_helpers.tpl
/home/sergueik/test/chart/templates/health-config.yaml
```
```sh
TAG=3.12.0
docker pull alpine/helm:$TAG
```

```sh
docker run --rm -it -w /tmp -v ~/test:/tmp alpine/helm:$TAG lint chart
```
```text
==> Linting chart
[INFO] Chart.yaml: icon is recommended
1 chart(s) linted, 0 chart(s) failed
```

### Consolidated

```sh
export IMAGE=basic-helmify; 
docker build -f Dockerfile -t $IMAGE .
```
```text
[+] Building 445.2s (18/18) FINISHED
 => [internal] load build definition from Dockerfile                       0.1s
 => => transferring dockerfile: 859B                                       0.0s
 => [internal] load .dockerignore                                          0.1s
 => => transferring context: 2B                                            0.0s
 => [internal] load metadata for docker.io/library/alpine:3.12.1           0.8s
 => [internal] load metadata for docker.io/library/golang:1.21-alpine      0.0s
 => CACHED [builder 1/8] FROM docker.io/library/golang:1.21-alpine         0.0s
 => [stage-1 1/4] FROM docker.io/library/alpine:3.12.1@sha256:c0e9560cda1  0.0s
 => CACHED [stage-1 2/4] WORKDIR /work                                     0.0s
 => [stage-1 3/4] RUN adduser -D -u 1000 app                               1.0s
 => [builder 2/8] RUN apk add --no-cache ca-certificates git               4.7s
 => [internal] load build context                                          0.2s
 => => transferring context: 4.80kB                                        0.0s
 => [builder 3/8] WORKDIR /build                                           0.6s
 => [builder 4/8] COPY go.mod go.sum ./                                    0.3s
 => [builder 5/8] RUN --mount=type=cache,target=/go/pkg/mod     go mod   143.5s
 => [builder 6/8] COPY pkg ./pkg/                                          0.8s
 => [builder 7/8] COPY cmd ./cmd/                                          0.3s
 => [builder 8/8] RUN --mount=type=cache,target=/root/.cache/go-build    258.7s
 => [stage-1 4/4] COPY --from=builder /build/helmify /usr/bin             33.0s
 => exporting to image                                                     0.5s
 => => exporting layers                                                    0.4s
 => => writing image sha256:40c3da99351e8859e720b72810dfef311d66b63368f1c  0.0s
 => => naming to docker.io/library/basic-helmify                           0.0s
```
### Troubleahooting 

> NOTE: not intend to fix

```sh
rm -fr ~/test/chart
```
```sh
awk 'FNR==1 && NR!=1  {print "---"}{print}' ../basic-argocd/probe-example/*.yaml | docker run -w /c/Users/$USERNAME/test:/ --rm -i $IMAGE '' my-chart
```
```text
time="2026-10-29T14:39:32Z" level=info msg="Chart name is not set. Using default name 'chart"
```
the directory `~/test` remains empty


```
docker run  --rm -v ~/test:/work -w /work --entrypoint '' -it $IMAGE  sh
/work $ pwd
/work
/work $ ls -ld .
drwxr-xr-x    2 root     root          4096 Oct 29 16:22 .

docker run  --rm -v ~/test:/work -w /work  -i $IMAGE  < data.yaml
```
```
time="2026-10-29T16:24:35Z" level=info msg="Chart name is not set. Using default name 'chart"
time="2026-10-29T16:24:35Z" level=error msg="helmify finished with error" error="mkdir chart: permission denied: unable create chart/templates dir"
```

same when

```sh
docker run  --rm -v ~/test:/work -w /work --user "$(id -u):$(id -g)"  -i $IMAGE  < data.yaml
```
```text
time="2026-10-29T16:48:40Z" level=info msg="Chart name is not set. Using default name 'chart"
time="2026-10-29T16:48:40Z" level=error msg="helmify finished with error" error="mkdir chart: permission denied: unable create chart/templates dir"
```
### See Also

  * [using helmify](https://devsecops.puziol.com.br/en/kubernetes/helm/using-helmify/)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
