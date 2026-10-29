### Info

replica of [arttor/helmify](https://github.com/arttor/helmify) - CLI tool that creates Helm chart from Kubernetes yaml

### Backgound

### Usage
```
docker pull golang:1.21-alpine

```

```
IMAGE=basic-builder
docker build -f Dockerfile.builder -t $IMAGE .
```

```
IMAGE=basic-build
docker build -f Dockerfile.build -t $IMAGE .
```
> NOTE: time consuming
```text

Sending build context to Docker daemon  592.9kB
Step 1/17 : FROM basic-builder
 ---> 77c5a47c572b
Step 2/17 : ARG VERSION
 ---> Using cache
 ---> 75834797f2d7
Step 3/17 : ARG HASH
 ---> Using cache
 ---> 89a44236ae71
Step 4/17 : ARG DATE
 ---> Using cache
 ---> 5669b95005fd
Step 5/17 : ENV GO111MODULE=on
 ---> Using cache
 ---> 3168c3c92339
Step 6/17 : ENV GOPROXY=https://proxy.golang.org
 ---> Using cache
 ---> 03e7c64cba08
Step 7/17 : ENV GOOS=linux
 ---> Using cache
 ---> 06db4d5ec828
Step 8/17 : ENV GOARCH=amd64
 ---> Using cache
 ---> dc2b49ec688e
Step 9/17 : ENV CGO_ENABLED=0
 ---> Using cache
 ---> 2e79ec69303b
Step 10/17 : ENV GOOS=linux
 ---> Using cache
 ---> 7ec1059a16d7
Step 11/17 : WORKDIR /build
 ---> Using cache
 ---> 308c6efecb89
Step 12/17 : COPY go.mod .
 ---> Using cache
 ---> 2e7b4ed505c8
Step 13/17 : COPY go.sum .
 ---> Using cache
 ---> fa11f2b36136
Step 14/17 : COPY pkg ./pkg/
 ---> c71d3daf5c5e
Step 15/17 : COPY cmd ./cmd/
 ---> faa592bf2371
Step 16/17 : RUN go mod download
 ---> Running in bdd3eb87b140
Removing intermediate container bdd3eb87b140
 ---> 09bf842c0f91
Step 17/17 : RUN go build -ldflags "-s -w -X main.Version=$VERSION -X main.CommitHash=$HASH -X 'main.CompileDate=$DATE'" -o helmify ./cmd/helmify
 ---> Running in 9eb391d18018
Removing intermediate container 9eb391d18018
 ---> b01bf6662088
Successfully built b01bf6662088
Successfully tagged basic-build:latest
```
```sh
export IMAGE=basic-helmify
docker build -f Dockerfile.run -t $IMAGE .
```
```
Sending build context to Docker daemon  621.1kB
Step 1/5 : FROM alpine:3.12.1
 ---> d6e46aa2470d
Step 2/5 : WORKDIR /
 ---> Using cache
 ---> 07082a32f5c3
Step 3/5 : ENV PATH=/
 ---> Using cache
 ---> 79a6e1599709
Step 4/5 : COPY --from=builder /build/helmify /
 ---> 20316bdcffc2
Step 5/5 : ENTRYPOINT ["./helmify"]
 ---> Running in 6451d8a50827
Removing intermediate container 6451d8a50827
 ---> dfaa0fbe204b
Successfully built dfaa0fbe204b
Successfully tagged basic-build:latest
```
```
docker image ls $IMAGE
```
```
REPOSITORY          TAG                 IMAGE ID            CREATED              SIZE
basic-helmify       latest              dfaa0fbe204b        About a minute ago
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
```sh
docker run --rm -v /c/Users/$USERNAME/test:/work -w /work  -i $IMAGE  < data.yaml
```
```text
time="2026-10-29T15:00:14Z" level=info msg="Chart name is not set. Using default name 'chart"
```
```sh
find   ~/test/chart/ -type f
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

```
TAG=3.12.0
docker pull alpine/helm:$TAG
```

```sh
docker run --rm -it -w /tmp -v ~/test:/tmp alpine/helm:$TAG lint  chart
```
```text
==> Linting chart
[INFO] Chart.yaml: icon is recommended
1 chart(s) linted, 0 chart(s) failed
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

### See Also

  * [using helmify](https://devsecops.puziol.com.br/en/kubernetes/helm/using-helmify/)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
