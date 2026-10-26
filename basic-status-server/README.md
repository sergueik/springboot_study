### Info

Bare minimal controllable HTTP application for Minikube/Argo CD learning exercise whose liveness and readiness probe responses are set through container environment variables


### Usage

```sh
mvn clean package
```

```cmd
java -cp target\example.status-server.jar example.App 
```
```cmd
java -jar target\example.status-server.jar -live up -ready down
```

```sh
curl http://localhost:8080/api/alive | jq '.'
```
```json
{
  "status": "error",
  "error": "Not Found"
}
```

```sh
curl -s http://localhost:8080/api/live | jq '.'
```
```json
{
  "status": "UP"
}
```
```sh
curl -s http://localhost:8080/api/ready | jq '.'
```
```json
{
  "status": "DOWN"
}
```
```sh
docker pull alpine/java:11-jre
```

```sh
NAME=status-server
docker build -f Dockerfile -t $NAME .
```

```text
Sending build context to Docker daemon  46.08kB
Step 1/5 : FROM alpine/java:11-jre
 ---> 468a214d75b6
Step 2/5 : ARG app_jar="example.status-server.jar"
 ---> Using cache
 ---> 43b242648e06
Step 3/5 : ADD "target/${app_jar}" app.jar
 ---> Using cache
 ---> ca815850e151
Step 4/5 : EXPOSE 8080
 ---> Using cache
 ---> 3583cff9b5e2
Step 5/5 : ENTRYPOINT java -cp app.jar example.App -ready ${READY:up}  -live ${LIVE:up}
 ---> Running in 58eeab62e6a6
Removing intermediate container 58eeab62e6a6
 ---> c2fa2f5dd60d
Successfully built c2fa2f5dd60d
Successfully tagged status-server:latest
```
```sh
docker run --name $NAME -d -e LIVE=up -e READY=down -p 8081:8080 $NAME
```

```sh
docker logs $NAME
```
```text
Server started on port 8080
```
```sh
curl -I http://$(docker-machine ip):8081/api/live
```
```text
HTTP/1.1 200 OK
Date: Sat, 24 Oct 2026 13:42:48 GMT
```
```sh
curl -I http://$(docker-machine ip):8081/api/ready
```
```
HTTP/1.1 503 Service Unavailable
Date: Sat, 24 Oct 2026 13:38:54 GMT
```
```sh
docker stop $NAME
docker container prune -f
```

### Note
Replace base image with [bellsoft](https://hub.docker.com/r/bellsoft/liberica-openjre-alpine). This yields a small nice present

```sh
docker pull bellsoft/liberica-openjre-alpine:8
```
```text
8: Pulling from bellsoft/liberica-openjre-alpine
55afa1ecc21d: Pull complete
44af7b7a294c: Pull complete
29a9756cd656: Pull complete
Digest: sha256:a537e3794dde32a8899f0db1a0e045db4e1634a351bf35d3a0c9d8fed7faa4f2
Status: Downloaded newer image for bellsoft/liberica-openjre-alpine:8
docker.io/bellsoft/liberica-openjre-alpine:8
```
```sh
NAME=status-server
docker build -f Dockerfile -t $NAME .
```

```sh
docker run --name $NAME -d -e LIVE=up -e READY=down -p 8081:8080 $NAME
```
```text
34e4ae4f6d17b0856219b3c8e050aaa7dc93ef02d06b0b6772524e1bf2400523
```
```sh
curl -I http://$(docker-machine ip):8081/api/ready
```
```text
curl: (7) Failed to connect to 192.168.99.106 port 8081 after 2004 ms: Could not connect to server
```
```sh
docker container ls
```
```text
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES
```
docker logs 34e4
```
Error occurred during initialization of VM
Cannot create VM thread. Out of system resources.
```
this is not caused by the code:
```sh
docker run --rm bellsoft/liberica-openjre-alpine:8 java -version
```
```text
Error occurred during initialization of VM
Cannot create VM thread. Out of system resources.
```
### TODO

Suppress
```text
WARNING: sendResponseHeaders: being invoked with a content length for a HEAD request
```


### Next Steps

To [load](https://minikube.sigs.k8s.io/docs/commands/image) the image into [Minikube](https://minikube.sigs.k8s.io/docs/) one first
[save](https://docs.docker.com/reference/cli/docker/image/save/)
a portable file `.tar` archive

```sh
docker save --output ${NAME}.tar $NAME 
```
> NOTE: __Docker Toolbox__ will store the archive on Windows side 
```cmd
set NAME=status-server
dir %NAME%.tar
```

```text
10/24/2026  03:22 PM       156,687,360 status-server.tar
```

```sh
docker-machine stop
```
```cmd
minikube.exe start --driver=virtualbox
```
```cmd
minikube.exe image load -v 4 status-server.tar
```
no output
this does not work:
```cmd
minikube.exe image ls
```

prints nothing

```cmd
minikube.exe status
```
```text
minikube
type: Control Plane
host: Paused
kubelet: Paused
apiserver: Paused
kubeconfig: Paused
```

Stop Docker Toolbox, start Minikube again


```
minikube.exe image load status-server.tar --v=3
```
no output

```cmd
minikube.exe image list --v=2
```
```text
quay.io/argoproj/argocd:v2.8.4
k8s.gcr.io/pause:3.2
k8s.gcr.io/kube-scheduler:v1.20.2
k8s.gcr.io/kube-proxy:v1.20.2
k8s.gcr.io/kube-controller-manager:v1.20.2
k8s.gcr.io/kube-apiserver:v1.20.2
k8s.gcr.io/etcd:3.4.13-0
k8s.gcr.io/coredns:1.7.0
ghcr.io/dexidp/dex:v2.37.0
gcr.io/k8s-minikube/storage-provisioner:v5
gcr.io/google-samples/gb-frontend:v5
docker.io/library/status-server:latest
docker.io/library/redis:7.0.11-alpine
docker.io/library/nginx:latest
docker.io/kubernetesui/metrics-scraper:v1.0.4
docker.io/kubernetesui/dashboard:v2.1.0
```

The entry  we will be using is known as `docker.io/library/status-server:latest`

```cmd
minikube.exe  kubectl -- --namespace example apply -f example.yaml
```
```text
deployment.apps/probe-example created
configmap/health-config unchanged
service/probe-example unchanged
```

```cmd
minikube.exe kubectl -- --namespace example get pod
```
```text
NAME                             READY   STATUS         RESTARTS   AGE
probe-example-64684f9c8d-m4vwv   0/1     ErrImagePull   0          13s
```

```cmd
minikube.exe kubectl -- --namespace example get pod
```
```text
NAME                             READY   STATUS             RESTARTS   AGE
probe-example-64684f9c8d-m4vwv   0/1     ImagePullBackOff   0          75s
```
```yaml
```

```cmd
minikube.exe kubectl -- --namespace example describe pod probe-example-64684f9c8d-m4vwv
```
```text
Name:         probe-example-64684f9c8d-m4vwv
Namespace:    example
Priority:     0
Node:         minikube/192.168.99.104
Start Time:   Sat, 24 Oct 2026 16:24:38 -0400
Labels:       app=app
              pod-template-hash=64684f9c8d
Annotations:  <none>
Status:       Pending
IP:           172.17.0.11
IPs:
  IP:           172.17.0.11
Controlled By:  ReplicaSet/probe-example-64684f9c8d
Containers:
  app-container:
    Container ID:
    Image:          docker.io/library/status-server:latest
    Image ID:
    Port:           8080/TCP
    Host Port:      0/TCP
    State:          Waiting
      Reason:       ImagePullBackOff
    Ready:          False
    Restart Count:  0
    Liveness:       http-get http://:8080/app/live delay=10s timeout=3s period=5s #success=1 #failure=3
    Readiness:      http-get http://:8080/app/ready delay=0s timeout=1s period=5s #success=1 #failure=3
    Startup:        http-get http://:8080/ delay=0s timeout=1s period=10s #success=1 #failure=30
    Environment:
      APP_READYNESS:  <set to the key 'APP_READYNESS' of config map 'health-config'>  Optional: false
      APP_LIVENESS:   <set to the key 'APP_LIVENESS' of config map 'health-config'>   Optional: false
    Mounts:
      /var/run/secrets/kubernetes.io/serviceaccount from default-token-lj4dv (ro)
Conditions:
  Type              Status
  Initialized       True
  Ready             False
  ContainersReady   False
  PodScheduled      True
Volumes:
  default-token-lj4dv:
    Type:        Secret (a volume populated by a Secret)
    SecretName:  default-token-lj4dv
    Optional:    false
QoS Class:       BestEffort
Node-Selectors:  <none>
Tolerations:     node.kubernetes.io/not-ready:NoExecute op=Exists for 300s
                 node.kubernetes.io/unreachable:NoExecute op=Exists for 300s
Events:
  Type     Reason     Age                   From               Message
  ----     ------     ----                  ----               -------
  Normal   Scheduled  3m40s                 default-scheduler  Successfully assigned example/probe-example-64684f9c8d-m4vwv to minikube
  Normal   Pulling    2m9s (x4 over 3m37s)  kubelet            Pulling image "docker.io/library/status-server:latest"
  Warning  Failed     2m8s (x4 over 3m36s)  kubelet            Failed to pull image "docker.io/library/status-server:latest": rpc error: code = Unknown desc = Error response from daemon: pull access denied for status-server, repository does not exist or may require 'docker login': denied: requested access to the resource is denied
  Warning  Failed     2m8s (x4 over 3m36s)  kubelet            Error: ErrImagePull
  Normal   BackOff    115s (x6 over 3m35s)  kubelet            Back-off pulling image "docker.io/library/status-server:latest"
  Warning  Failed     100s (x7 over 3m35s)  kubelet            Error: ImagePullBackOff

```

The error is
```text
Error response from daemon: pull access denied for status-server,
repository does not exist or may require 'docker login'
```

add 
```yaml
image: docker.io/library/status-server:latest
imagePullPolicy: IfNotPresent
```

```
minikube.exe kubectl -- --namespace example get pods -w
```
> NOTE don't have to keep `w` 
```text
NAME                             READY   STATUS                       RESTARTS   AGE
probe-example-64684f9c8d-m4vwv   0/1     ImagePullBackOff             0          8m19s
probe-example-fd944f89d-g2r74    0/1     CreateContainerConfigError   0          14s

```

```cmd
minikube.exe ssh -- docker images status-server
```
```text
REPOSITORY      TAG       IMAGE ID       CREATED       SIZE
status-server   latest    c2fa2f5dd60d   7 hours ago   155MB
```

```
minikube.exe kubectl -- --namespace example describe pod probe-example-7d444dc7f8-tnn9v
```
```text
Name:         probe-example-7d444dc7f8-tnn9v
Namespace:    example
Priority:     0
Node:         minikube/192.168.99.104
Start Time:   Sat, 24 Oct 2026 16:54:24 -0400
Labels:       app=app
              pod-template-hash=7d444dc7f8
Annotations:  <none>
Status:       Pending
IP:           172.17.0.11
IPs:
  IP:           172.17.0.11
Controlled By:  ReplicaSet/probe-example-7d444dc7f8
Containers:
  app-container:
    Container ID:
    Image:          docker.io/library/status-server:latest
    Image ID:
    Port:           8080/TCP
    Host Port:      0/TCP
    State:          Waiting
      Reason:       CreateContainerConfigError
    Ready:          False
    Restart Count:  0
    Liveness:       http-get http://:8080/app/live delay=10s timeout=3s period=5s #success=1 #failure=3
    Readiness:      http-get http://:8080/app/ready delay=0s timeout=1s period=5s #success=1 #failure=3
    Startup:        http-get http://:8080/ delay=0s timeout=1s period=10s #success=1 #failure=30
    Environment:
      READY:  <set to the key 'APP_READYNESS' of config map 'health-config'>  Optional: false
      LIVE:   <set to the key 'APP_LIVENESS' of config map 'health-config'>   Optional: false
    Mounts:
      /var/run/secrets/kubernetes.io/serviceaccount from default-token-lj4dv (ro)
Conditions:
  Type              Status
  Initialized       True
  Ready             False
  ContainersReady   False
  PodScheduled      True
Volumes:
  default-token-lj4dv:
    Type:        Secret (a volume populated by a Secret)
    SecretName:  default-token-lj4dv
    Optional:    false
QoS Class:       BestEffort
Node-Selectors:  <none>
Tolerations:     node.kubernetes.io/not-ready:NoExecute op=Exists for 300s
                 node.kubernetes.io/unreachable:NoExecute op=Exists for 300s
Events:
  Type     Reason     Age                   From               Message
  ----     ------     ----                  ----               -------
  Normal   Scheduled  2m20s                 default-scheduler  Successfully assigned example/probe-example-7d444dc7f8-tnn9v to minikube
  Normal   Pulled     18s (x12 over 2m19s)  kubelet            Container image "docker.io/library/status-server:latest" already present on machine
  Warning  Failed     4s (x13 over 2m19s)   kubelet            Error: couldn't find key APP_LIVENESS in ConfigMap example/health-config

```
update config to not use `binaryData` for env.
```cmd
minikube.exe kubectl -- --namespace example get pods
```
```text
NAME                             READY   STATUS    RESTARTS   AGE
probe-example-7d444dc7f8-tnn9v   1/1     Running   0          5m26s
```

change config:
```yaml
---
apiVersion: v1
kind: ConfigMap
metadata:
  name: health-config
data:
  APP_READYNESS: "down" 
  APP_LIVENESS: "up"
```

```cmd
minikube.exe kubectl -- --namespace example delete deployment probe-example
```
```text
deployment.apps "probe-example" deleted
```
```cmd
minikube.exe kubectl -- --namespace example apply -f  example.yaml
```
```text
deployment.apps/probe-example created
configmap/health-config unchanged
service/probe-example unchanged
```
```cmd
minikube.exe kubectl -- --namespace example get pod
```
```text
NAME                             READY   STATUS    RESTARTS   AGE
probe-example-5497848cf4-6jllb   0/1     Running   0          10s
```
restore configuration and redeploy. NOTE: will have to recreate deployment

```cmd
minikube.exe kubectl -- --namespace example delete deployment probe-example
minikube.exe kubectl -- --namespace example apply -f  example.yaml
```
observe
```cmd
minikube.exe kubectl -- --namespace example get pod
```
```text
NAME                             READY   STATUS        RESTARTS   AGE
probe-example-5497848cf4-4gvbg   1/1     Running       0          12s
probe-example-5497848cf4-6jllb   0/1     Terminating   0          33m
```

However, deletion and recreation are not strictly necessary just to change the application configuration.
For a normal configuration update, you can edit the ConfigMap and [restart](https://kubernetes.io/docs/reference/kubectl/generated/kubectl_rollout/kubectl_rollout_restart/) the Deployment:
```cmd
minikube.exe kubectl -- --namespace example apply -f example.yaml
minikube.exe kubectl -- --namespace example rollout restart deployment/probe-example
minikube.exe kubectl -- --namespace example rollout status deployment/probe-example
```

_restart_ is important when configuration is injected through environment variables because existing processes do not automatically receive updated environment values.
In __restore the configuration and redeploy__ example Kubernetes deletes and recreates the Deployment. 
Kubernetes creates a replacement Pod, while the previous Pod may remain in Terminating state briefly as cleanup completes


#### Kind

The other popular framework for running local Kubernetes clusters using Docker container primarily for testing, __Kubernetes in Docker__ ([kind](https://kind.sigs.k8s.io/)) 
also supports loading a locally built __Docker__ image directly into its __Kubernetes__ nodes.
One does not need to publish work in progress image e.g. `status-server:latest` image to __Docker Hub__ or another registry

The [equivalent](https://kind.sigs.k8s.io/docs/user/quick-start/#loading-an-image-into-your-cluster) of your Minikube workflow is very similar:
```sh
docker save --output status-server.tar status-server
```
```
kind load image-archive status-server.tar --name my-cluster
``
### Trubleshooting Notepad Graphical Subsystem Error

```cmd
ver
```
```text
Microsoft Windows [Version 10.0.26200.9457]
```

```text
Faulting application name: Notepad.exe, version: 11.2607.14.0, time stamp: 0x6a7ce775
Faulting module name: dwmcorei.dll, version: 10.0.27200.1054, time stamp: 0x8a84d4fe
Exception code: 0x8898008d
Fault offset: 0x000000000006fbb8
Faulting process id: 0x5684
Faulting application start time: 0x1DD63F3974F1882
Faulting application path: C:\Program Files\WindowsApps\Microsoft.WindowsNotepad_11.2607.14.0_x64__8wekyb3d8bbwe\Notepad\Notepad.exe
Faulting module path: C:\Program Files\WindowsApps\Microsoft.WindowsAppRuntime.2_2.5.1.0_x64__8wekyb3d8bbwe\dwmcorei.dll
Report Id: 00b14aa3-f35a-4846-b5b6-13e76d246807
Faulting package full name: Microsoft.WindowsNotepad_11.2607.14.0_x64__8wekyb3d8bbwe
Faulting package-relative application ID: App
```