### Info

it is _not_ impossible to rewrite [GitHub Actions](https://docs.github.com/en/actions) completely on [Argo CD](https://argo-cd.readthedocs.io/en/stable/) and other way around.

but notably __GitHub Actions__ and __Argo CD__ are not two interchangeable implementations of the _same thing_

__GitHub Actions__
> Event-driven workflows and CI
>   - Compile, build, and test code.
>   - Run unit tests, Selenium tests, linters, and security scans.
>   - Package artifacts and publish container images.
>   - Trigger deployments and automate repository operations.



__Argo CD__:
> Kubernetes GitOps and reconciliation
>   - Observe desired Kubernetes configuration stored in Git.
>   - Compare the desired state with the live cluster.
>   - Apply changes and report synchronization and health status.
>   - Detect configuration drift and, when configured, correct it.


|Characteristic|__Puppet__|__Argo CD__|
|--------------|----------|-----------|
|Desired state|Declarative configuration and resource manifests|Declarative Kubernetes manifests, Helm charts, Kustomize, etc.|
|Actual state|Managed machine configuration|Live Kubernetes resources|
|Reconciliation|Agent or server periodically enforces configuration, depending on setup|Controller continuously compares desired and live application state|
|Drift correction|Corrects configuration drift|Can correct application drift<br/>through self-healing|
|Primary domain|Operating-system and<br/>infrastructure configuration|Kubernetes application delivery|
|Source of truth|Puppet code and data|Git repository, typically|



The key similarity is declarative desired-state management backed by reconciliation. 
__Puppet__ was already doing that long before __Argo CD__ appeared.
There is also a meaningful difference: Puppet traditionally manages resources on machines — __packages__, __files__, __services__, __users__ and __configuration__. 
__Argo CD__ narrows this to delivering applications into __Kubernetes__ and 
tracking their health and synchronization state.
So I would characterize __Argo CD__ as an __GitOps__-oriented descendant of the same architectural idea, 
rather than literally a replacement for __Puppet__.
And this connects directly to your earlier __GitHub Actions__ question:
- __GitHub Actions__ says: execute this workflow when an event occurs.
- __Puppet__ says: make the managed machine conform to this configuration.
- __Argo CD__ says: make the __Kubernetes__ application conform to the desired state in __Git__.

The last two share a fundamental model. The first has a different execution model.
The historical irony: __GitOps__ didn't invent declarative reconciliation. 
It applied that established configuration-management idea to a __Kubernetes__-centric, __Git__-driven application-delivery workflow. 
__Argo CD__ feels like __Puppet__ reborn because, at the level of control-loop architecture, there really is a family resemblance.



__Argo CD__ narrows __Puppet__ resource-management domain to __Kubernetes__ application delivery,
but it does not merely replace __Puppet__ operation dictionary with __kubectl__ and __Helm__ commands.
  

The distinction is between reducing the managed resource model and reducing the execution vocabulary.

|Dimension|Puppet|Argo CD|
|---------|------|-------|
|Resource domain|OS configuration, packages, files, services, users, and more|Kubernetes resources and applications|
|Desired-state description|Puppet DSL and resource types/providers|Kubernetes manifests, Helm, Kustomize, and supported config-management plugins|
|Execution model|Resource providers and agent/server enforcement|Kubernetes controller and reconciliation|
|Source of truth|Puppet code and data, among other configurations|Git, typically|
|Extension mechanism|Custom resource types and providers|Config-management plugins and integrations, alongside Kubernetes extensibility|



The subtlety is that __Helm__ is not simply another set of imperative operations. __Argo CD__ can render a __Helm__ chart into Kubernetes manifests and then reconcile the resulting resources. 
Likewise, __Kustomize__ and plain __YAML__ are supported; __Argo CD__ is not limited to __Helm__

__Argo CD__ is conceptually a simplified and specialized declarative configuration-management controller:
it takes the broad resource-management problem and concentrates on reconciling __Kubernetes__
application state from __Git__.


__Argo CD__ understands __Kubernetes__ __Jobs__ and lifecycle hooks, so you can make it execute certain procedural tasks. That does not turn it into a general-purpose workflow engine; hooks run within the lifecycle of a synchronization operation.


Argo CD is a massively simplified, domain-specific configuration-management system because Kubernetes gives it a standardized subject model. Puppet must deal with a much broader and more heterogeneous world of managed resources; Argo CD can rely on Kubernetes APIs, resource schemas, controllers, and a well-defined reconciliation boundary.
The interesting engineering challenge is therefore the reverse: emulate Argo CD's behavior using GitHub Actions, while keeping the reconciliation logic outside the Kubernetes cluster being managed.
That last constraint changes the design substantially. You are not merely replacing one deployment tool with another. You are rebuilding a control loop using an event-driven workflow engine that does not inherently provide that control loop.

|Responsibility|GitHub Actions implementation|Complexity|Importance|
|--------------|-----------------------------|----------|----------|
|Discover desired state|Checkout repository; render manifests with Helm or Kustomize|⭐⭐💀|⭐|
|Observe live state|Query remote Kubernetes API|⭐⭐💀|⭐⭐|
|Detect differences|Compare desired vs. live state, accounting for defaults and server-side mutations|⭐⭐⭐⭐⭐💀💀|⭐⭐⭐⭐⭐|
|Reconcile|Apply desired manifests using kubectl apply or server-side apply|⭐⭐⭐💀|⭐⭐⭐⭐|
|Verify convergence|Inspect resource status, rollout conditions and health; report failures or retry|⭐⭐⭐⭐💀|⭐⭐⭐|

#### Components

__API Server__: Handles the Web UI, CLI commands, and manages Role-Based Access Control (RBAC).
__Repository Server__: Communicates with Git repositories, maintains a local cache, and renders manifests from tools like Helm, Kustomize, or Jsonnet.
__Application Controller__: Acts as the reconciliation engine, continuously comparing live cluster states against the target Git state and detecting configuration drift (OutOfSync).
__Redis Cache__: state cache

### Usage

Initial setup based on [gist](https://gist.github.com/bhimsur/b6c575916883ff7712861beacbe1ff0b)

The install on Windows MiniKube (relatively old version - update suggested):
```cmd
minikube.exe start --driver=virtualbox
```
```cmd
minikube.exe status 
```

```
minikube
type: Control Plane
host: Running
kubelet: Running
apiserver: Running
kubeconfig: Configured
```

```
minikube.exe kubectl create namespace argocd 
```
> NOTE in first (e.g. if recycled prior ) will show downloads
```text
    > kubectl.exe.sha256: 64 B / 64 B [----------------------] 100.00% ? p/s 0s
    > kubectl.exe: 39.52 MiB / 39.52 MiB [----------] 100.00% 4.91 MiB p/s 8.2s
namespace/argocd created
```
download __CRD__ to examine and /or customize:

```sh
curl -skLO https://raw.githubusercontent.com/argoproj/argo-cd/v2.8.4/manifests/install.yaml
```
feed to kubectl (it will deal with direcrory
```cmd
minikube.exe kubectl -- apply --namespace argocd -f install.yaml
```
```text
customresourcedefinition.apiextensions.k8s.io/applications.argoproj.io created
customresourcedefinition.apiextensions.k8s.io/applicationsets.argoproj.io created
customresourcedefinition.apiextensions.k8s.io/appprojects.argoproj.io created
serviceaccount/argocd-application-controller created
serviceaccount/argocd-applicationset-controller created
serviceaccount/argocd-dex-server created
serviceaccount/argocd-notifications-controller created
serviceaccount/argocd-redis created
serviceaccount/argocd-repo-server created
serviceaccount/argocd-server created
role.rbac.authorization.k8s.io/argocd-application-controller created
role.rbac.authorization.k8s.io/argocd-applicationset-controller created
role.rbac.authorization.k8s.io/argocd-dex-server created
role.rbac.authorization.k8s.io/argocd-notifications-controller created
role.rbac.authorization.k8s.io/argocd-server created
clusterrole.rbac.authorization.k8s.io/argocd-application-controller created
clusterrole.rbac.authorization.k8s.io/argocd-server created
rolebinding.rbac.authorization.k8s.io/argocd-application-controller created
rolebinding.rbac.authorization.k8s.io/argocd-applicationset-controller created
rolebinding.rbac.authorization.k8s.io/argocd-dex-server created
rolebinding.rbac.authorization.k8s.io/argocd-notifications-controller created
rolebinding.rbac.authorization.k8s.io/argocd-server created
clusterrolebinding.rbac.authorization.k8s.io/argocd-application-controller created
clusterrolebinding.rbac.authorization.k8s.io/argocd-server created
configmap/argocd-cm created
configmap/argocd-cmd-params-cm created
configmap/argocd-gpg-keys-cm created
configmap/argocd-notifications-cm created
configmap/argocd-rbac-cm created
configmap/argocd-ssh-known-hosts-cm created
configmap/argocd-tls-certs-cm created
secret/argocd-notifications-secret created
secret/argocd-secret created
service/argocd-applicationset-controller created
service/argocd-dex-server created
service/argocd-metrics created
service/argocd-notifications-controller-metrics created
service/argocd-redis created
service/argocd-repo-server created
service/argocd-server created
service/argocd-server-metrics created
deployment.apps/argocd-applicationset-controller created
deployment.apps/argocd-dex-server created
deployment.apps/argocd-notifications-controller created
deployment.apps/argocd-redis created
deployment.apps/argocd-repo-server created
deployment.apps/argocd-server created
statefulset.apps/argocd-application-controller created
networkpolicy.networking.k8s.io/argocd-application-controller-network-policy created
networkpolicy.networking.k8s.io/argocd-applicationset-controller-network-policy created
networkpolicy.networking.k8s.io/argocd-dex-server-network-policy created
networkpolicy.networking.k8s.io/argocd-notifications-controller-network-policy created
networkpolicy.networking.k8s.io/argocd-redis-network-policy created
networkpolicy.networking.k8s.io/argocd-repo-server-network-policy created
networkpolicy.networking.k8s.io/argocd-server-network-policy created
```

Verify the installation by getting all the objects in the ArgoCD namespace.
```cmd
minikube.exe kubectl -- get all -n argocd
```
(wait till all pods in that namespace running)

```text
NAME                                                    READY   STATUS    RESTARTS   AGE
pod/argocd-application-controller-0                     1/1     Running   0          7m3s
pod/argocd-applicationset-controller-5cf484b59d-7cxc5   1/1     Running   0          7m4s
pod/argocd-dex-server-857455bcbd-7kf7f                  1/1     Running   0          7m4s
pod/argocd-notifications-controller-66bfff8745-965bb    1/1     Running   0          7m4s
pod/argocd-redis-8698676ff6-6htqt                       1/1     Running   0          7m4s
pod/argocd-repo-server-7c8d87478b-msk8d                 1/1     Running   0          7m4s
pod/argocd-server-6bf47d79b4-lxg72                      1/1     Running   0          7m3s

NAME                                              TYPE        CLUSTER-IP       EXTERNAL-IP   PORT(S)                      AGE
service/argocd-applicationset-controller          ClusterIP   10.104.43.83     <none>        7000/TCP,8080/TCP            7m5s
service/argocd-dex-server                         ClusterIP   10.110.168.213   <none>        5556/TCP,5557/TCP,5558/TCP   7m5s
service/argocd-metrics                            ClusterIP   10.96.43.118     <none>        8082/TCP                     7m5s
service/argocd-notifications-controller-metrics   ClusterIP   10.102.211.96    <none>        9001/TCP                     7m5s
service/argocd-redis                              ClusterIP   10.101.13.81     <none>        6379/TCP                     7m5s
service/argocd-repo-server                        ClusterIP   10.110.108.169   <none>        8081/TCP,8084/TCP            7m5s
service/argocd-server                             ClusterIP   10.97.251.21     <none>        80/TCP,443/TCP               7m5s
service/argocd-server-metrics                     ClusterIP   10.103.229.52    <none>        8083/TCP                     7m5s

NAME                                               READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/argocd-applicationset-controller   1/1     1            1           7m5s
deployment.apps/argocd-dex-server                  1/1     1            1           7m4s
deployment.apps/argocd-notifications-controller    1/1     1            1           7m4s
deployment.apps/argocd-redis                       1/1     1            1           7m4s
deployment.apps/argocd-repo-server                 1/1     1            1           7m4s
deployment.apps/argocd-server                      1/1     1            1           7m3s

NAME                                                          DESIRED   CURRENT   READY   AGE
replicaset.apps/argocd-applicationset-controller-5cf484b59d   1         1         1       7m5s
replicaset.apps/argocd-dex-server-857455bcbd                  1         1         1       7m4s
replicaset.apps/argocd-notifications-controller-66bfff8745    1         1         1       7m4s
replicaset.apps/argocd-redis-8698676ff6                       1         1         1       7m4s
replicaset.apps/argocd-repo-server-7c8d87478b                 1         1         1       7m4s
replicaset.apps/argocd-server-6bf47d79b4                      1         1         1       7m3s

NAME                                             READY   AGE
statefulset.apps/argocd-application-controller   1/1     7m3s
```
```cmd
minikube.exe kubectl -- port-forward svc/argocd-server --namespace argocd 8080:443
```

```text
Forwarding from 127.0.0.1:8080 -> 8080
Forwarding from [::1]:8080 -> 8080
```
let remain running

```cmd
VBoxManage.exe list runningvms
```
```cmd
"minikube" {e8e61b40-c769-4c5c-ae19-51e35b9c8bde}
```
```cmd
VBoxManage.exe guestproperty get "e8e61b40-c769-4c5c-ae19-51e35b9c8bde" "/VirtualBox/GuestInfo/Net/0/V4/IP"
```
```text
Value: 10.0.2.15
```
```
SET VM="{e8e61b40-c769-4c5c-ae19-51e35b9c8bde}"
VBoxManage.exe modifyvm "%VM%" --nat-pf1 "web,tcp/udp,,8080,10.0.2.15,8080"
```

```text
VBoxManage.exe: error: The machine 'minikube' is already locked for a session (or being unlocked)
VBoxManage.exe: error: Details: code VBOX_E_INVALID_OBJECT_STATE (0x80bb0007), component MachineWrap, interface IMachine, callee IUnknown
VBoxManage.exe: error: Context: "LockMachine(a->session, LockType_Write)" at line 529 of file VBoxManageModifyVM.cpp
```
for testing sake do the same iinteractively through Virtualbox Manager:

![Port Forwarding](screenshots/port-forwarding.png)

acknowledge `net::ERR_CERT_AUTHORITY_INVALID`

![Port Forwarding](screenshots/untrusted-cert.png)


* retrieve
```cmd
minikube.exe kubectl -- --namespace argocd get secret argocd-initial-admin-secret -o jsonpath="{.data.password}"
```
```text
eEQ1cDRJbzJiOU9FMTBTcw==
```
```powershell
$data = 'eEQ1cDRJbzJiOU9FMTBTcw=='
$bytes = [System.Convert]::FromBase64String($data)
[System.Text.Encoding]::UTF8.GetString($bytes)
```
```text
xD5p4Io2b9OE10Ss
```
![Login](screenshots/login-argocd.png)

if run from bash
 
```sh
minikube.exe kubectl -- -n argocd get secret argocd-initial-admin-secret -o jsonpath="{.data.password}" | base64 -d; echo
```
login as `admin`, will be redirected to initial dashboard:

![Login](screenshots/dashbpard-argocd.png)

connect to demo repository `https://github.com/OdabasiMehmet/nginx-kubernetes.git` and 

![Appliction Created](screenshots/application-argocd.png)

create application  as explained.


![Appliction Details](screenshots/application-details-argocd.png)


```cmd
minikube.exe kubectl -- scale deployment -n test nginx-deployment --replicas=2
```
```text
deployment.apps/nginx-deployment scaled
```
![Appliction Details](screenshots/application-scaled-argocd.png)

### See Also

   * [docker hub](https://hub.docker.com/r/argoproj/argocd)
   * collection of [Example Apps to Demonstrate Argo CD](https://github.com/argoproj/argocd-example-apps) and related [video](https://www.youtube.com/watch?v=0WAm0y2vLIo)
   * [simple nginx application to demonstarte working with argocd](https://github.com/OdabasiMehmet/nginx-kubernetes)
   * [ArgoCD In One Shot](https://github.com/LondheShubham153/argocd-in-one-shot)
   * [Demos of arogcd one shot video project sources](https://github.com/LondheShubham153/argocd-demos)
   * [installing ArgoCD on Minikube and deploying a test application](https://medium.com/@mehmetodabashi/installing-argocd-on-minikube-and-deploying-a-test-application-caa68ec55fbf)
   * [Setting Up ArgoCD on Kubernetes CLuster](https://dev.to/durrello/setting-up-argocd-on-minikube-for-a-local-dev-environment-5637)
   * [hands-on introduction to Argo CD : Kubernetes DevOps CI/CD](https://www.youtube.com/watch?v=2WSJF7d8dUg)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
