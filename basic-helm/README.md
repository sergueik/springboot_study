### Info

The goal is not *necessarily* to eliminate __every__ external dependency. It is to establish a __small, reproducible, internally hosted toolchain__ where the important artifacts are under your control, and __Argo CD__ provides the __GitOps__ user experience on top.

to exercise __Argo CD__ __Helm__ chart-source integration, with the chart packages and their __Helm__ __DSL__ (`Chart.yaml`, `values.yaml`, `templates`, and `dependencies`) hosted in-house.

There are two separate things to host:
- __Helm__ charts — the material __Argo CD__ fetches and renders.
- The __Helm__ executable — the binary that `argocd-repo-server` runs to render the chart.


#### Recommended implementation

Use a local __Helm__ repository or __Open Container Initiative__ ([OCI](https://en.wikipedia.org/wiki/Open_Container_Initiative)) compliant container registry for subject charts. 
For a first exercise, a conventional __HTTP__ __Helm repository__ is arguably the easiest to inspect; 
technically __OCI__ is also supported natively. 

|Component|Location|
|---------|--------|
|Learning source and Application YAML|Regular remote Git|
|Chart DSL and packaged chart versions|Local Helm repository / OCI registry|
|Helm executable|Inside argocd-repo-server, pinned or customized|
|Probe application image|Local container registry|
|Running workloads|kind or Minikube|

Argo CD native Helm integration uses Helm to run helm template; Argo CD itself manages the resulting Kubernetes resources. It is not a separate Helm server that maintains Helm releases

Practical step is to stand up the chart repository outside the cluster, package a tiny chart that deploys existing probe application, and point an __Argo CD__ Application at that repository. That directly tests the chart-fetching and rendering path one wants to learn.


__Local chart directory in Git__:  server-less. __Argo CD__ fetches regular Git repository and renders the chart at its specified path. This is the fewest moving parts, but does not exercise fetching a chart from a separate __Helm__ repository.

__Local HTTP Helm repository__: dedicated lightweight web server which serves `index.yaml` and packaged `.tgz` charts that __Argo CD__ connects to from inside the cluster. This exercises the separate __Helm__ chart-source integration.

The minimal Helm directory is
```code
repo/
├── index.yaml
└── probe-app-0.1.0.tgz
```
the chart archive contains the tandard Helm DSL entries — `Chart.yaml`, `values.yaml`, and `templates/`. 
__Argo CD__ fetches `index.yaml`, resolves the chart version, downloads the archive, and renders its templates using __Helm__ processing tool
embedded in `argocd-repo-server`.

#### Usage
Using locally hosted container image with Helm Server/Runner binaier from Docker Hub 
> NOTE do not pick `latest`: Argo CD version __3.5__, the underlying tool has *transitioned* its default rendering engine to Helm __v4__. With range of   Argo CDs __2.0__ up to __3.4__, Helm __v3__ is the secured standard default built-in binary (we pin the __v2.8.4__).
On the other hand Helm __v2__ is completely unsupported.

Find the exact version of __Helm__ bundled inside your running Argo CD instance one  cannot through native argocd CLI command because __Helm__ is executed *internally* in the `argocd-repo-server` component
```cmd
minikube.exe kubectl -- exec --namespace argocd deploy/argocd-repo-server -c argocd-repo-server -- helm version
```
```text
version.BuildInfo{Version:"v3.12.1", GitCommit:"f32a527a060157990e2aa86bf45010dfb3cc8b8d", GitTreeState:"clean", GoVersion:"go1.20.4"}
```
One can pin the argocd version deployed on kind just as always:
```sh
VERSION=v2.8.4
```
```sh
curl -s -o /dev/null -w "%{http_code}\n"  https://raw.githubusercontent.com/argoproj/argo-cd/$VERSION/manifests/install.yaml
```
```text
200
```
and in kind `kind-argocd-cluster` cluster,

```sh
kubectl apply --namespace argocd --server-side --force-conflicts -f https://raw.githubusercontent.com/argoproj/argo-cd/$VERSION/manifests/install.yaml
```
> NOTE: the version search does not work
```sh
curl -skI https://github.com/argoproj/argo-cd/releases?q=$VERSION
```
```sh
curl -sSL -o /tmp/argocd https://github.com/argoproj/argo-cd/releases/download/$VERSION/argocd-linux-amd64
sudo install -m 555 /tmp/argocd /usr/local/bin/argocd
```
```poweshell
$VERSION='v2.8.4'
invoke-webrequest -usebasicparsing -outfile 'argocd.exe' -uri "https://github.com/argoproj/argo-cd/releases/download/${VERSION}/argocd-windows-amd64.exe"
```
now
```cmd
TAG=v3.12.0
docker pull alpine/helm:$TAG
```

How to make the in house helm backend accessible from  minikube / kind hosted argocd server ?
