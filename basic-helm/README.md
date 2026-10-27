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

Neither format requires that server-side application understands the underlying programming language of the payload served.
A static file server can serve both repository layouts, provided the necessary metadata and URLs are correct.
NOTE OCI chart repositories use a different protocol from its traditional HTTP chart repositories.
One can choose the conventional `index.yaml` layout for your first exercise, your existing Java server is a natural fit

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
the chart archive contains the standard __Helm__ DSL entries — `Chart.yaml`, `values.yaml`, and `templates/`.
__Argo CD__ fetches `index.yaml`, resolves the chart version, downloads the archive, and renders its templates using __Helm__ processing tool
embedded in `argocd-repo-server`.

one should easily use the helm application itself
to generate or update the index.yaml file  it will accept
```sh
helm repo index  $(pwd)/application/ --url http://localhost:8081/application
```
The ./application/ must contain the packaged chart archive, such as `helm-guestbook-0.1.0.tgz` (see e.g. [demo project](https://github.com/argoproj/argocd-example-apps/tree/master/helm-guestbook)).
The source directory alone is not enough.

one can even delegate:
```sh
helm package ./helm-guestbook --destination ./application/
```

one then can attempt to follow the [Chart Repository Guide](https://helm.sh/docs/topics/chart_repository/)
```sh
helm repo add helm-guestbook http://localhost:8081/application
helm repo update
helm install helm-guestbook helm-guestbook/helm-guestbook
```


|Token|Lexical role|Meaning|
|-----|------------|-------|
|my-guestbook|Release name|The name assigned to this particular installation in the cluster. You choose it.|
|First helm-guestbook|Repository alias|The local name you assigned with helm repo add. It maps to a repository URL.|
|Second helm-guestbook|Chart name|The chart's actual name, declared in its Chart.yaml metadata.|

To make __kind__ or __Minikube__ provide the fake hostname through Kubernetes DNS — the names needs to be registered as Kubernetes Services.
Neither kind nor Minikube automatically resolves an arbitrary hostname such as helm-repository.

For example, one can package basic HTTP server as a pod in the same namespace (`argocd`) and has a Service named `helm-repository`, Argo CD can use:

#### Usage

#### Download to Development Machine

```cmd
TAG=v3.12.0
```
> NOTE: the search does not work on `https://github.com/helm/helm/releases?q=v3.12.0&expanded=true` - have to browse through release history manually. There is no binary in the Assets any longer, only source and [PGP signatures](https://en.wikipedia.org/wiki/Pretty_Good_Privacy) `.asc`

> The recent [helmify](https://github.com/arttor/helmify) only supports __Helm__ versions newer than __v3.6.0__.

```cmd
curl -skLO https://github.com/helm/helm/releases/download/v3.12.0/helm-v3.12.0-windows-amd64.zip.asc
file helm-v3.12.0-windows-amd64.zip.asc
```
```text
helm-v3.12.0-windows-amd64.zip.asc: PGP signature Signature (old)
```
Using locally hosted container image with Helm Server/Runner binaier from [Docker Hub](https://hub.docker.com/r/alpine/helm)
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
curl -s -o /dev/null -w "%{http_code}\n" https://raw.githubusercontent.com/argoproj/argo-cd/$VERSION/manifests/install.yaml
```
```text
200
```
and in __kind__ `kind-argocd-cluster` cluster,

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
```sh
TAG=3.12.0
docker pull alpine/helm:$TAG
```
```sh
docker run --rm -it alpine/helm:$TAG help
```
```text
Usage:
  helm [command]

Available Commands:
...
```

Download [helmify](https://github.com/arttor/helmify/releases).
> NOTE may choose an older release matching the chosen __Helm__ age:

```sh
curl -skLO https://github.com/arttor/helmify/releases/download/v0.4.11/helmify_Windows_x86_64.zip
unzip helmify_Windows_x86_64.zip -d /c/tools/
```

```sh
cat ../basic-argocd/example.yaml | helmify my-chart
```
this will create
```text
my-chart
my-chart/.helmignore
my-chart/Chart.yaml
my-chart/templates
my-chart/templates/deployment.yaml
my-chart/templates/health-config.yaml
my-chart/templates/probe-example.yaml
my-chart/templates/_helpers.tpl
my-chart/values.yaml
```
> NOTE: do not assume the `helmify/helmify` is useful:
>
> ```sh
> docker pull helmify/helmify:0.6.0
> ```
> but [this](https://github.com/helmify/helmify) might not the __helmify__ one are looking for:
> ```
> docker run  --rm -it helmify/helmify:0.6.0 help
> ```
> ```text
> Setting Active Processor Count to 1
> Calculating JVM memory based on 635316K available memory
> For more information on this calculation, see https://paketo.io/docs/reference/java-reference/#memory-calculator
> Calculated JVM Memory Configuration: -XX:MaxDirectMemorySize=10M -Xmx20168K -XX:MaxMetaspaceSize=103147K -XX:ReservedCodeCacheSize=240M -Xss1M (Total Memory: 635316K, Thread Count: 250, Loaded Class Count: 15797, Headroom: 0%)
> Enabling Java Native Memory Tracking
> Adding 137 container CA certificates to JVM truststore
> Spring Cloud Bindings Enabled
> Picked up JAVA_TOOL_OPTIONS: -Djava.security.properties=/layers/paketo-buildpacks_bellsoft-liberica/java-security-properties/java-security.properties -XX:+ExitOnOutOfMemoryError -XX:ActiveProcessorCount=1 -XX:MaxDirectMemorySize=10M -Xmx20168K -XX:MaxMetaspaceSize=103147K -XX:ReservedCodeCacheSize=240M -Xss1M -XX:+UnlockDiagnosticVMOptions -XX:NativeMemoryTracking=summary -XX:+PrintNMTStatistics -Dorg.springframework.cloud.bindings.boot.enable=true
> [0.026s][warning][os,thread] Failed to start thread "VM Periodic Task Thread" - pthread_create failed (EPERM) for attributes: stacksize: 1024k, guardsize: 4k, detached.
> [0.034s][warning][os,thread] Failed to start thread "VM Thread" - pthread_create failed (EPERM) for attributes: stacksize: 1024k, guardsize: 4k, detached.
> Error occurred during initialization of VM
> Cannot create VM thread. Out of system resources.
> ```
How to make the in house helm backend accessible from  minikube / kind hosted argocd server ?


`arttor/helmify` does not have an official, pre-built Docker image published by the maintainer on Docker Hub. It is primarily distributed as a Go CLI tool installed from GitHub Releases. One certainly can create a cached install fake `Dockerfile`:

```docker
FROM alpine:latest
ARG HELMIFY_VERSION=0.4.20
RUN apk add --no-cache curl tar
RUN curl -sL https://github.com{HELMIFY_VERSION}/helmify_Linux_x86_64.tar.gz | tar -xz -C /usr/local/bin
ENTRYPOINT ["helmify"]
```

### Promise

__Helm__ overhead is especially visible in tiny projects, where the application configuration is simpler than the machinery required to parameterize it:

```sh
find my-chart/ -type f | xargs cat |wc -l
```
```text
203
```
```sh
wc -l ../basic-argocd/example.yaml
```
```text
79 ../basic-argocd/example.yam
```
and that is before accounting for the additional concepts a learner must understand: __values__, __template expressions__, __helper definitions__, __release__-specific naming, __rendering__, __chart metadata__ and __overrides__

For tiny Kubernetes applications, Helm can impose a disproportionate complexity tax

An straightforward *"all in one"* declarative manifest becomes a collection of templates, values and helper functions, making the effective specification harder to read and reason about


without hands-on experience with small, deliberately simple applications, the engineer may never develop a working mental model of what the chart actually does.

That creates precisely the operational pattern you're describing:
Deploy the vendor's Helm chart → something does not work → roll back → redeploy and hope.

This can be an entirely reasonable emergency response. It is not, by itself, diagnosis or engineering.

A tiny, deliberately unoptimized project describing a trivial application is a controlled experiment. 
You know what its Deployment, Service, ConfigMap, probes and labels are supposed to do. 
When exmining the generated, one can compare the source manifests with the rendered output and determine exactly what changed.

A vendor can have compelling reasons to distribute its product as a Helm chart: the product may contain many interdependent services, and Helm provides a packaging mechanism for configuring and installing them. A customer may have little practical choice but to use the vendor-supported deployment method

The release engineer then inherits two separate problems:
- __Application complexity__: the vendor product actual architecture, dependencies, resource requirements and supported configuration
- __Deployment machinery complexity__: Helm templates, values, Kubernetes resource relationships, upgrades and rollback behavior

Without the ability to distinguish these layers, diagnosing a failure becomes much harder. A broken Service selector, an invalid value override, a missing ConfigMap key, a failed readiness probe and an application-level defect can all present as a deployment that does not work—but they require different remedies

In a regulated enterprise, the application release engineer cannot assume that the vendor will always deliver a timely fix for every component embedded in its deployment package. Security obligations, audit findings and internal upgrade requirements can force the customer to modify a vendor-supported Helm deployment. Without practical knowledge of how the chart maps to Kubernetes resources and how its dependencies are managed, the engineer is reduced to trial-and-error deployment and rollback. Small, deliberately simple projects provide a controlled way to acquire the skills needed to diagnose and safely maintain those complex deployments.


The Vendor Helm Chart Ownership Problem

Consider an anecdotal dialogue between a bank (B) and its software vendor (A):

B: Patch component C!

A: ???

B: I can see C right there in your Helm chart!

A: Honestly, that Helm chart is not entirely my chart.

The exchange is deliberately simplified, but the underlying problem is real. A vendor's Helm chart may package or reference components maintained by other vendors, open-source projects, or separate internal teams. The fact that a component appears in the vendor's deployment package does not necessarily mean the vendor owns its source code, can patch it independently, or can guarantee compatibility with a newer version.

Meanwhile, the customer may be a security-audited institution with a deadline to remediate a vulnerability or upgrade a deeply embedded dependency. Waiting indefinitely for the vendor is not necessarily an acceptable option, and replacing the entire application may not be practical.

Argo CD supports Helm; that does not mean Argo CD inherently requires Helm. A vendor's deployment instructions can create a practical dependency between them even when no such architectural dependency is necessary.

There are three different relationships here:
- Technical dependency: Argo CD can deploy ordinary Kubernetes manifests without Helm.
- Vendor-imposed prerequisite: Vendor A documents a Helm chart as the supported installation method for its product.
- Operational dependency: Bank B adopts that documented installation path and consequently has to understand and maintain the Helm-based deployment.
The second relationship creates the third. It can make Helm appear to be an intrinsic requirement of Argo CD, when it is actually a requirement of the chosen application's delivery model.


Argo CD supports Helm; that does not mean Argo CD inherently requires Helm. A vendor's deployment instructions can create a practical dependency between them even when no such architectural dependency is necessary.

Vista/NVIDIA analogy (NVIDIA leads to , but Vista is blamed for experiencing BSOD, Causing the BSOD: NVIDIA. Taking the blame: Vista) makes the related diagnostic point: the visible failure is not necessarily caused by the component receiving the blame. A blue screen observed while using NVIDIA drivers does not, by itself, establish whether the fault lies in the driver, Windows, hardware, or an interaction between them.
ut that reveals both subjects too early.

Triggered by NVIDIA. Blamed for its occurrence: Vista.

What is it?

Spoiler: BSOD.
works as a little riddle
the punchline depends on delaying the identification of the blamed party until the end. The reader initially interprets the sentence as a technical statement, then discovers that the wrong subject has been assigned responsibility
### Troubleshooting

```sh
git pull 
```
```text
```
resolution: WIFI likely redirects first attempts to reach internet to accept page

![Caption Free WIFI Accept](screenshots/free_wifi_accept.png) 

### The Cost of Heroic Engineering

An experienced, exhausted architect—an engineer whose capabilities were recognized by the developers—eventually stepped in, rescued the situation, and saved the day.

But there is a difference between being capable of solving a Titanic-scale problem and being expected to solve one every day.

> It is not right to work at Titanic scale every day. It is too risky. One does like to be a hero occasionally—not every day.

Heroic intervention can be necessary. It should not become the normal operating model. When success repeatedly depends on one exhausted expert stepping in to resolve problems that others cannot diagnose, the organization has accumulated operational risk rather than eliminated it.

The objective of good release engineering is not to produce more heroes. It is to make failures understandable, interventions repeatable, and recovery possible without requiring a rescue.

a successful rescue proves that someone could solve the problem; it does not prove that the system was designed to be operated safely

the rescue itself distorts the victim’s estimate of the problem.

The observer sees the final execution time: one hour. But that hour is conditional on having A available, with the experience to recognize the problem, discard irrelevant paths, and select a workable solution.

The counterfactual is invisible:
- With A: one hour.
- With A guiding inexperienced people: potentially many hours or days, with repeated dead ends.
- Without A: perhaps an indefinite failure, because nobody recognizes the decisive step.

A successful rescue can teach an organization precisely the wrong lesson: that the problem was easy, rather than that the rescuer was exceptional.

### See Also

  * [automate Helm Chart Creation with Helmify](https://www.linkedin.com/pulse/automate-helm-chart-creation-helmify-manoj-dhobale/) (NOTE: Linkedin link - may be unavailanble when unauthorize
  
  


-----
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
