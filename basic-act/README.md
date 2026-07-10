### Info 

[GitHub Actions](https://docs.github.com/en/actions) 
allow auyomatically
[Building and testing your code](https://docs.github.com/en/actions/tutorials/build-and-test-code)


[act](https://nektosact.com/)
is an open-source command-line tool developed by Nektos that allows
one run and test GitHub Actions workflows locally  not bound to committing
and pushing changes to GitHub trigger real github events 
and to verify that one's CI/CD pipeline works. 
One can use `act` to replicate much of the GitHub Actions 
execution environment directly on one's developer machine.

### Background 

When executed, `act` parses [Workflow Definitions](https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax)
and __GitHub Actions__ from `.github/workflows/`, determines which jobs and actions need to run

It resolves the job dependency graph (needs) and determines the order in which jobs should execute.

then performs  run uses the [Docker API](https://docs.docker.com/reference/api/engine/) 
of local Docker instance through socket `/var/run/docker.sock`
making it seamless  to either pull or build the necessary images, 
as defined in workflow files and finally determines the execution path based on the dependencies that were defined. 

Once it has the execution path, it then uses the Docker API to run containers for each action based on the images prepared earlier. 

`act` attempts to emulate the __GitHub Actions__ runtime by providing similar environment variables, filesystem layout, and runner behavior. 
While highly compatible, it is not guaranteed byte-for-byte replica of __GitHub__-hosted runners

### Runner Images

[List of Docker images](https://github.com/nektos/act/blob/master/IMAGES.md)  for `act`

### Usage

* download binary release from [releases](https://github.com/nektos/act/releases)

```sh
curl -skLo act_Linux_x86_64.tar.gz https://github.com/nektos/act/releases/download/v0.2.89/act_Linux_x86_64.tar.gz
```
```sh
tar xzf act_Linux_x86_64.tar.gz act
chmod +x act
```
Run Example demo project workflows (`.github/workflows/main.yml` in `nodejs` and `java`):
```
pushd nodejs
cat .github/workflows/main.yml
```
```yaml
name: CI
on: push

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - uses: actions/setup-node@v1
    - run: npm install
    - run: npm test
```
update workflow definition to
```
name: CI
on: push

jobs:
  test:
    runs-on: ubuntu-latest
    container:
      image: node:18-alpine
    steps:
    - uses: actions/checkout@v2
    - uses: actions/setup-node@v1
    - run: npm install
    - run: npm test

```
* run interactively

```sh
../act
```
with pinned image
get error:

```text
INFO[0000] Using docker host 'unix:///var/run/docker.sock', and daemon socket 'unix:///var/run/docker.sock' 
[CI/test] ⭐ Run Set up job
[CI/test] 🚀  Start image=node:18-alpine
[CI/test]   🐳  docker pull image=node:18-alpine platform= username= forcePull=true
[CI/test]   🐳  docker create image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker run image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
[CI/test]   ✅  Success - Set up job
[CI/test]   ☁  git clone 'https://github.com/actions/setup-node' # ref=v1
[CI/test] ⭐ Run Main actions/checkout@v2
[CI/test]   🐳  docker cp src=/home/sergueik/src/springboot_study/basic-act/nodejs/. dst=/home/sergueik/src/springboot_study/basic-act/nodejs
[CI/test]   ✅  Success - Main actions/checkout@v2 [14.303455ms]
[CI/test] ⭐ Run Main actions/setup-node@v1
[CI/test]   🐳  docker cp src=/home/sergueik/.cache/act/actions-setup-node@v1/ dst=/var/run/act/actions/actions-setup-node@v1/
[CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v1/dist/index.js] user= workdir=
| [command]/opt/hostedtoolcache/node/10.24.1/x64/bin/node --version
[CI/test]   ❗  ::error::There was an error when attempting to execute the process '/opt/hostedtoolcache/node/10.24.1/x64/bin/node'. This may indicate the process failed to start. Error: spawn /opt/hostedtoolcache/node/10.24.1/x64/bin/node ENOENT
[CI/test]   ❌  Failure - Main actions/setup-node@v1 [712.193257ms]
[CI/test]   ⚙  ::add-path:: /opt/hostedtoolcache/node/10.24.1/x64/bin
[CI/test] exitcode '1': failure
[CI/test] ⭐ Run Complete job
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job failed
Error: Job 'test' failed
```
fix the root cause: old library assumption
```text
spawn /opt/hostedtoolcache/node/10.24.1/x64/bin/node ENOENT
``` 
```text
INFO[0000] Using docker host 'unix:///var/run/docker.sock', and daemon socket 'unix:///var/run/docker.sock' 

? Please choose the default image you want to use with act:
  - Large size image: ca. 17GB download + 53.1GB storage, you will need 75GB of free disk space, 
    snapshots of GitHub Hosted Runners without snap and pulled docker images
  - Medium size image: ~500MB, includes only necessary tools to bootstrap actions
    and aims to be compatible with most actions
  - Micro size image: <200MB, contains only NodeJS required to bootstrap actions
    doesn't work with all actions
```
>  NOTE: Choose the Micro image

```text
[CI/test] ⭐ Run Set up job
[CI/test] 🚀  Start image=node:16-buster-slim
[CI/test]   🐳  docker pull image=node:16-buster-slim platform= username= forcePull=true
[CI/test]   🐳  docker create image=node:16-buster-slim platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker run image=node:16-buster-slim platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
[CI/test]   ✅  Success - Set up job
[CI/test]   ☁  git clone 'https://github.com/actions/setup-node' # ref=v1
[CI/test] ⭐ Run Main actions/checkout@v2
[CI/test]   🐳  docker cp src=/home/sergueik/src/springboot_study/basic-act/github-actions-demo/. dst=/home/sergueik/src/springboot_study/basic-act/github-actions-demo
[CI/test]   ✅  Success - Main actions/checkout@v2 [229.658791ms]
[CI/test] ⭐ Run Main actions/setup-node@v1
[CI/test]   🐳  docker cp src=/home/sergueik/.cache/act/actions-setup-node@v1/ dst=/var/run/act/actions/actions-setup-node@v1/
[CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v1/dist/index.js] user= workdir=
| [command]/bin/tar xz --warning=no-unknown-keyword -C /tmp/8258bb27-7f3c-4e61-9f6d-4624ba761dec -f /tmp/06cad1ab-af62-42f4-b8c1-955f8b853f05
| [command]/opt/hostedtoolcache/node/10.24.1/x64/bin/node --version
| v10.24.1
| [command]/opt/hostedtoolcache/node/10.24.1/x64/bin/npm --version
| 6.14.12
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v1/.github/tsc.json
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v1/.github/eslint-stylish.json
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v1/.github/eslint-compact.json
[CI/test]   ✅  Success - Main actions/setup-node@v1 [14.876025042s]
[CI/test]   ⚙  ::add-path:: /opt/hostedtoolcache/node/10.24.1/x64/bin
[CI/test] ⭐ Run Main npm install
[CI/test]   🐳  docker exec cmd=[bash -e /var/run/act/workflow/2] user= workdir=
| npm notice This endpoint is being retired. Use the bulk advisory endpoint instead. See the following docs for more info: https://api-docs.npmjs.com/#tag/Audit
| added 280 packages from 643 contributors and audited 280 packages in 9.367s
| 
| 24 packages are looking for funding
|   run `npm fund` for details
| 
| found 79 vulnerabilities (12 low, 26 moderate, 37 high, 4 critical)
|   run `npm audit fix` to fix them, or `npm audit` for details
[CI/test]   ✅  Success - Main npm install [10.001451828s]
[CI/test] ⭐ Run Main npm test
[CI/test]   🐳  docker exec cmd=[bash -e /var/run/act/workflow/3] user= workdir=
| 
| > github-actions-demo@1.0.0 test /home/sergueik/src/springboot_study/basic-act/github-actions-demo
| > mocha ./tests --recursive
| 
| 
| 
|   GET /
|     ✓ should respond with hello world
| 
| 
|   1 passing (22ms)
| 
[CI/test]   ✅  Success - Main npm test [506.939229ms]
[CI/test] ⭐ Run Complete job
[CI/test] Cleaning up container for job test
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job succeeded
```

>NOTE: On the first execution, `act` stores the selected default runner image in its configuration file. 
Depending on the platform and version, the file is created in either the `XDG` configuration directory `~/.config/act/actrc`
or the user's home directory `~/.actrc`. One can edit this file at any time to change the default runner image or add other default command-line options:
```text
-P ubuntu-latest=node:16-buster-slim
-P ubuntu-22.04=node:16-bullseye-slim
-P ubuntu-20.04=node:16-buster-slim
-P ubuntu-18.04=node:16-buster-slim
```
the syntax can be found in [act Usage guide](https://nektosact.com/usage/)

* examine inventory
```
docker image ls
```
```text
node:16-buster-slim             eb8b8b8a3610        179MB             0B        
```
> NOTE: The Micro runner image uses `node:16-buster-slim`, which is pulled by act if it is not already available locally.
### Cleanup
```sh
rm act_Linux_x86_64.tar.gz
docker container prune -f
docker image rm node:16-buster-slim || true
```

### See Also

   * [introduction to GitHub Actions with Docker](https://docs.docker.com/guides/gha/)
   * [curated list of awesome actions to use on GitHub](https://github.com/cplee/awesome-actions/tree/master#collection-of-actions)
   * Runners - 
     + https://hub.docker.com/search?q=act-runner
     + https://hub.docker.com/search?q=act_runner
     + https://hub.docker.com/search?q=act
The Workflows that themselves invoke __Docker++ may require [Docker-in-Docker (DinD)](https://github.com/jpetazzo/dind) or access to the host Docker socket.

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
