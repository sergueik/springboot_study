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

The very first run is a little different: `act` auto switches to interactive mode: 

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
because `actions/setup-node@v1` was written with the assumption that it is running on a GitHub-hosted runner. 

after updating versions of actions the act succeds:
```
[CI/test] ⭐ Run Set up job
[CI/test] 🚀  Start image=node:18-alpine
[CI/test]   🐳  docker pull image=node:18-alpine platform= username= forcePull=true
[CI/test]   🐳  docker create image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker run image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
[CI/test]   ✅  Success - Set up job
[CI/test]   ☁  git clone 'https://github.com/actions/setup-node' # ref=v4
[CI/test] ⭐ Run Main actions/checkout@v4
[CI/test]   🐳  docker cp src=/home/sergueik/src/springboot_study/basic-act/nodejs/. dst=/home/sergueik/src/springboot_study/basic-act/nodejs
[CI/test]   ✅  Success - Main actions/checkout@v4 [12.898925ms]
[CI/test] ⭐ Run Main actions/setup-node@v4
[CI/test]   🐳  docker cp src=/home/sergueik/.cache/act/actions-setup-node@v4/ dst=/var/run/act/actions/actions-setup-node@v4/
[CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v4/dist/setup/index.js] user= workdir=
| Attempting to download 18...
| Acquiring 18.20.8 - x64 from https://github.com/actions/node-versions/releases/download/18.20.8-14110393767/node-18.20.8-linux-x64.tar.gz
| Extracting ...
| [command]/bin/tar xz --strip 1 -C /tmp/a05d052f-b644-473e-975d-9c89e9136218 -f /tmp/ff70a279-0e6b-4a34-9f62-24d39b4aeae8
| Adding to the cache ...
[CI/test]   ❓  ::group::Environment details
| node: 
| npm: 10.8.2
| yarn: 1.22.22
[CI/test]   ❓  ::endgroup::
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/tsc.json
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/eslint-stylish.json
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/eslint-compact.json
[CI/test]   ✅  Success - Main actions/setup-node@v4 [24.899641543s]
[CI/test]   ⚙  ::set-output:: node-version=
[CI/test]   ⚙  ::add-path:: /opt/hostedtoolcache/node/18.20.8/x64/bin
[CI/test] ⭐ Run Main npm install
[CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/2.sh] user= workdir=
| npm warn deprecated inflight@1.0.6: This module is not supported, and leaks memory. Do not use it. Check out lru-cache if you want a good and tested way to coalesce async requests by a key value, which is much more comprehensive and powerful.
| npm warn deprecated rimraf@2.6.3: Rimraf versions prior to v4 are no longer supported
| npm warn deprecated glob@7.1.2: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
| npm warn deprecated glob@7.2.3: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
| npm warn deprecated superagent@8.1.2: Please upgrade to superagent v10.2.2+, see release notes at https://github.com/forwardemail/superagent/releases/tag/v10.2.2 - maintenance is supported by Forward Email @ https://forwardemail.net
| npm warn deprecated mkdirp@0.5.1: Legacy versions of mkdirp are no longer supported. Please update to mkdirp 1.x. (Note that the API surface has changed to use Promises in 1.x.)
| npm warn deprecated eslint@6.8.0: This version is no longer supported. Please see https://eslint.org/version-support for other options.
| 
| added 375 packages, and audited 376 packages in 22s
| 
| 104 packages are looking for funding
|   run `npm fund` for details
| 
| 12 vulnerabilities (3 low, 6 high, 3 critical)
| 
| To address all issues (including breaking changes), run:
|   npm audit fix --force
| 
| Run `npm audit` for details.
[CI/test]   ✅  Success - Main npm install [22.489104074s]
[CI/test] ⭐ Run Main npm test
[CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/3.sh] user= workdir=
| 
| > github-actions-demo@1.0.0 test
| > mocha ./tests --recursive
| 
| 
| 
|   GET /
|     ✓ should respond with hello world
| 
| 
|   1 passing (46ms)
| 
[CI/test]   ✅  Success - Main npm test [956.340369ms]
[CI/test] ⭐ Run Post actions/setup-node@v4
[CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v4/dist/cache-save/index.js] user= workdir=
[CI/test]   ✅  Success - Post actions/setup-node@v4 [387.633206ms]
[CI/test] ⭐ Run Complete job
[CI/test] Cleaning up container for job test
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job succeeded
```
Note that the actoon is  approach
and update

to finalize the commit for 'Provision on demand' I
```
docker run  --rm -it node:18-alpine  cat /etc/alpine-release
```
```text
3.21.3
```
 and updated workflow with the eatlier  commens in README.md
 but check first - one can not be overly specific:
```text
[CI/test]   ❌  Failure - Set up job
[CI/test] 🏁  Job failed
Error: Error response from daemon: Head "https://registry-1.docker.io/v2/library/alpine/manifests/3.21.3": Get "https://auth.docker.io/token?scope=repository%3Alibrary%2Falpine%3Apull&service=registry.docker.io": net/http: request canceled while waiting for connection (Client.Timeout exceeded while awaiting headers)
```
debugging CI often feels like peeling an onion

```text
[CI/test] ⭐ Run Set up job
[CI/test] 🚀  Start image=alpine:3.21
[CI/test]   🐳  docker pull image=alpine:3.21 platform= username= forcePull=true
[CI/test]   🐳  docker create image=alpine:3.21 platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker run image=alpine:3.21 platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
[CI/test]   ✅  Success - Set up job
[CI/test]   ☁  git clone 'https://github.com/actions/setup-node' # ref=v4
[CI/test] ⭐ Run Main actions/checkout@v4
[CI/test]   🐳  docker cp src=/home/sergueik/src/springboot_study/basic-act/nodejs/. dst=/home/sergueik/src/springboot_study/basic-act/nodejs
[CI/test]   ✅  Success - Main actions/checkout@v4 [12.78728ms]
[CI/test] ⭐ Run Main actions/setup-node@v4
[CI/test]   🐳  docker cp src=/home/sergueik/.cache/act/actions-setup-node@v4/ dst=/var/run/act/actions/actions-setup-node@v4/
[CI/test]   🐳  docker exec cmd=[node /var/run/act/actions/actions-setup-node@v4/dist/setup/index.js] user= workdir=
| OCI runtime exec failed: exec failed: unable to start container process: exec: "node": executable file not found in $PATH
[CI/test]   ❌  Failure - Main actions/setup-node@v4 [489.510089ms]
[CI/test] exitcode '127': command not found, please refer to https://github.com/nektos/act/issues/107 for more information
[CI/test] ⭐ Run Complete job
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job failed
Error: Job 'test' failed

```
indicates there is a 


```text
there is a bootstrap dependency:

Need Node
    ↓
to run setup-node
    ↓
whose job is to install Node
```
this workflow commit demonstrates a semi-circular dependency: `setup-node` requires a __Node__ runtime in order to *set up __Node__*.
```text
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


try to update to a later pinned `node` image (node:18-alpine) without modifying anything else 
and get `act` error:

```text
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
because `actions/setup-node@v1` was written with the assumption that it is running on a GitHub-hosted runner. 

after updating versions of actions the act succeds:
```
[CI/test] ⭐ Run Set up job
[CI/test] 🚀  Start image=node:18-alpine
[CI/test]   🐳  docker pull image=node:18-alpine platform= username= forcePull=true
[CI/test]   🐳  docker create image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker run image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
[CI/test]   ✅  Success - Set up job
[CI/test]   ☁  git clone 'https://github.com/actions/setup-node' # ref=v4
[CI/test] ⭐ Run Main actions/checkout@v4
[CI/test]   🐳  docker cp src=/home/sergueik/src/springboot_study/basic-act/nodejs/. dst=/home/sergueik/src/springboot_study/basic-act/nodejs
[CI/test]   ✅  Success - Main actions/checkout@v4 [12.898925ms]
[CI/test] ⭐ Run Main actions/setup-node@v4
[CI/test]   🐳  docker cp src=/home/sergueik/.cache/act/actions-setup-node@v4/ dst=/var/run/act/actions/actions-setup-node@v4/
[CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v4/dist/setup/index.js] user= workdir=
| Attempting to download 18...
| Acquiring 18.20.8 - x64 from https://github.com/actions/node-versions/releases/download/18.20.8-14110393767/node-18.20.8-linux-x64.tar.gz
| Extracting ...
| [command]/bin/tar xz --strip 1 -C /tmp/a05d052f-b644-473e-975d-9c89e9136218 -f /tmp/ff70a279-0e6b-4a34-9f62-24d39b4aeae8
| Adding to the cache ...
[CI/test]   ❓  ::group::Environment details
| node: 
| npm: 10.8.2
| yarn: 1.22.22
[CI/test]   ❓  ::endgroup::
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/tsc.json
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/eslint-stylish.json
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/eslint-compact.json
[CI/test]   ✅  Success - Main actions/setup-node@v4 [24.899641543s]
[CI/test]   ⚙  ::set-output:: node-version=
[CI/test]   ⚙  ::add-path:: /opt/hostedtoolcache/node/18.20.8/x64/bin
[CI/test] ⭐ Run Main npm install
[CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/2.sh] user= workdir=
| npm warn deprecated inflight@1.0.6: This module is not supported, and leaks memory. Do not use it. Check out lru-cache if you want a good and tested way to coalesce async requests by a key value, which is much more comprehensive and powerful.
| npm warn deprecated rimraf@2.6.3: Rimraf versions prior to v4 are no longer supported
| npm warn deprecated glob@7.1.2: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
| npm warn deprecated glob@7.2.3: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
| npm warn deprecated superagent@8.1.2: Please upgrade to superagent v10.2.2+, see release notes at https://github.com/forwardemail/superagent/releases/tag/v10.2.2 - maintenance is supported by Forward Email @ https://forwardemail.net
| npm warn deprecated mkdirp@0.5.1: Legacy versions of mkdirp are no longer supported. Please update to mkdirp 1.x. (Note that the API surface has changed to use Promises in 1.x.)
| npm warn deprecated eslint@6.8.0: This version is no longer supported. Please see https://eslint.org/version-support for other options.
| 
| added 375 packages, and audited 376 packages in 22s
| 
| 104 packages are looking for funding
|   run `npm fund` for details
| 
| 12 vulnerabilities (3 low, 6 high, 3 critical)
| 
| To address all issues (including breaking changes), run:
|   npm audit fix --force
| 
| Run `npm audit` for details.
[CI/test]   ✅  Success - Main npm install [22.489104074s]
[CI/test] ⭐ Run Main npm test
[CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/3.sh] user= workdir=
| 
| > github-actions-demo@1.0.0 test
| > mocha ./tests --recursive
| 
| 
| 
|   GET /
|     ✓ should respond with hello world
| 
| 
|   1 passing (46ms)
| 
[CI/test]   ✅  Success - Main npm test [956.340369ms]
[CI/test] ⭐ Run Post actions/setup-node@v4
[CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v4/dist/cache-save/index.js] user= workdir=
[CI/test]   ✅  Success - Post actions/setup-node@v4 [387.633206ms]
[CI/test] ⭐ Run Complete job
[CI/test] Cleaning up container for job test
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job succeeded
```
Note that the actoon is  approach
and update

to finalize the commit for 'Provision on demand' I
```
docker run  --rm -it node:18-alpine  cat /etc/alpine-release
```
```text
3.21.3
```
 and updated workflow with the eatlier  commens in README.md
 but check first - one can not be overly specific:
```text
[CI/test]   ❌  Failure - Set up job
[CI/test] 🏁  Job failed
Error: Error response from daemon: Head "https://registry-1.docker.io/v2/library/alpine/manifests/3.21.3": Get "https://auth.docker.io/token?scope=repository%3Alibrary%2Falpine%3Apull&service=registry.docker.io": net/http: request canceled while waiting for connection (Client.Timeout exceeded while awaiting headers)
```
debugging CI often feels like peeling an onion

```text
[CI/test] ⭐ Run Set up job
[CI/test] 🚀  Start image=alpine:3.21
[CI/test]   🐳  docker pull image=alpine:3.21 platform= username= forcePull=true
[CI/test]   🐳  docker create image=alpine:3.21 platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker run image=alpine:3.21 platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
[CI/test]   ✅  Success - Set up job
[CI/test]   ☁  git clone 'https://github.com/actions/setup-node' # ref=v4
[CI/test] ⭐ Run Main actions/checkout@v4
[CI/test]   🐳  docker cp src=/home/sergueik/src/springboot_study/basic-act/nodejs/. dst=/home/sergueik/src/springboot_study/basic-act/nodejs
[CI/test]   ✅  Success - Main actions/checkout@v4 [12.78728ms]
[CI/test] ⭐ Run Main actions/setup-node@v4
[CI/test]   🐳  docker cp src=/home/sergueik/.cache/act/actions-setup-node@v4/ dst=/var/run/act/actions/actions-setup-node@v4/
[CI/test]   🐳  docker exec cmd=[node /var/run/act/actions/actions-setup-node@v4/dist/setup/index.js] user= workdir=
| OCI runtime exec failed: exec failed: unable to start container process: exec: "node": executable file not found in $PATH
[CI/test]   ❌  Failure - Main actions/setup-node@v4 [489.510089ms]
[CI/test] exitcode '127': command not found, please refer to https://github.com/nektos/act/issues/107 for more information
[CI/test] ⭐ Run Complete job
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job failed
Error: Job 'test' failed

```
indicates there is a 


```text
there is a bootstrap dependency:

Need Node
    ↓
to run setup-node
    ↓
whose job is to install Node
```
this workflow commit demonstrates a semi-circular dependency: `setup-node` requires a __Node__ runtime in order to *set up __Node__*.

After the workflow is fixed the log shows healthy run:

```text
[CI/test] ⭐ Run Set up job
[CI/test] 🚀  Start image=node:18-alpine
[CI/test]   🐳  docker pull image=node:18-alpine platform= username= forcePull=true
[CI/test]   🐳  docker create image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker run image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
[CI/test]   ✅  Success - Set up job
[CI/test]   ☁  git clone 'https://github.com/actions/setup-node' # ref=v4
[CI/test] ⭐ Run Main actions/checkout@v4
[CI/test]   🐳  docker cp src=/home/sergueik/src/springboot_study/basic-act/nodejs/. dst=/home/sergueik/src/springboot_study/basic-act/nodejs
[CI/test]   ✅  Success - Main actions/checkout@v4 [10.73517ms]
[CI/test] ⭐ Run Main actions/setup-node@v4
[CI/test]   🐳  docker cp src=/home/sergueik/.cache/act/actions-setup-node@v4/ dst=/var/run/act/actions/actions-setup-node@v4/
[CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v4/dist/setup/index.js] user= workdir=
| Found in cache @ /opt/hostedtoolcache/node/18.20.8/x64
[CI/test]   ❓  ::group::Environment details
| node: 
| npm: 10.8.2
| yarn: 1.22.22
[CI/test]   ❓  ::endgroup::
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/tsc.json
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/eslint-stylish.json
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/eslint-compact.json
[CI/test]   ✅  Success - Main actions/setup-node@v4 [1.035348523s]
[CI/test]   ⚙  ::set-output:: node-version=
[CI/test]   ⚙  ::add-path:: /opt/hostedtoolcache/node/18.20.8/x64/bin
[CI/test] ⭐ Run Main npm install
[CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/2.sh] user= workdir=
| npm warn deprecated inflight@1.0.6: This module is not supported, and leaks memory. Do not use it. Check out lru-cache if you want a good and tested way to coalesce async requests by a key value, which is much more comprehensive and powerful.
| npm warn deprecated glob@7.1.2: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
| npm warn deprecated rimraf@2.6.3: Rimraf versions prior to v4 are no longer supported
| npm warn deprecated superagent@8.1.2: Please upgrade to superagent v10.2.2+, see release notes at https://github.com/forwardemail/superagent/releases/tag/v10.2.2 - maintenance is supported by Forward Email @ https://forwardemail.net
| npm warn deprecated glob@7.2.3: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
| npm warn deprecated mkdirp@0.5.1: Legacy versions of mkdirp are no longer supported. Please update to mkdirp 1.x. (Note that the API surface has changed to use Promises in 1.x.)
| npm warn deprecated eslint@6.8.0: This version is no longer supported. Please see https://eslint.org/version-support for other options.
| 
| added 375 packages, and audited 376 packages in 23s
| 
| 104 packages are looking for funding
|   run `npm fund` for details
| 
| 12 vulnerabilities (3 low, 6 high, 3 critical)
| 
| To address all issues (including breaking changes), run:
|   npm audit fix --force
| 
| Run `npm audit` for details.
[CI/test]   ✅  Success - Main npm install [23.143761578s]
[CI/test] ⭐ Run Main npm test
[CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/3.sh] user= workdir=
| 
| > github-actions-demo@1.0.0 test
| > mocha ./tests --recursive
| 
| 
| 
|   GET /
|     ✓ should respond with hello world
| 
| 
|   1 passing (31ms)
| 
[CI/test]   ✅  Success - Main npm test [810.713437ms]
[CI/test] ⭐ Run Post actions/setup-node@v4
[CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v4/dist/cache-save/index.js] user= workdir=
[CI/test]   ✅  Success - Post actions/setup-node@v4 [402.368825ms]
[CI/test] ⭐ Run Complete job
[CI/test] Cleaning up container for job test
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job succeeded

```
* examine inventory
```
docker image ls
```
```text
alpine:3.21                   2607caa98058       7.83MB             0B        
node:18-alpine                ee77c6cd7c18        127MB             0B        
```
> NOTE: The Micro runner image uses `node:16-buster-slim`, which is pulled by act if it is not already available locally.
### Cleanup
```sh
rm act_Linux_x86_64.tar.gz
docker container prune -f
docker image rm node:16-buster-slim alpine:3.21  node:18-alpine|| true
```

```sh
pushd java
../act
```
```text
[CI/test] ⭐ Run Set up job
[CI/test] 🚀  Start image=atools/jdk-maven-node:mvn3-jdk11-node16
[CI/test]   🐳  docker pull image=atools/jdk-maven-node:mvn3-jdk11-node16 platform= username= forcePull=true
[CI/test]   🐳  docker create image=atools/jdk-maven-node:mvn3-jdk11-node16 platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker run image=atools/jdk-maven-node:mvn3-jdk11-node16 platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
[CI/test]   ✅  Success - Set up job
[CI/test]   ☁  git clone 'https://github.com/actions/setup-java' # ref=v4
[CI/test] Non-terminating error while running 'git clone': some refs were not updated
[CI/test]   ☁  git clone 'https://github.com/actions/upload-artifact' # ref=v4
[CI/test] ⭐ Run Main actions/checkout@v6
[CI/test]   🐳  docker cp src=/home/sergueik/src/springboot_study/basic-act/java/. dst=/home/sergueik/src/springboot_study/basic-act/java
[CI/test]   ✅  Success - Main actions/checkout@v6 [18.98786ms]
[CI/test] ⭐ Run Main Set up JDK 11 for x64
[CI/test]   🐳  docker cp src=/home/sergueik/.cache/act/actions-setup-java@v4/ dst=/var/run/act/actions/actions-setup-java@v4/
[CI/test]   🐳  docker exec cmd=[/usr/bin/node /var/run/act/actions/actions-setup-java@v4/dist/setup/index.js] user= workdir=
[CI/test]   ❓  ::group::Installed distributions
| Resolved Java 11.0.31+11 from tool-cache
| Setting Java 11.0.31+11 as the default
| Creating toolchains.xml for JDK version 11 from temurin
| Writing to /root/.m2/toolchains.xml
| 
| Java configuration:
|   Distribution: temurin
|   Version: 11.0.31+11
|   Path: /opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/11.0.31-11/x64
| 
[CI/test]   ❓  ::endgroup::
[CI/test]   ❓ add-matcher /run/act/actions/actions-setup-java@v4/.github/java.json
| Creating settings.xml with server-id: github
| Writing to /root/.m2/settings.xml
| maven cache is not found
[CI/test]   ✅  Success - Main Set up JDK 11 for x64 [962.262249ms]
[CI/test]   ⚙  ::set-env:: JAVA_HOME_11_X64=/opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/11.0.31-11/x64
[CI/test]   ⚙  ::set-env:: JAVA_HOME=/opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/11.0.31-11/x64
[CI/test]   ⚙  ::set-output:: distribution=Temurin-Hotspot
[CI/test]   ⚙  ::set-output:: path=/opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/11.0.31-11/x64
[CI/test]   ⚙  ::set-output:: version=11.0.31+11
[CI/test]   ⚙  ::set-output:: cache-hit=false
[CI/test]   ⚙  ::add-path:: /opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/11.0.31-11/x64/bin
[CI/test] ⭐ Run Main Build with Maven test phase
[CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/2.sh] user= workdir=
| [INFO] Scanning for projects...
| [WARNING] 
| [WARNING] Some problems were encountered while building the effective model for example:basic-tests:jar:0.38.0-SNAPSHOT
| [WARNING] 'repositories.repository.id' must not be 'local', this identifier is reserved for the local repository, using it for other repositories will corrupt your repository metadata. @ line 115, column 11
| [WARNING] 
| [WARNING] It is highly recommended to fix these problems because they threaten the stability of your build.
| [WARNING] 
| [WARNING] For this reason, future Maven versions might no longer support building such malformed projects.
| [WARNING] 
| [INFO] 
| [INFO] ------------------------< example:basic-tests >-------------------------
| [INFO] Building basic 0.38.0-SNAPSHOT
| [INFO] --------------------------------[ jar ]---------------------------------
| [WARNING] The POM for org.slf4j:jul-to-slf4j:jar:1.7.34 is missing, no dependency information available
| [INFO] ------------------------------------------------------------------------
| [INFO] BUILD FAILURE
| [INFO] ------------------------------------------------------------------------
| [INFO] Total time:  1.336 s
| [INFO] Finished at: 2026-07-11T05:36:52Z
| [INFO] ------------------------------------------------------------------------
| [ERROR] Failed to execute goal on project basic-tests: Could not resolve dependencies for project example:basic-tests:jar:0.38.0-SNAPSHOT: Cannot access local (file:///root/.m2/repository) in offline mode and the artifact org.slf4j:jul-to-slf4j:jar:1.7.34 has not been downloaded from it before. -> [Help 1]
| [ERROR] 
| [ERROR] To see the full stack trace of the errors, re-run Maven with the -e switch.
| [ERROR] Re-run Maven using the -X switch to enable full debug logging.
| [ERROR] 
| [ERROR] For more information about the errors and possible solutions, please read the following articles:
| [ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/DependencyResolutionException
[CI/test]   ❌  Failure - Main Build with Maven test phase [2m12.558808471s]
[CI/test] exitcode '1': failure
[CI/test] ⭐ Run Post Set up JDK 11 for x64
[CI/test]   🐳  docker exec cmd=[/usr/bin/node /var/run/act/actions/actions-setup-java@v4/dist/cleanup/index.js] user= workdir=
[CI/test]   ✅  Success - Post Set up JDK 11 for x64 [1.054757607s]
[CI/test] ⭐ Run Complete job
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job failed
Error: Job 'test' failed
```
```sh
mvn depednency:tree
```
```text
[INFO] +- net.bytebuddy:byte-buddy:jar:1.10.20:compile
[INFO] +- com.google.code.gson:gson:jar:2.8.9:compile
[INFO] +- org.springframework.boot:spring-boot-starter-web:jar:2.3.4.RELEASE:compile
[INFO] |  +- org.springframework.boot:spring-boot-starter:jar:2.3.4.RELEASE:compile
[INFO] |  |  +- org.springframework.boot:spring-boot:jar:2.3.4.RELEASE:compile
[INFO] |  |  +- org.springframework.boot:spring-boot-autoconfigure:jar:2.3.4.RELEASE:compile
[INFO] |  |  +- org.springframework.boot:spring-boot-starter-logging:jar:2.3.4.RELEASE:compile
[INFO] |  |  |  +- ch.qos.logback:logback-classic:jar:1.2.3:compile
[INFO] |  |  |  |  \- ch.qos.logback:logback-core:jar:1.2.3:compile
[INFO] |  |  |  +- org.apache.logging.log4j:log4j-to-slf4j:jar:2.13.3:compile
[INFO] |  |  |  |  \- org.apache.logging.log4j:log4j-api:jar:2.13.3:compile
[INFO] |  |  |  \- org.slf4j:jul-to-slf4j:jar:1.7.30:compile
[INFO] |  |  +- jakarta.annotation:jakarta.annotation-api:jar:1.3.5:compile
[INFO] |  |  \- org.yaml:snakeyaml:jar:1.26:compile
[INFO] |  +- org.springframework.boot:spring-boot-starter-json:jar:2.3.4.RELEASE:compile
[INFO] |  |  +- com.fasterxml.jackson.core:jackson-databind:jar:2.11.2:compile
[INFO] |  |  |  +- com.fasterxml.jackson.core:jackson-annotations:jar:2.11.2:compile
[INFO] |  |  |  \- com.fasterxml.jackson.core:jackson-core:jar:2.11.2:compile
[INFO] |  |  +- com.fasterxml.jackson.datatype:jackson-datatype-jdk8:jar:2.11.2:compile
[INFO] |  |  +- com.fasterxml.jackson.datatype:jackson-datatype-jsr310:jar:2.11.2:compile
[INFO] |  |  \- com.fasterxml.jackson.module:jackson-module-parameter-names:jar:2.11.2:compile
[INFO] |  +- org.springframework.boot:spring-boot-starter-tomcat:jar:2.3.4.RELEASE:compile
[INFO] |  |  +- org.apache.tomcat.embed:tomcat-embed-core:jar:9.0.38:compile
[INFO] |  |  +- org.glassfish:jakarta.el:jar:3.0.3:compile
[INFO] |  |  \- org.apache.tomcat.embed:tomcat-embed-websocket:jar:9.0.38:compile
[INFO] |  +- org.springframework:spring-web:jar:5.2.9.RELEASE:compile
[INFO] |  |  \- org.springframework:spring-beans:jar:5.2.9.RELEASE:compile
[INFO] |  \- org.springframework:spring-webmvc:jar:5.2.9.RELEASE:compile
[INFO] |     +- org.springframework:spring-aop:jar:5.2.9.RELEASE:compile
[INFO] |     +- org.springframework:spring-context:jar:5.2.9.RELEASE:compile
[INFO] |     \- org.springframework:spring-expression:jar:5.2.9.RELEASE:compile
[INFO] +- org.springframework.boot:spring-boot-starter-test:jar:2.3.4.RELEASE:test
[INFO] |  +- org.springframework.boot:spring-boot-test:jar:2.3.4.RELEASE:test
[INFO] |  +- org.springframework.boot:spring-boot-test-autoconfigure:jar:2.3.4.RELEASE:test
[INFO] |  +- com.jayway.jsonpath:json-path:jar:2.4.0:test
[INFO] |  |  \- net.minidev:json-smart:jar:2.3:test
[INFO] |  |     \- net.minidev:accessors-smart:jar:1.2:test
[INFO] |  |        \- org.ow2.asm:asm:jar:5.0.4:test
[INFO] |  +- jakarta.xml.bind:jakarta.xml.bind-api:jar:2.3.3:test
[INFO] |  |  \- jakarta.activation:jakarta.activation-api:jar:1.2.2:test
[INFO] |  +- org.assertj:assertj-core:jar:3.16.1:test
[INFO] |  +- org.hamcrest:hamcrest:jar:2.2:test
[INFO] |  +- org.junit.jupiter:junit-jupiter:jar:5.6.2:test
[INFO] |  |  +- org.junit.jupiter:junit-jupiter-params:jar:5.6.2:test
[INFO] |  |  \- org.junit.jupiter:junit-jupiter-engine:jar:5.6.2:test
[INFO] |  |     \- org.junit.platform:junit-platform-engine:jar:1.6.2:test
[INFO] |  +- org.mockito:mockito-core:jar:3.10.0:test
[INFO] |  |  +- net.bytebuddy:byte-buddy-agent:jar:1.10.14:test
[INFO] |  |  \- org.objenesis:objenesis:jar:3.2:test
[INFO] |  +- org.skyscreamer:jsonassert:jar:1.5.0:test
[INFO] |  |  \- com.vaadin.external.google:android-json:jar:0.0.20131108.vaadin1:test
[INFO] |  +- org.springframework:spring-core:jar:5.2.9.RELEASE:compile
[INFO] |  |  \- org.springframework:spring-jcl:jar:5.2.9.RELEASE:compile
[INFO] |  +- org.springframework:spring-test:jar:5.2.9.RELEASE:test
[INFO] |  \- org.xmlunit:xmlunit-core:jar:2.7.0:test
[INFO] +- org.slf4j:slf4j-api:jar:1.7.34:compile
[INFO] \- org.mockito:mockito-junit-jupiter:jar:3.10.0:test
[INFO]    \- org.junit.jupiter:junit-jupiter-api:jar:5.6.2:test
[INFO]       +- org.apiguardian:apiguardian-api:jar:1.1.0:test
[INFO]       +- org.opentest4j:opentest4j:jar:1.2.0:test
[INFO]       \- org.junit.platform:junit-platform-commons:jar:1.6.2:test
[INFO] ------------------------------------------------------------------------
```

> NOTE: fixing through exclusions is very time consuming

workaround: temporariy remove offline flag.

### See Also

   * [introduction to GitHub Actions with Docker](https://docs.docker.com/guides/gha/)
   * [curated list of awesome actions to use on GitHub](https://github.com/cplee/awesome-actions/tree/master#collection-of-actions)
   * Runners - 
     + https://hub.docker.com/search?q=act-runner
     + https://hub.docker.com/search?q=act_runner
     + https://hub.docker.com/search?q=act
The Workflows that themselves invoke __Docker++ may require [Docker-in-Docker (DinD)](https://github.com/jpetazzo/dind) or access to the host Docker socket.
   * recommended for Java maven [gitea/act_runner](https://hub.docker.com/r/gitea/act_runner): 
```sh
docker pull gitea/act_runner:0.6.1
docker pull gitea/act_runner:0.6-dind
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
