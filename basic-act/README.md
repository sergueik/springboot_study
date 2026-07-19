 ### Info

[GitHub Actions](https://docs.github.com/en/actions)
allows automatic [building and testing your code](https://docs.github.com/en/actions/tutorials/build-and-test-code)


[act](https://nektosact.com/) is an open-source command-line tool developed by Nektos that allows
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
[CI/test] ⭐ Run Main actions/checkout@v6
[CI/test]   🐳  docker cp src=/home/sergueik/src/springboot_study/basic-act/java/. dst=/home/sergueik/src/springboot_study/basic-act/java
[CI/test]   ✅  Success - Main actions/checkout@v6 [47.062251ms]
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
[CI/test]   ✅  Success - Main Set up JDK 11 for x64 [1.241658079s]
[CI/test]   ⚙  ::set-env:: JAVA_HOME=/opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/11.0.31-11/x64
[CI/test]   ⚙  ::set-env:: JAVA_HOME_11_X64=/opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/11.0.31-11/x64
[CI/test]   ⚙  ::set-output:: version=11.0.31+11
[CI/test]   ⚙  ::set-output:: cache-hit=false
[CI/test]   ⚙  ::set-output:: distribution=Temurin-Hotspot
[CI/test]   ⚙  ::set-output:: path=/opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/11.0.31-11/x64
[CI/test]   ⚙  ::add-path:: /opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/11.0.31-11/x64/bin
[CI/test] ⭐ Run Main Build with Maven test phase
[CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/2.sh] user= workdir=
| [INFO] Scanning for projects...
| [WARNING]
| [WARNING] Some problems were encountered while building the effective model for example:basic-tests:jar:0.1.0-SNAPSHOT
| [WARNING] 'repositories.repository.id' must not be 'local', this identifier is reserved for the local repository, using it for other repositories will corrupt your repository metadata. @ line 78, column 11
| [WARNING]
| [WARNING] It is highly recommended to fix these problems because they threaten the stability of your build.
| [WARNING]
| [WARNING] For this reason, future Maven versions might no longer support building such malformed projects.
| [WARNING]
| [INFO]
| [INFO] ------------------------< example:basic-tests >-------------------------
| [INFO] Building basic 0.1.0-SNAPSHOT
| [INFO] --------------------------------[ jar ]---------------------------------
| [INFO] Downloading from local: file:///root/.m2/repository/net/minidev/json-smart/2.4.8/json-smart-2.4.8.pom
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/net/minidev/json-smart/2.4.8/json-smart-2.4.8.pom
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/minidev/json-smart/2.4.8/json-smart-2.4.8.pom
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/minidev/json-smart/2.4.8/json-smart-2.4.8.pom (8.3 kB at 6.9 kB/s)
| [INFO] Downloading from local: file:///root/.m2/repository/net/minidev/accessors-smart/2.4.8/accessors-smart-2.4.8.pom
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/net/minidev/accessors-smart/2.4.8/accessors-smart-2.4.8.pom
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/minidev/accessors-smart/2.4.8/accessors-smart-2.4.8.pom
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/minidev/accessors-smart/2.4.8/accessors-smart-2.4.8.pom (10 kB at 24 kB/s)
| [INFO] Downloading from local: file:///root/.m2/repository/com/fasterxml/jackson/dataformat/jackson-dataformat-yaml/2.13.4/jackson-dataformat-yaml-2.13.4.pom
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/com/fasterxml/jackson/dataformat/jackson-dataformat-yaml/2.13.4/jackson-dataformat-yaml-2.13.4.pom
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/com/fasterxml/jackson/dataformat/jackson-dataformat-yaml/2.13.4/jackson-dataformat-yaml-2.13.4.pom
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/com/fasterxml/jackson/dataformat/jackson-dataformat-yaml/2.13.4/jackson-dataformat-yaml-2.13.4.pom (2.6 kB at 5.4 kB/s)
| [INFO] Downloading from local: file:///root/.m2/repository/com/fasterxml/jackson/dataformat/jackson-dataformats-text/2.13.4/jackson-dataformats-text-2.13.4.pom
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/com/fasterxml/jackson/dataformat/jackson-dataformats-text/2.13.4/jackson-dataformats-text-2.13.4.pom
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/com/fasterxml/jackson/dataformat/jackson-dataformats-text/2.13.4/jackson-dataformats-text-2.13.4.pom
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/com/fasterxml/jackson/dataformat/jackson-dataformats-text/2.13.4/jackson-dataformats-text-2.13.4.pom (2.9 kB at 7.2 kB/s)
| [INFO] Downloading from local: file:///root/.m2/repository/net/bytebuddy/byte-buddy/1.12.22/byte-buddy-1.12.22.pom
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/net/bytebuddy/byte-buddy/1.12.22/byte-buddy-1.12.22.pom
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy/1.12.22/byte-buddy-1.12.22.pom
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy/1.12.22/byte-buddy-1.12.22.pom (16 kB at 29 kB/s)
| [INFO] Downloading from local: file:///root/.m2/repository/net/bytebuddy/byte-buddy-parent/1.12.22/byte-buddy-parent-1.12.22.pom
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/net/bytebuddy/byte-buddy-parent/1.12.22/byte-buddy-parent-1.12.22.pom
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy-parent/1.12.22/byte-buddy-parent-1.12.22.pom
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy-parent/1.12.22/byte-buddy-parent-1.12.22.pom (58 kB at 98 kB/s)
| [INFO] Downloading from local: file:///root/.m2/repository/net/bytebuddy/byte-buddy-agent/1.12.22/byte-buddy-agent-1.12.22.pom
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/net/bytebuddy/byte-buddy-agent/1.12.22/byte-buddy-agent-1.12.22.pom
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy-agent/1.12.22/byte-buddy-agent-1.12.22.pom
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy-agent/1.12.22/byte-buddy-agent-1.12.22.pom (10 kB at 28 kB/s)
| [INFO] Downloading from local: file:///root/.m2/repository/net/minidev/json-smart/2.4.8/json-smart-2.4.8.jar
| [INFO] Downloading from local: file:///root/.m2/repository/net/minidev/accessors-smart/2.4.8/accessors-smart-2.4.8.jar
| [INFO] Downloading from local: file:///root/.m2/repository/com/fasterxml/jackson/dataformat/jackson-dataformat-yaml/2.13.4/jackson-dataformat-yaml-2.13.4.jar
| [INFO] Downloading from local: file:///root/.m2/repository/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
| [INFO] Downloading from local: file:///root/.m2/repository/net/bytebuddy/byte-buddy-agent/1.12.22/byte-buddy-agent-1.12.22.jar
| [INFO] Downloading from local: file:///root/.m2/repository/net/bytebuddy/byte-buddy/1.12.22/byte-buddy-1.12.22.jar
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/net/minidev/json-smart/2.4.8/json-smart-2.4.8.jar
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/net/minidev/accessors-smart/2.4.8/accessors-smart-2.4.8.jar
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/com/fasterxml/jackson/dataformat/jackson-dataformat-yaml/2.13.4/jackson-dataformat-yaml-2.13.4.jar
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/net/bytebuddy/byte-buddy/1.12.22/byte-buddy-1.12.22.jar
| [INFO] Downloading from maven-central: https://mvnrepository.com/repos/central/net/bytebuddy/byte-buddy-agent/1.12.22/byte-buddy-agent-1.12.22.jar
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/minidev/json-smart/2.4.8/json-smart-2.4.8.jar
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/minidev/accessors-smart/2.4.8/accessors-smart-2.4.8.jar
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/com/fasterxml/jackson/dataformat/jackson-dataformat-yaml/2.13.4/jackson-dataformat-yaml-2.13.4.jar
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy/1.12.22/byte-buddy-1.12.22.jar
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/minidev/json-smart/2.4.8/json-smart-2.4.8.jar (120 kB at 204 kB/s)
| [INFO] Downloading from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy-agent/1.12.22/byte-buddy-agent-1.12.22.jar
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/minidev/accessors-smart/2.4.8/accessors-smart-2.4.8.jar (30 kB at 32 kB/s)
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar (41 kB at 42 kB/s)
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/com/fasterxml/jackson/dataformat/jackson-dataformat-yaml/2.13.4/jackson-dataformat-yaml-2.13.4.jar (52 kB at 51 kB/s)
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy/1.12.22/byte-buddy-1.12.22.jar (3.9 MB at 1.8 MB/s)
| [INFO] Downloaded from verve: https://verve.jfrog.io/artifactory/verve-gradle-release/net/bytebuddy/byte-buddy-agent/1.12.22/byte-buddy-agent-1.12.22.jar (257 kB at 114 kB/s)
| [INFO]
| [INFO] --- maven-resources-plugin:3.2.0:resources (default-resources) @ basic-tests ---
| [INFO] Using 'UTF-8' encoding to copy filtered resources.
| [INFO] Using 'UTF-8' encoding to copy filtered properties files.
| [INFO] skip non existing resourceDirectory /home/sergueik/src/springboot_study/basic-act/java/src/main/resources
| [INFO]
| [INFO] --- maven-compiler-plugin:3.10.1:compile (default-compile) @ basic-tests ---
| [INFO] Changes detected - recompiling the module!
| [INFO] Compiling 7 source files to /home/sergueik/src/springboot_study/basic-act/java/target/classes
| [INFO]
| [INFO] --- maven-resources-plugin:3.2.0:testResources (default-testResources) @ basic-tests ---
| [INFO] Using 'UTF-8' encoding to copy filtered resources.
| [INFO] Using 'UTF-8' encoding to copy filtered properties files.
| [INFO] skip non existing resourceDirectory /home/sergueik/src/springboot_study/basic-act/java/src/test/resources
| [INFO]
| [INFO] --- maven-compiler-plugin:3.10.1:testCompile (default-testCompile) @ basic-tests ---
| [INFO] No sources to compile
| [INFO]
| [INFO] --- maven-surefire-plugin:2.22.2:test (default-test) @ basic-tests ---
| [INFO] No tests to run.
| [INFO] ------------------------------------------------------------------------
| [INFO] BUILD SUCCESS
| [INFO] ------------------------------------------------------------------------
| [INFO] Total time:  14.483 s
| [INFO] Finished at: 2026-07-11T13:22:09Z
| [INFO] ------------------------------------------------------------------------
| [INFO] Scanning for projects...
| [WARNING]
| [WARNING] Some problems were encountered while building the effective model for example:basic-tests:jar:0.1.0-SNAPSHOT
| [WARNING] 'repositories.repository.id' must not be 'local', this identifier is reserved for the local repository, using it for other repositories will corrupt your repository metadata. @ line 78, column 11
| [WARNING]
| [WARNING] It is highly recommended to fix these problems because they threaten the stability of your build.
| [WARNING]
| [WARNING] For this reason, future Maven versions might no longer support building such malformed projects.
| [WARNING]
| [INFO]
| [INFO] ------------------------< example:basic-tests >-------------------------
| [INFO] Building basic 0.1.0-SNAPSHOT
| [INFO] --------------------------------[ jar ]---------------------------------
| [INFO]
| [INFO] --- maven-resources-plugin:3.2.0:resources (default-resources) @ basic-tests ---
| [INFO] Using 'UTF-8' encoding to copy filtered resources.
| [INFO] Using 'UTF-8' encoding to copy filtered properties files.
| [INFO] skip non existing resourceDirectory /home/sergueik/src/springboot_study/basic-act/java/src/main/resources
| [INFO]
| [INFO] --- maven-compiler-plugin:3.10.1:compile (default-compile) @ basic-tests ---
| [INFO] Nothing to compile - all classes are up to date
| [INFO]
| [INFO] --- maven-resources-plugin:3.2.0:testResources (default-testResources) @ basic-tests ---
| [INFO] Using 'UTF-8' encoding to copy filtered resources.
| [INFO] Using 'UTF-8' encoding to copy filtered properties files.
| [INFO] skip non existing resourceDirectory /home/sergueik/src/springboot_study/basic-act/java/src/test/resources
| [INFO]
| [INFO] --- maven-compiler-plugin:3.10.1:testCompile (default-testCompile) @ basic-tests ---
| [INFO] No sources to compile
| [INFO]
| [INFO] --- maven-surefire-plugin:2.22.2:test (default-test) @ basic-tests ---
| [INFO] Tests are skipped.
| [INFO]
| [INFO] --- maven-jar-plugin:3.2.2:jar (default-jar) @ basic-tests ---
| [INFO] Building jar: /home/sergueik/src/springboot_study/basic-act/java/target/basic-tests-0.1.0-SNAPSHOT.jar
| [INFO]
| [INFO] --- spring-boot-maven-plugin:2.7.8:repackage (repackage) @ basic-tests ---
| [INFO] Replacing main artifact with repackaged archive
| [INFO] ------------------------------------------------------------------------
| [INFO] BUILD SUCCESS
| [INFO] ------------------------------------------------------------------------
| [INFO] Total time:  5.369 s
| [INFO] Finished at: 2026-07-11T13:22:18Z
| [INFO] ------------------------------------------------------------------------
[CI/test]   ✅  Success - Main Build with Maven test phase [3m17.237246008s]
[CI/test] ⭐ Run Post Set up JDK 11 for x64
[CI/test]   🐳  docker exec cmd=[/usr/bin/node /var/run/act/actions/actions-setup-java@v4/dist/cleanup/index.js] user= workdir=
| [command]/bin/tar --posix -cf cache.tgz --exclude cache.tgz -P -C /home/sergueik/src/springboot_study/basic-act/java --files-from manifest.txt -z
| Cache Size: ~95 MB (99931307 B)
| Cache saved successfully
| Cache saved with the key: setup-java-Linux-x64-maven-aeaffdb5d437ef2e6d439c319ea15fe6fe24aca560944eadcb0762746cc0daf9
[CI/test]   ✅  Success - Post Set up JDK 11 for x64 [7.611009354s]
[CI/test] ⭐ Run Complete job
[CI/test] Cleaning up container for job test
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job succeeded
```

> NOTE: suppressed `artifact upload` step and offline option in maven.
The `dependency:go-offline` often misses transitive dependencies, especially those pulled in by plugins, Surefire, ByteBuddy


### Cleanup

```sh
rm act_Linux_x86_64.tar.gz
docker container prune -f
docker image rm alpine:3.21 node:18-alpine node:16-buster-slim atools/jdk-maven-node:mvn3-jdk11-node16|| true
```
### Dockerized Act

```sh
IMAGE=efrecon/act:v0.2.89
docker pull $IMAGE
```
```sh
docker run --entrypoint '' -it --rm --name actrunner $IMAGE sh
```
```sh
/ # find / -iname 'act'
```
```text
/usr/local/bin/act
/sys/module/thermal/parameters/act
```

```sh
act -h
```
```text
Run GitHub actions locally by specifying the event name (e.g. `push`) or an action name directly.

Usage:
  act [event name to run] [flags]

If no event name passed, will default to "on: push"
...
```
run with the job name defined in the workflow (`test`):

```sh
mkdir -p $HOME/.config/act
touch $HOME/.config/act/actrc
cat <<EOF| tee $HOME/.config/act/actrc
-P ubuntu-18.04=node:16-buster-slim
EOF
cat <<EOF|tee $HOME/.config/act/event.json
{
  "ref": "refs/heads/main",
  "repository": {
    "full_name": "local/dummy"
  }
}
EOF
```
```sh
PROJECT=nodejs
JOB=test
docker run --rm -v "$PWD/$PROJECT":/workspace -w /workspace -v /var/run/docker.sock:/var/run/docker.sock -v "$HOME/.config/act:/root/.config/act" -it $IMAGE -W .github/workflows/main.yml --detect-event=false --eventpath /root/.config/act/event.json -j $JOB  
```
```text
INFO[0000] Using docker host 'unix:///var/run/docker.sock', and daemon socket 'unix:///var/run/docker.sock'
```
it may prompt
```
? Please choose the default image you want to use with act:
  - Large size image: ca. 17GB download + 53.1GB storage, you will need 75GB of free disk space, snapshots of GitHub Hosted Runners without snap and pulled docker images
  - Medium size image: ~500MB, includes only necessary tools to bootstrap actions and aims to be compatible with most actions
  - Micro size image: <200MB, contains only NodeJS required to bootstrap actions, doesn't work with all actions
```
select `Micro`
```text
Default image and other options can be changed manually in /root/.config/act/actrc (please refer to https://nektosact.com/usage/index.html?highlight=configur#configuration-file for additional information about file structure) Micro
```
```
INFO[0000] Using docker host 'unix:///var/run/docker.sock', and daemon socket 'unix:///var/run/docker.sock' 
ERRO[0000] path/workspacenot located inside a git repository  error="repository does not exist"
WARN[0000] unable to get git revision: repository does not exist 
[CI/test] ⭐ Run Set up job
[CI/test] 🚀  Start image=node:18-alpine
[CI/test]   🐳  docker pull image=node:18-alpine platform= username= forcePull=true
[CI/test]   🐳  docker create image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker run image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
[CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
[CI/test]   ✅  Success - Set up job
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test]   ☁  git clone 'https://github.com/actions/setup-node' # ref=v4

[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] ⭐ Run Main actions/checkout@v4
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test]   🐳  docker cp src=/workspace/. dst=/workspace
[CI/test]   ✅  Success - Main actions/checkout@v4 [10.566904ms]
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] ⭐ Run Main actions/setup-node@v4
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test]   🐳  docker cp src=/root/.cache/act/actions-setup-node@v4/ dst=/var/run/act/actions/actions-setup-node@v4/
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
[CI/test]   ✅  Success - Main actions/setup-node@v4 [814.950828ms]
[CI/test]   ⚙  ::set-output:: node-version=
[CI/test]   ⚙  ::add-path:: /opt/hostedtoolcache/node/18.20.8/x64/bin
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] ⭐ Run Main npm install
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/2.sh] user= workdir=
| npm warn deprecated inflight@1.0.6: This module is not supported, and leaks memory. Do not use it. Check out lru-cache if you want a good and tested way to coalesce async requests by a key value, which is much more comprehensive and powerful.
| npm warn deprecated glob@7.1.2: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
| npm warn deprecated rimraf@2.6.3: Rimraf versions prior to v4 are no longer supported
| npm warn deprecated superagent@8.1.2: Please upgrade to superagent v10.2.2+, see release notes at https://github.com/forwardemail/superagent/releases/tag/v10.2.2 - maintenance is supported by Forward Email @ https://forwardemail.net
| npm warn deprecated glob@7.2.3: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
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
[CI/test]   ✅  Success - Main npm install [22.238756172s]
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] ⭐ Run Main npm test
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
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
|   1 passing (25ms)
| 
[CI/test]   ✅  Success - Main npm test [521.649642ms]
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test] ⭐ Run Post actions/setup-node@v4
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v4/dist/cache-save/index.js] user= workdir=
[CI/test]   ✅  Success - Post actions/setup-node@v4 [249.357912ms]
[CI/test] ⭐ Run Complete job
[CI/test] Cleaning up container for job test
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
[CI/test]   ✅  Success - Complete job
[CI/test] 🏁  Job succeeded

```

ignore all errors:
```text
[CI/test] path/workspacenot located inside a git repository
[CI/test] unable to get git revision: repository does not exist
```

- there is no .git directory in the container - this is intentional 

NOTE: the [Dockerfile](https://github.com/efrecon/docker-images/blob/master/act/Dockerfile) is essentially bare minimal possible example of a binary wrapper container featuring the fabulous [tarinstall.sh](https://github.com/efrecon/bininstall/blob/5b6a31a0e2c6f38491ec02491dd6c151ed05d708/tarinstall.sh)

```sh
Alpine rootfs
    |
    +-- wget/curl release artifact
    |
    +-- unpack nektos/act binary
    |
    +-- /usr/local/bin/act
    |
    +-- ENTRYPOINT act
```
### Usage

```sh
docker build -f Dockefile -t act .
```
```text
Sending build context to Docker daemon  593.4kB
Step 1/10 : FROM alpine:3.18
 ---> 802c91d52981
Step 2/10 : ARG VERSION=latest
 ---> Using cache
 ---> c4d62cba6f70
Step 3/10 : ARG PROJECT=nektos/act
 ---> Using cache
 ---> 60e0367eefae
Step 4/10 : ARG RELEASE=https://github.com/${PROJECT}/releases
 ---> Using cache
 ---> b63e9f2c2c72
Step 5/10 : ARG ACT_DWROOT=${RELEASE}/download
 ---> Using cache
 ---> cf9a1344528e
Step 6/10 : RUN apk add --no-cache less curl git ca-certificates
 ---> Using cache
 ---> 52b381fd4c96
Step 7/10 : COPY tarinstall.sh /usr/local/bin/
 ---> Using cache
 ---> 45e7058c1a4b
Step 8/10 : RUN if [ "$VERSION" = "latest" ]; then VERSION=$(wget -q  -O - "$RELEASE"|grep "href=\"/${PROJECT}/releases/tag/v[0-9].[0-9]*.[0-9]*\"" | grep -v no-underline | head -n 1 | sed 's/^.*href=//g' | cut -d '"' -f 2 | awk '{n=split($NF,a,"/");print a[n]}' | awk 'a !~ $0{print}; {a=$0}'); fi     && echo "tarinstall.sh -v -x \"act\" \"${RELEASE}/download/v${VERSION#v*}/act_Linux_x86_64.tar.gz\""     && tarinstall.sh -v -x "act" "${RELEASE}/download/v${VERSION#v*}/act_Linux_x86_64.tar.gz"
 ---> Running in 154d1d3d7b7b
tarinstall.sh -v -x "act" "https://github.com/nektos/act/releases/download/v0.2.89/act_Linux_x86_64.tar.gz"
Downloading https://github.com/nektos/act/releases/download/v0.2.89/act_Linux_x86_64.tar.gz
LICENSE
README.md
act
Installing as /usr/local/bin/act
Removing intermediate container 154d1d3d7b7b
 ---> bfe5228f9b23
Step 9/10 : ENTRYPOINT [ "/usr/local/bin/act" ]
 ---> Running in 79297518cc5d
Removing intermediate container 79297518cc5d
 ---> ab3d835eb286
Step 10/10 : CMD [ "--help" ]
 ---> Running in 9d16ea96f7b0
Removing intermediate container 9d16ea96f7b0
 ---> 221d30eabbd0
Successfully built 221d30eabbd0
Successfully tagged act:latest
```

```sh
WORKSPACE=$(pwd)/nodejs docker-compose up --build 
```
```text
Creating network "basic-act_default" with the default driver
Pulling act (alpine:3.18)...
3.18: Pulling from library/alpine
44cf07d57ee4: Pull complete
Digest: sha256:de0eb0b3f2a47ba1eb89389859a9bd88b28e82f5826b6969ad604979713c2d4f
Status: Downloaded newer image for alpine:3.18
Creating basic-act_act_1 ... done
Attaching to basic-act_act_1
act_1  | fetch https://dl-cdn.alpinelinux.org/alpine/v3.18/main/x86_64/APKINDEX.tar.gz
act_1  | fetch https://dl-cdn.alpinelinux.org/alpine/v3.18/community/x86_64/APKINDEX.tar.gz
act_1  | (1/18) Installing ncurses-terminfo-base (6.4_p20230506-r0)
act_1  | (2/18) Installing libncursesw (6.4_p20230506-r0)
act_1  | (3/18) Installing readline (8.2.1-r1)
act_1  | (4/18) Installing bash (5.2.15-r5)
act_1  | Executing bash-5.2.15-r5.post-install
act_1  | (5/18) Installing ca-certificates (20241121-r1)
act_1  | (6/18) Installing brotli-libs (1.0.9-r14)
act_1  | (7/18) Installing libunistring (1.1-r1)
act_1  | (8/18) Installing libidn2 (2.3.4-r1)
act_1  | (9/18) Installing nghttp2-libs (1.57.0-r0)
act_1  | (10/18) Installing libpsl (0.21.5-r0)
act_1  | (11/18) Installing libcurl (8.12.1-r0)
act_1  | (12/18) Installing curl (8.12.1-r0)
act_1  | (13/18) Installing libexpat (2.7.0-r0)
act_1  | (14/18) Installing pcre2 (10.42-r1)
act_1  | (15/18) Installing git (2.40.4-r0)
act_1  | (16/18) Installing libacl (2.3.1-r3)
act_1  | (17/18) Installing tar (1.34-r3)
act_1  | (18/18) Installing wget (1.21.4-r0)
act_1  | Executing busybox-1.36.1-r7.trigger
act_1  | Executing ca-certificates-20241121-r1.trigger
act_1  | OK: 22 MiB in 33 packages
act_1  | time="2026-07-19T13:41:25Z" level=info msg="Using docker host 'unix:///var/run/docker.sock', and daemon socket 'unix:///var/run/docker.sock'"
? Please choose the default image you want to use with act:
act_1  |   - Large size image: ca. 17GB download + 53.1GB storage, you will need 75GB of free disk space, snapshots of GitHub Hosted Runners without snap and pulled docker images
act_1  |   - Medium size image: ~500MB, includes only necessary tools to bootstrap actions and aims to be compatible with most actions
act_1  |   - Micro size image: <200MB, contains only NodeJS required to bootstrap actions, doesn't work with all actions
act_1  | 
act_1  | Default image and other options can be changed manually in /root/.config/act/actrc (please refer to https://nektosact.com/usage/index.html?highlight=configur#configuration-file for additional information about file structure)  [Use arrows to move, type to filter, ? for more help]
act_1  |   Large
act_1  | > Medium
act_1  |   Micro
act_1  | time="2026-07-19T13:41:25Z" level=fatal msg=EOF
basic-act_act_1 exited with code 1
Exception in thread Thread-4 (watch_events):
Traceback (most recent call last):
  File "/usr/lib/python3.10/threading.py", line 1016, in _bootstrap_inner
    self.run()
  File "/usr/lib/python3.10/threading.py", line 953, in run
    self._target(*self._args, **self._kwargs)
  File "/usr/lib/python3/dist-packages/compose/cli/log_printer.py", line 202, in watch_events
    for event in event_stream:
  File "/usr/lib/python3/dist-packages/compose/project.py", line 626, in yield_loop
    yield build_container_event(event)
  File "/usr/lib/python3/dist-packages/compose/project.py", line 594, in build_container_event
    container = Container.from_id(self.client, event['id'])
KeyError: 'id'


```
```sh
WORKSPACE=$(pwd)/nodejs docker-compose run --rm --build act
```
```text
Creating basic-act_act_1 ... done
Attaching to basic-act_act_1
act_1  | fetch https://dl-cdn.alpinelinux.org/alpine/v3.18/main/x86_64/APKINDEX.tar.gz
act_1  | fetch https://dl-cdn.alpinelinux.org/alpine/v3.18/community/x86_64/APKINDEX.tar.gz
act_1  | (1/18) Installing ncurses-terminfo-base (6.4_p20230506-r0)
act_1  | (2/18) Installing libncursesw (6.4_p20230506-r0)
act_1  | (3/18) Installing readline (8.2.1-r1)
act_1  | (4/18) Installing bash (5.2.15-r5)
act_1  | Executing bash-5.2.15-r5.post-install
act_1  | (5/18) Installing ca-certificates (20241121-r1)
act_1  | (6/18) Installing brotli-libs (1.0.9-r14)
act_1  | (7/18) Installing libunistring (1.1-r1)
act_1  | (8/18) Installing libidn2 (2.3.4-r1)
act_1  | (9/18) Installing nghttp2-libs (1.57.0-r0)
act_1  | (10/18) Installing libpsl (0.21.5-r0)
act_1  | (11/18) Installing libcurl (8.12.1-r0)
act_1  | (12/18) Installing curl (8.12.1-r0)
act_1  | (13/18) Installing libexpat (2.7.0-r0)
act_1  | (14/18) Installing pcre2 (10.42-r1)
act_1  | (15/18) Installing git (2.40.4-r0)
act_1  | (16/18) Installing libacl (2.3.1-r3)
act_1  | (17/18) Installing tar (1.34-r3)
act_1  | (18/18) Installing wget (1.21.4-r0)
act_1  | Executing busybox-1.36.1-r7.trigger
act_1  | Executing ca-certificates-20241121-r1.trigger
act_1  | OK: 22 MiB in 33 packages
act_1  | LICENSE
act_1  | README.md
act_1  | act
act_1  | install -m755 "/tmp/tmp.deFdfO/act" /usr/local/bin/act
act_1  | rm -rf "/tmp/tmp.deFdfO"
act_1  | root
act_1  | -P ubuntu-18.04=node:16-buster-slim
act_1  | time="2026-07-19T14:06:15Z" level=info msg="Using docker host 'unix:///var/run/docker.sock', and daemon socket 'unix:///var/run/docker.sock'"
act_1  | time="2026-07-19T14:06:15Z" level=error msg="path/workspacenot located inside a git repository" error="repository does not exist"
act_1  | time="2026-07-19T14:06:15Z" level=warning msg="unable to get git ref: repository does not exist"
act_1  | time="2026-07-19T14:06:15Z" level=error msg="path/workspacenot located inside a git repository" error="repository does not exist"
act_1  | time="2026-07-19T14:06:15Z" level=warning msg="unable to get git revision: repository does not exist"
act_1  | [CI/test] ⭐ Run Set up job
act_1  | [CI/test] 🚀  Start image=node:18-alpine
act_1  | [CI/test]   🐳  docker pull image=node:18-alpine platform= username= forcePull=true
act_1  | [CI/test]   🐳  docker create image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
act_1  | [CI/test]   🐳  docker run image=node:18-alpine platform= entrypoint=["tail" "-f" "/dev/null"] cmd=[] network="host"
act_1  | [CI/test]   🐳  docker exec cmd=[node --no-warnings -e console.log(process.execPath)] user= workdir=
act_1  | [CI/test]   ✅  Success - Set up job
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
...
act_1  | [CI/test]   ☁  git clone 'https://github.com/actions/setup-node' # ref=v4
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
...
act_1  | [CI/test] ⭐ Run Main actions/checkout@v4
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
...
act_1  | [CI/test] ⭐ Run Main actions/setup-node@v4
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
...
act_1  | [CI/test]   🐳  docker cp src=/root/.cache/act/actions-setup-node@v4/ dst=/var/run/act/actions/actions-setup-node@v4/
act_1  | [CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v4/dist/setup/index.js] user= workdir=
act_1  | [CI/test]   | Attempting to download 18...
act_1  | [CI/test]   | Acquiring 18.20.8 - x64 from https://github.com/actions/node-versions/releases/download/18.20.8-14110393767/node-18.20.8-linux-x64.tar.gz
act_1  | [CI/test]   | Extracting ...
act_1  | [CI/test]   | [command]/bin/tar xz --strip 1 -C /tmp/7ca6ec3e-aa6f-486f-92d2-300e6d36183c -f /tmp/c0a58a12-3d09-4f7a-8827-4d11923bd2c0
act_1  | [CI/test]   | Adding to the cache ...
act_1  | [CI/test]   ❓  ::group::Environment details
act_1  | [CI/test]   | node: 
act_1  | [CI/test]   | npm: 10.8.2
act_1  | [CI/test]   | yarn: 1.22.22
act_1  | [CI/test]   ❓  ::endgroup::
act_1  | [CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/tsc.json
act_1  | [CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/eslint-stylish.json
act_1  | [CI/test]   ❓ add-matcher /run/act/actions/actions-setup-node@v4/.github/eslint-compact.json
act_1  | [CI/test]   ✅  Success - Main actions/setup-node@v4 [28.274747217s]
act_1  | [CI/test]   ⚙  ::set-output:: node-version=
act_1  | [CI/test]   ⚙  ::add-path:: /opt/hostedtoolcache/node/18.20.8/x64/bin
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
...
act_1  | [CI/test] ⭐ Run Main npm install
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
...
act_1  | [CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/2.sh] user= workdir=
act_1  | [CI/test]   | npm warn deprecated inflight@1.0.6: This module is not supported, and leaks memory. Do not use it. Check out lru-cache if you want a good and tested way to coalesce async requests by a key value, which is much more comprehensive and powerful.
act_1  | [CI/test]   | npm warn deprecated rimraf@2.6.3: Rimraf versions prior to v4 are no longer supported
act_1  | [CI/test]   | npm warn deprecated glob@7.1.2: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
act_1  | [CI/test]   | npm warn deprecated glob@7.2.3: Old versions of glob are not supported, and contain widely publicized security vulnerabilities, which have been fixed in the current version. Please update. Support for old versions may be purchased (at exorbitant rates) by contacting i@izs.me
act_1  | [CI/test]   | npm warn deprecated mkdirp@0.5.1: Legacy versions of mkdirp are no longer supported. Please update to mkdirp 1.x. (Note that the API surface has changed to use Promises in 1.x.)
act_1  | [CI/test]   | npm warn deprecated superagent@8.1.2: Please upgrade to superagent v10.2.2+, see release notes at https://github.com/forwardemail/superagent/releases/tag/v10.2.2 - maintenance is supported by Forward Email @ https://forwardemail.net
act_1  | [CI/test]   | npm warn deprecated eslint@6.8.0: This version is no longer supported. Please see https://eslint.org/version-support for other options.
act_1  | [CI/test]   | 
act_1  | [CI/test]   | added 375 packages, and audited 376 packages in 49s
act_1  | [CI/test]   | 
act_1  | [CI/test]   | 104 packages are looking for funding
act_1  | [CI/test]   |   run `npm fund` for details
act_1  | [CI/test]   | 
act_1  | [CI/test]   | 12 vulnerabilities (3 low, 6 high, 3 critical)
act_1  | [CI/test]   | 
act_1  | [CI/test]   | To address all issues (including breaking changes), run:
act_1  | [CI/test]   |   npm audit fix --force
act_1  | [CI/test]   | 
act_1  | [CI/test]   | Run `npm audit` for details.
act_1  | [CI/test]   ✅  Success - Main npm install [49.043529443s]
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
...
act_1  | [CI/test] ⭐ Run Main npm test
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
...
act_1  | [CI/test]   🐳  docker exec cmd=[sh -e /var/run/act/workflow/3.sh] user= workdir=
act_1  | [CI/test]   | 
act_1  | [CI/test]   | > github-actions-demo@1.0.0 test
act_1  | [CI/test]   | > mocha ./tests --recursive
act_1  | [CI/test]   | 
act_1  | [CI/test]   | 
act_1  | [CI/test]   | 
act_1  | [CI/test]   |   GET /
act_1  | [CI/test]   |     ✓ should respond with hello world
act_1  | [CI/test]   | 
act_1  | [CI/test]   | 
act_1  | [CI/test]   |   1 passing (71ms)
act_1  | [CI/test]   | 
act_1  | [CI/test]   ✅  Success - Main npm test [1.147521229s]
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
...

act_1  | [CI/test] ⭐ Run Post actions/setup-node@v4
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
act_1  | [CI/test]   🐳  docker exec cmd=[/usr/local/bin/node /var/run/act/actions/actions-setup-node@v4/dist/cache-save/index.js] user= workdir=
act_1  | [CI/test]   ✅  Success - Post actions/setup-node@v4 [467.150968ms]
act_1  | [CI/test] ⭐ Run Complete job
act_1  | [CI/test] Cleaning up container for job test
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git ref: repository does not exist
act_1  | [CI/test] path/workspacenot located inside a git repository
act_1  | [CI/test] unable to get git revision: repository does not exist
act_1  | [CI/test]   ✅  Success - Complete job
act_1  | [CI/test] 🏁  Job succeeded
```
### Cleanup
```sh
docker-compose stop
docker-compose rm -f
docker container prune -f
docker image prune -f
docker image rm act
```

### Troubleshooting

```sh
WORKSPACE=$(pwd)/nodejs docker-compose up --build 
```
```text
Creating basic-act_act_1 ... done
Attaching to basic-act_act_1
act_1  | fetch https://dl-cdn.alpinelinux.org/alpine/v3.18/main/x86_64/APKINDEX.tar.gz
act_1  | fetch https://dl-cdn.alpinelinux.org/alpine/v3.18/community/x86_64/APKINDEX.tar.gz
act_1  | (1/18) Installing ncurses-terminfo-base (6.4_p20230506-r0)
act_1  | (2/18) Installing libncursesw (6.4_p20230506-r0)
act_1  | (3/18) Installing readline (8.2.1-r1)
act_1  | (4/18) Installing bash (5.2.15-r5)
act_1  | Executing bash-5.2.15-r5.post-install
act_1  | (5/18) Installing ca-certificates (20241121-r1)
act_1  | (6/18) Installing brotli-libs (1.0.9-r14)
act_1  | (7/18) Installing libunistring (1.1-r1)
act_1  | (8/18) Installing libidn2 (2.3.4-r1)
act_1  | (9/18) Installing nghttp2-libs (1.57.0-r0)
act_1  | (10/18) Installing libpsl (0.21.5-r0)
act_1  | (11/18) Installing libcurl (8.12.1-r0)
act_1  | (12/18) Installing curl (8.12.1-r0)
act_1  | (13/18) Installing libexpat (2.7.0-r0)
act_1  | (14/18) Installing pcre2 (10.42-r1)
act_1  | (15/18) Installing git (2.40.4-r0)
act_1  | (16/18) Installing libacl (2.3.1-r3)
act_1  | (17/18) Installing tar (1.34-r3)
act_1  | (18/18) Installing wget (1.21.4-r0)
act_1  | Executing busybox-1.36.1-r7.trigger
act_1  | Executing ca-certificates-20241121-r1.trigger
act_1  | OK: 22 MiB in 33 packages
act_1  | LICENSE
act_1  | README.md
act_1  | act
act_1  | install -m755 "/tmp/tmp.khNeCb/act" /usr/local/bin/act
act_1  | rm -rf "/tmp/tmp.khNeCb"
act_1  | rm: unrecognized option: p
act_1  | BusyBox v1.36.1 (2024-06-12 11:52:11 UTC) multi-call binary.
act_1  | 
act_1  | Usage: rm [-irf] FILE...
act_1  | 
act_1  | Remove (unlink) FILEs
act_1  | 
act_1  | 	-i	Always prompt before removing
act_1  | 	-f	Never prompt
act_1  | 	-R,-r	Recurse
Exception in thread Thread-4 (watch_events):
Traceback (most recent call last):
  File "/usr/lib/python3.10/threading.py", line 1016, in _bootstrap_inner
    self.run()
  File "/usr/lib/python3.10/threading.py", line 953, in run
    self._target(*self._args, **self._kwargs)
basic-act_act_1 exited with code 1
```

workaround - provide blank line after every command in `docker-compose.yml`.

```sh
WORKSPACE=$(pwd)/nodejs docker-compose up --build
```
```text
...
act_1  | [CI/test]   ✅  Success - Complete job
act_1  | [CI/test] 🏁  Job succeeded
basic-act_act_1 exited with code 0

Exception in thread Thread-4 (watch_events):
Traceback (most recent call last):
  File "/usr/lib/python3.10/threading.py", line 1016, in _bootstrap_inner
    self.run()
  File "/usr/lib/python3.10/threading.py", line 953, in run
    self._target(*self._args, **self._kwargs)
  File "/usr/lib/python3/dist-packages/compose/cli/log_printer.py", line 202, in watch_events
    for event in event_stream:
  File "/usr/lib/python3/dist-packages/compose/project.py", line 626, in yield_loop
    yield build_container_event(event)
  File "/usr/lib/python3/dist-packages/compose/project.py", line 594, in build_container_event
    container = Container.from_id(self.client, event['id'])
KeyError: 'id'

```
the workflow has already completed successfully.

This "error" is emitted by the legacy `docker-compose` v1 Python event watcher during container teardown. It is not an `act` failure.

Run the one-shot command form instead:
```sh
WORKSPACE=$(pwd)/nodejs docker-compose run --rm --build act
```
`run` is the appropriate `docker-compose` command for a disposable CLI tool such as `act`;
`up` is intended for long-running services

### See Also

   * [introduction to GitHub Actions with Docker](https://docs.docker.com/guides/gha/)
   * [curated list of awesome actions to use on GitHub](https://github.com/cplee/awesome-actions/tree/master#collection-of-actions)
   * Runners -
     + https://hub.docker.com/search?q=act-runner
     + https://hub.docker.com/search?q=act_runner
     + https://hub.docker.com/search?q=act
The Workflows that themselves invoke __Docker++ may require [Docker-in-Docker (DinD)](https://github.com/jpetazzo/dind) or access to the host Docker socket.
   * "recommended" for Java maven [gitea/act_runner](https://hub.docker.com/r/gitea/act_runner) - is a huge overkill, and there is *no* `act` inside `act_runner`
     + [gitea/act_runner](https://hub.docker.com/r/gitea/act_runner) - older (deprecated)
     + [gitea/runner](https://hub.docker.com/r/gitea/runner) - newer

   * Emmanuel Frecon [repositories](https://github.com/efrecon?tab=repositories)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
