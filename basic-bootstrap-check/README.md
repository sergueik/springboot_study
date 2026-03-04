### Info

This project demonstrates how to validate the Maven Wrapper during initial project default branch setup by developer. The `mvnw*`, and `.mvn/wrapper` are sometimes incomplete in a local or enterprise setup leading to
Validation including confirming the successful downloader `MavenWrapperDownloader.java` execution, enterprise-specific `distribution URL` configuration in the `.mvn/wrapper/maven-wrapper.properties`, and proper artifact downloading into `~/m2/wrapper/dists/apache-maven-*/` and. `~/m2/wrapper/dists/apache-maven-*-bin`.
### Usage
There are few (four) main distribution types defined in Apache Maven Wrapper plugin.

Specifically the command
```cmd
mvn wrapper:wrapper -Dtype=source
```
produces the files:

```text
MavenWrapperDownloader.java
maven-wrapper.properties
```
while the default run without specifying any distribution type produces the files:

```text
maven-wrapper.jar
maven-wrapper.properties
```
The choice is based on binary check in policies.

This project contains scripts which are useful for hunting unfinished or partially configured "enterprise" Maven setups.
> Note: The `.gitignore` should allow check-in of `.mvn/wrapper/maven-wrapper.jar` but ignore local Maven caches (like `~/.m2`).

`mvnw` / `mvnw.cmd` are the executable wrappers for bash / Windows.

#### Maven Wrapper Components

`mvnw` / `mvnw.cmd` – Scripts to launch Maven via the wrapper.

`.mvn/wrapper/maven-wrapper.properties` – Configuration, contains:

`distributionUrl=https://repo.maven.apache.org/maven2/org/apache/maven/apache-maven/3.9.2/apache-maven-3.9.2-bin.zip`

This URL can be replaced by an enterprise mirror.

Validate that `distributionUrl` points to a reachable location (even if it’s a placeholder).

`.mvn/wrapper/maven-wrapper.jar` – The actual Maven Wrapper Java downloader.

#### Validation Scenarios
**Scenario A** – Fresh download

Start clean: Remove any existing Maven distribution in `~/.m2/wrapper/dists/`.
```sh
rm -rf ~/.m2/wrapper/dists/
```
##### Run the wrapper:
```sh
./mvnw -v        # or mvnw.cmd on Windows
```
the output will clearly show the version and home directory:
```text
Apache Maven 3.6.1 (d66c9c0b3152b2e69ee9bac180bb8fcc8e6af555; 2019-04-04T15:00:29-04:00)
Maven home: C:\Users\kouzm\.m2\wrapper\dists\apache-maven-3.6.1-bin\ced06cd4\apache-maven-3.6.1
Java version: 11.0.12, vendor: BellSoft, runtime: c:\java\jdk-11.0.12
Default locale: en_US, platform encoding: Cp1252
OS name: "windows 10", version: "10.0", arch: "amd64", family: "windows"

```
NOTE: the Windows path format will be observed in the `mvnw` output when run from Git Bash on Windows

```text
Apache Maven 3.6.1 (d66c9c0b3152b2e69ee9bac180bb8fcc8e6af555; 2019-04-04T15:00:29-04:00)
Maven home: C:\Users\kouzm\.m2\wrapper\dists\apache-maven-3.6.1-bin\ced06cd4\apache-maven-3.6.1
Java version: 11.0.12, vendor: BellSoft, runtime: C:\java\jdk-11.0.12
Default locale: en_US, platform encoding: Cp1252
OS name: "windows 10", version: "10.0", arch: "amd64", family: "windows"
```
>NOTE: mvnw.cmd does not recognize the *dash* options very well.

> NOTE mvnw behavior may be slghtly different if the file `.mvn/jvm.config` is detected. We do not try to explore it here.

Expected Results: the wrapper dists dir `.m2/wrapper/dists/apache-maven-*/` is created.

ZIP distribution is unpacked.

Maven version printed matches distribution in `maven-wrapper.properties`.

##### Inspect contents:

Check for `bin/mvn` (or `bin/mvn.cmd` on Windows)

`lib/maven-*.jar` files

`BOOT-INF/classes` or similar

##### Cleanup:

Remove `.m2/wrapper/dists/apache-maven-*`

Re-run to ensure reproducibility.

**Scenario B** – Pre-existing `maven-wrapper.jar`

If `.mvn/wrapper/maven-wrapper.jar` already exists:

Confirm its checksum matches expected release (optional)
Run
```sh
./mvnw -v
```
Observe whether it downloads the Maven distribution or uses a cached copy

Purge `~/.m2/wrapper/dists/` to simulate a pristine environment

##### Checks:

Wrapper runs without errors.

Distribution is unpacked correctly.

No unexpected files appear (e.g., leftover temp directories).

#### Scenario C – Enterprise mirror placeholder

Change `distributionUrl` to a non-existent internal mirror:

`distributionUrl=https://internal-mirror.company.com/maven/apache-maven-3.9.2-bin`
Run
```
./mvnw -v
```
or  
```cmd
mvnw.cmd -v
```

Expected Outcome:

Fails gracefully if mirror server is unreachable.

Error messages indicate which file is missing (helps detect “unfinished enterprise setup”)

`Git` & `.gitignore` Tips /sample file fragment:

```
# Ignore local Maven wrapper caches
.m2/wrapper/dists/

# Track wrapper jar and properties
!.mvn/wrapper/maven-wrapper.jar
!.mvn/wrapper/maven-wrapper.properties

# Wrapper scripts
!/mvnw
!/mvnw.cmd
```

> NOTE: when testing Maven Wrapper in an environment that already has a system Maven (mvn) installed, you want to shadow or isolate the wrapper run so that you’re truly testing what it downloads and runs, not the pre-installed Maven.
### Summary

This project detects:

stale internal mirrors

wrapper JAR accidentally removed

JRE-only environments

broken bootstrap logic

“works on my machine” Maven installs

It ensures Maven Wrapper is:

reproducible

portable

SDK-correct

mirror-independent (or mirror-validated)

A broken Maven Wrapper bootstrap causes damage far beyond a local developer inconvenience.

If the wrapper cannot start (missing maven-wrapper.jar, broken MavenWrapperDownloader.java, invalid distributionUrl, missing JDK), then:

The build never reaches compile

The build never reaches test

The build never reaches package

The build never reaches deploy

Which means:

CI/CD never executes the code health gate

his is more harmful than a failing unit test, because:

| Failure Type	    | Impact                                 |
| ----------------- | -------------------------------------- |
| Test failure      | Code quality is evaluated and rejected |
| Bootstrap failure	| Code quality is never evaluated at al  |




A broken bootstrap silently disables:

 * unit tests
 * static analysis
 * security scans
 * packaging validation
 * deployment verification

The pipeline becomes a no-op.
When bootstrap is broken, teams may see:

 * "Pipeline failed" without meaningful diagnostics
 * Developers bypassing CI with local mvn install
 * Hotfixes merged without tests
 * Gradual erosion of trust in CI/CD
 * This leads to: **Code health is no longer guarded by automation**.
### NOTE

**Defensive programming first**: assume things **will** fail; celebrate when they **don’t**.

**TDD first**: define "done” by **passing tests**, either at runtime or in automated test runs — **not** before.

Ask for **reasoning**: when advice is **good** or **bad**, ask **why**, **read** explanations, and **take notes**.

**Razor principle**: implement the **minimal possible** change to **deliver a feature**. Keep it human-friendly—avoid outputs that look like a **Base64 dump**.

### See Also
  * [apache maven wrapper](https://maven.apache.org/tools/wrapper/)
  * [usage](https://maven.apache.org/tools/wrapper/maven-wrapper-plugin/usage.html)

### Solution

![Flow](https://github.com/sergueik/springboot_study/blob/master/basic-bootstrap-check/screenshots/flow.png)


By applying the same discipline used in multi-stage Dockerfile optimization, we can significantly improve the reliability and predictability of our GitHub Actions workflows.

Separating dependency synchronization from build, test, and deploy phases transforms the pipeline into a controlled, deterministic process. Network-sensitive steps are isolated and time-bounded, while compute-only steps remain fast and stable.

This approach reduces unexpected CI failures, shortens feedback loops, and increases confidence in every commit. In effect, we are reusing proven container optimization principles to make our CI pipeline more resilient, more transparent, and easier to operate at scale.

Outcome: higher build success rates, clearer diagnostics, and a workflow architecture that reflects engineering best practices rather than environmental luck.

```yaml
# GitHub Actions: Step-Level Timeout for Maven Dependency Sync

GitHub Actions allows imposing a timeout on an individual step using:

    timeout-minutes:

This is supported natively and documented behavior.
It prevents long-hanging operations (for example Maven downloads behind a firewall)
from blocking the entire CI job for up to 1 hour.

This pattern isolates the risky network phase from build/test/deploy.

---

## Pipeline Design

Phases:

1. Copy / inspect raw pom.xml
2. Sync dependencies (with strict timeout)
3. Build (offline)
4. Test (offline)
5. Deploy (offline)

Only step #2 is allowed to fail quickly.

---

## Example Workflow (copyable)

```yaml
name: Maven CI with Step Timeout

on:
  push:
    branches: [ main ]
  pull_request:

jobs:
  build:
    runs-on: ubuntu-latest
    timeout-minutes: 60   # global safety net

    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      # Step 1: Copy / inspect POM only
      - name: Show pom.xml
        run: |
          echo "==== pom.xml ===="
          cat pom.xml

      # Step 2: Dependency sync with strict timeout
      - name: Sync Maven dependencies (timeout protected)
        timeout-minutes: 7
        env:
          MAVEN_OPTS: >
            -Dmaven.wagon.http.retryHandler.count=2
            -Dmaven.wagon.http.timeout=20000
            -Dmaven.wagon.httpconnectionManager.ttlSeconds=30
        run: |
          echo "Prefetching dependencies..."
          mvn -B dependency:go-offline -U

      # Step 3: Build offline
      - name: Build (offline)
        run: |
          mvn -B clean compile -o

      # Step 4: Test offline
      - name: Test (offline)
        run: |
          mvn -B test -o

      # Step 5: Package / Deploy offline
      - name: Package
        run: |
          mvn -B package -o
```
---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
