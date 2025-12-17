### Info
this directory contains experiment with using [dive](https://github.com/wagoodman/dive) tool for exploring each layer in.

### Notes

* BuildKit is a bit of a *geeky DevOps-only novelty* in the sense that its improvements are practically invisible to casual users, but very meaningful to "serious CI/CD practitioners" claiming: __BuildKit__ is *more advanced* than the classic Docker builder. Classic Docker build is essentially *legacy* now

* Classic Docker build (legacy)
+ Linear
+ Layer-by-layer
+ Each step runs sequentially
+ Weak cache model
+ Minimal metadata
* BuildKit "innovations" around *x`Compute a dependency graph, execute only what’s needed*
+ DAG-based build graph
+ Steps can run in parallel
+ Content-addressable filesystem
+  Strong cache semantics
+ OCI-native


Exactly — that’s a very accurate way to put it.

The Docker community and ecosystem (especially post-2017, with BuildKit, Buildx, multi-stage builds, cache mounts, parallel execution, etc.) focused almost entirely on developer and CI/CD productivity:

* Faster builds
* Smaller images
* Parallelism
* Reproducible layers
* Secrets and ephemeral mounts
* Multi-platform builds


…but security scanning, vulnerability management, and compliance auditing were deliberately left out of scope.

A few points to underline why:

1. Philosophical separation
Docker’s goal has always been: “efficient, reproducible container builds”.
Security scanning is a consuming or post-processing activity, not part of the build definition.
That’s why there’s no “S” in Docker — security scanning is conceptually orthogonal.


2. Ecosystem approach
Security tooling became a separate layer, handled by projects like:

* Trivy, Grype, Clair, Anchore (for vulnerability scanning)
* Syft, CycloneDX (for SBOM generation)
* Cosign, Notary (for image signing)


Docker intentionally left this modular, rather than bake it into the Dockerfile syntax.


3. Practical implications

Modern Dockerfiles and BuildKit features do nothing to ensure the vendor JARs you pull are safe.

BuildKit improves efficiency and reproducibility, but not content trust.



4. Fintech / compliance perspective
Anyone in regulated environments quickly learns:

You cannot trust Docker builds alone

You need post-build inspection, SBOMs, CVE scanning, and policy enforcement


### See Also:

  * `dive` [image](https://hub.docker.com/r/wagoodman/dive) and standalone [releases](https://github.com/wagoodman/dive/releases). NOTE: the very last release my not yet be available for Windows. The image is created via [buildkit](https://docs.docker.com/build/buildkit/)

  * Docker has become and [article](https://habr.com/ru/companies/ruvds/articles/975114/) (in Russian)

  * Slim Toolkit [repository](https://github.com/slimtoolkit/slim) (former DockerSlim)
  * __BuildKit__ on __Kubernetes__ [manual](https://habr.com/ru/companies/kts/articles/960922/)(in Russian)
  * https://docs.docker.com/build/buildkit/
  * https://docs.docker.com/build/builders

### Notes, Continued

You have:

A vendor‑supplied Docker image

Inside it: a Java application stack (fat JAR / Spring Boot JAR)

Running in:

SaaS‑style Kubernetes

Multi‑tenant or controlled client environment

Fintech / regulated context


You need to:

Inspect

Dive into

Security scan

Possibly prove compliance



And you cannot:

Rebuild the image

Modify the vendor’s Dockerfile

Trust opaque “we scanned it” PDFs


This is extremely common in banks and payment processors.


---

2. What does NOT work (common misconceptions)

❌ Relying on:

docker history (too coarse)

Image signature alone (cosign ≠ content safety)

CVE scan of base OS only

Vendor attestation without evidence

Kubernetes runtime isolation alone


❌ Assuming:

“Fat JAR = one thing”

“Java deps are scanned at build time by vendor”



---

3. The real industry solutions (layered)

Layer 1: Image-level vulnerability scanning

This is table stakes, not sufficient alone.

Tools:

Trivy

Grype

Clair

Anchore


What they do:

Scan:

OS packages

Known JARs in standard locations

Manifest metadata



Problem:

Fat JARs often contain nested JARs

Shaded dependencies may not be detected

No visibility into what Java libs are actually loaded


Result:

> “Medium confidence” at best




---

Layer 2: SBOM extraction (this is key)

This is where the industry shifted in the last 2–3 years.

Software Bill of Materials (SBOM):

Enumerates all components

Including Java dependencies


Formats:

SPDX

CycloneDX (very common for Java)


How it’s done for vendor images:

Option A — Vendor-provided SBOM (preferred)

Generated at build time

Signed

Tied to image digest


But:

Often incomplete

Sometimes missing shaded deps


Option B — Post‑hoc SBOM extraction (your case)

Tools that can extract SBOM from an image:

Syft (Anchore)

Trivy SBOM

Docker Scout


These tools:

Crack open the image layers

Detect:

JARs

pom.properties

MANIFEST.MF

Nested dependencies



This is how banks do it when vendors don’t cooperate fully.


---

Layer 3: Java‑aware deep inspection

For Java specifically, serious orgs go further:

a) Explode the fat JAR

Inside a controlled environment:

jar tf app.jar

Or:

java -Djarmode=layertools -jar app.jar extract

This exposes:

Dependency layers

Third‑party JARs

Spring Boot layout


Then:

Run dependency scanners on the exploded contents


This is often done:

In CI

In a quarantined scanning namespace

Not in production clusters



---

Layer 4: Runtime dependency verification

Because static scans lie.

Tools:

eBPF-based runtime scanners

Java agents

Sidecars


They observe:

Which classes are actually loaded

Which JARs are touched

Which CVEs are reachable


This is how fintechs justify:

> “Present but not exploitable”




