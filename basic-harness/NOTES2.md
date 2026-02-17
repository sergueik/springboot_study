## Tenant Artifact Vetting and Deployment Model (SaaS Vendor Images)

## 1. Purpose

This document defines how a tenant can accept, verify, and deploy Docker
images produced by a SaaS vendor into tenant-controlled Kubernetes
environments.

The tenant does not own the software build process and receives only
container images.\
The tenant's objective is limited and pragmatic:

- ensure images pass AquaSec (or equivalent) security vetting
- store approved images in the tenant artifact repository
- optionally apply simple, reliable quality indicators on JARs inside
  the container
- deploy only vetted images to Kubernetes

This is a **custom flow**, not a standard SaaS vendor pipeline.

## 2. Responsibility Split

### 2.1 SaaS Vendor Responsibilities

The SaaS vendor is responsible for:

- source code management
- build and packaging pipeline
- automated testing
- dependency and vulnerability scanning (vendor internal)
- Docker image creation
- publishing images
- internal verification and deployment in the vendor environment

These processes are opaque to the tenant and cannot be reproduced by the
tenant.

### 2.2 Tenant Responsibilities

The tenant is responsible for:

- owning the runtime Kubernetes environment
- owning the container registry / artifact repository
- independently scanning vendor images
- deciding whether images are acceptable for deployment
- enforcing deployment policy

The tenant does not rebuild or modify vendor software.

## 3. Management Question

> Is verification and deployment already done by the SaaS vendor, and
> can the tenant perform the same process?

**Answer:**

- Yes, the SaaS vendor performs its own verification and deployment
  internally.
- No, that process is not visible, auditable, or reproducible by the
  tenant.
- The tenant can only perform **post-delivery artifact vetting**.

This is a different and weaker assurance model than owning the full
CI/CD chain.

## 4. Tenant Acceptance Criteria (Practical Goal)

The tenant is satisfied if:

1.  Vendor image is scanned by AquaSec (or equivalent)

2.  Image passes security policy

3.  Image is stored in the tenant artifact repository

4.  Optional lightweight checks are applied to JARs inside the
    container, such as:

    - signature verification
    - checksum validation
    - basic static scan
    - dependency listing

These checks are comparable to simple GitHub Actions / Jenkins / GitLab
CI steps.

## 5. "Difficult but Yes"

The following is technically possible with a custom pipeline (for
example using Harness or similar tools):

- pull vendor Docker images
- scan images with AquaSec
- reject images that fail policy
- extract JARs from the container
- apply simple quality indicators (signature, hash, basic scan)
- push approved images into the tenant artifact repository
- deploy only approved images to Kubernetes

This achieves the tenant's stated goal:\
**"Only AquaSec-approved images enter our environment."**

However:

- this is not provided out of the box
- it must be engineered as a custom pipeline
- it requires security and platform integration work

## 6. "No, Not Really"

The tenant cannot realistically:

- reproduce the SaaS vendor's full build and verification pipeline
- validate source-level security
- rebuild identical binaries
- guarantee equivalence between vendor source and delivered image
- gain the same assurance as owning the entire CI/CD chain
- easily model this flow using unsupported or community tooling

This remains artifact vetting, not full supply-chain control.

## 7. Tooling Reality

The tenant team:

- is experienced with GitHub and basic CI pipelines
- has no experience with Harness

Implications:

- conceptually, this flow is similar to a GitHub Actions or Jenkins job
- operationally, learning Harness adds complexity
- unsupported or Community Edition tooling makes this significantly
  harder

This is not a "quick proof of concept" task.

## 8. Bonus Question: Can the tenant learn and replicate the vendor process?

**In theory:**\
The tenant can learn how to:

- scan images
- verify signatures
- enforce deployment policies

**In practice:**\
The tenant cannot replicate the SaaS vendor's internal verification and
deployment process because:

- source code is unavailable
- build inputs are unavailable
- signing keys are vendor-controlled
- security rules are proprietary

The tenant can only approximate trust using:

- AquaSec scans
- artifact repository controls
- simple quality indicators

## 9. Conclusion

- The SaaS vendor already performs build, verification, and deployment
  internally.

- That process cannot be reused or reproduced by the tenant.

- The tenant can implement a **custom artifact vetting pipeline**:

  - AquaSec scan
  - optional JAR checks
  - artifact repository storage
  - controlled deployment

This is:

- **Difficult but yes**: artifact vetting and controlled deployment
- **No, not really**: reproducing the vendor's full CI/CD and security
  process

This model provides reasonable operational assurance but does not
replace end-to-end supply chain control.
