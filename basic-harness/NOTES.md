## README.md  
### Tenant Artifact Vetting and Deployment Model (SaaS Vendor Images)

---

### 1. Purpose

This document defines how a tenant can accept, verify, and deploy Docker images produced by a SaaS vendor into tenant-controlled Kubernetes environments.

The tenant does not own the software build process and receives only container images.  
The tenant’s objective is pragmatic and limited:

- ensure images pass AquaSec (or equivalent) security vetting  
- store approved images in the tenant artifact repository  
- optionally apply simple, reliable quality indicators on JARs inside the container  
- deploy only vetted images to Kubernetes  

This is a **custom flow**, not a default SaaS vendor pipeline.

---

### 2. Trust Model

A strict trust boundary exists between SaaS vendor and tenant.

Vendor images are treated as **third-party binaries**, not as trusted internal build outputs.

The tenant does not rely solely on vendor assurances and performs independent artifact checks before runtime.

---

### 3. Responsibility Split

#### 3.1 SaaS Vendor Responsibilities

The SaaS vendor is responsible for:

- source code management  
- build and packaging pipeline  
- automated testing  
- dependency and vulnerability scanning (vendor internal)  
- Docker image creation  
- image signing (if implemented)  
- publishing images  
- internal verification and deployment in the vendor environment  

These processes are opaque to the tenant and cannot be reproduced by the tenant.

---

#### 3.2 Tenant Responsibilities

The tenant is responsible for:

- owning the runtime Kubernetes environment (on-prem or private cloud)  
- owning the container registry / artifact repository  
- independently scanning vendor images (AquaSec or equivalent)  
- optionally inspecting JARs inside containers  
- deciding whether images are acceptable for deployment  
- enforcing deployment policy  

The tenant does not rebuild or modify vendor software.

---

### 4. Management Question

> Is verification and deployment already done by the SaaS vendor, and can the tenant perform the same process?

**Answer:**

- Yes, the SaaS vendor performs its own verification and deployment internally.  
- No, that process is not visible, auditable, or reproducible by the tenant.  
- The tenant can only perform **post-delivery artifact vetting**.

This is a different and weaker assurance model than owning the full CI/CD chain.

---

### 5. Tenant Acceptance Criteria (Practical Goal)

The tenant is satisfied if:

1. Vendor image is scanned by AquaSec (or equivalent)  
2. Image passes tenant security policy  
3. Image is stored in the tenant artifact repository  
4. Optional lightweight checks are applied to JARs inside the container, such as:
   - signature verification  
   - checksum validation  
   - basic static scan  
   - dependency inventory  

These checks are comparable to simple GitHub Actions / Jenkins / GitLab CI steps.

---

### 6. Core Management Question (New)

> Can the SaaS vendor run an equivalent AquaSec scan and directly release images into the tenant’s non-default artifact repository (connector, private registry, etc.) as part of the vendor build/release process?

#### Short Answer

**Difficult but yes (technically).**  
**No, not really (from a governance and assurance perspective).**

---

#### 6.1 Difficult but Yes (Technically Possible)

The SaaS vendor can:

- integrate AquaSec (or similar) into its own build pipeline  
- configure a connector to the tenant’s artifact repository  
- push images directly into the tenant registry after passing vendor-side scans  
- optionally sign images before publishing  

From a tooling perspective (for example using :contentReference[oaicite:0]{index=0} or similar CI systems), this is feasible.

This would look like:

```yaml
Vendor build → Vendor security scan → Push image to tenant repo
```

This can reduce duplication and speed up delivery.

---

#### 6.2 No, Not Really (From Tenant Assurance View)

Even if the vendor performs AquaSec scanning and publishes directly into the tenant repository:

- the scan is still executed under vendor control
- the tenant cannot verify scan configuration or policy rigor
- results are not independently reproducible
- audit and compliance remain vendor-dependent
- trust boundary is not reduced

Therefore, this does **not** replace tenant-side vetting.

From the tenant’s governance perspective, this remains equivalent to:
> “The vendor says the image is safe.”

Which is insufficient for regulated or risk-aware environments.

---

### 7. “Difficult but Yes”

The tenant can implement a custom pipeline that:

- pulls vendor Docker images
- scans them with AquaSec
- rejects images that fail policy
- optionally extracts JARs from containers
- applies simple quality indicators (signature, hash, basic scan)
- pushes approved images into the tenant artifact repository
- deploys only approved images to Kubernetes

This achieves the tenant’s stated goal:

> **Only AquaSec-approved images enter our environment.**

However:

- this is not provided out of the box
- it must be engineered as a custom pipeline
- it requires security and platform integration work

---

### 8. “No, Not Really”

The tenant cannot realistically:

- reproduce the SaaS vendor’s full build and verification pipeline
- validate source-level security
- rebuild identical binaries
- guarantee equivalence between vendor source and delivered image
- achieve the same assurance as owning the entire CI/CD chain
- easily model this using unsupported or community tooling

This remains **artifact vetting**, not full supply-chain control.

---

### 9. Tooling Reality

The tenant team:

- is experienced with GitHub and simple CI pipelines
- has no experience with Harness

Implications:

- conceptually, this flow resembles GitHub Actions or Jenkins jobs
- operationally, learning Harness adds complexity
- unsupported or community tooling significantly increases risk and effort

This is not a quick proof-of-concept exercise.

---

### 10. Bonus Question: Can the tenant learn and replicate the vendor process?

**In theory:**
The tenant can learn how to:
- scan images
- verify signatures
- enforce deployment policies

**In practice:**
The tenant cannot replicate the SaaS vendor’s internal verification and deployment process because:

- source code is unavailable
- build inputs are unavailable
- signing keys are vendor-controlled
- security rules are proprietary

The tenant can only approximate trust using:
- AquaSec scans
- artifact repository controls
- simple quality indicators

---

### 11. Conclusion

- The SaaS vendor already performs build, verification, and deployment internally.
- That process cannot be reused or reproduced by the tenant.
- The tenant can implement a **custom artifact vetting pipeline**:
  - AquaSec scan
  - optional JAR checks
  - artifact repository storage
  - controlled deployment

Regarding the key management question:

- **Difficult but yes:** the vendor can scan and publish directly to the tenant repository.
- **No, not really:** this does not replace tenant-side verification or reduce trust dependency.

This model provides reasonable operational assurance but does not replace end-to-end supply-chain control.

