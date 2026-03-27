# End-to-End Example: Git + CI/CD + Terraform + RBAC (Regulated Environment)

## 1. Code Lifecycle (Git)

feature/*  → short-lived development branches  
develop    → integration branch  
release/*  → stabilization / UAT  
main       → production-ready code  

Rules:
- No direct commits to `main`
- Pull Requests required
- Approvals required (e.g., QA + Release Manager)
- Full audit trail (commit → ticket → deployment)

---

## 2. CI/CD Pipeline Flow

1. Developer pushes to `feature/*`
   → CI builds + unit tests
   → deploys to DEV environment

2. Merge `feature/*` → `develop`
   → CI builds + integration tests
   → deploys to STAGING

3. Create `release/*`
   → CI runs regression / UAT
   → deploys to PRE-PROD

4. Merge `release/*` → `main`
   → CI builds signed artifact
   → deploys to PROD (approval required)

---

## 3. Infrastructure Mapping (Terraform)

Git Branch        → Terraform Workspace      → Environment
-----------------------------------------------------------
feature/*         → dev                      → DEV
develop           → staging                  → STAGING
release/*         → preprod                  → PRE-PROD
main              → prod                     → PROD

Notes:
- Terraform workspace = isolated state
- Same code, different variables (e.g., instance size, endpoints, secrets)
- State backends strictly separated (e.g., different S3 buckets or backends)

---

## 4. Access Control (RBAC)

Role              Code Access        Deploy Rights        Data Access
-----------------------------------------------------------------------
Developer         feature/*          DEV only             fake/redacted
QA                develop/release    STAGING/PRE-PROD     masked data
Release Manager   all branches       PROD (approval)      limited
Ops               read-only          PROD execution       no raw data

Key Principles:
- Separation of Duties (SoD)
- No single actor controls code + deploy + data
- All actions logged and auditable

---

## 5. Data Separation

Environment   Data Type
------------------------
DEV           synthetic / fake
STAGING       masked production-like
PRE-PROD      near-real (controlled)
PROD          real data (restricted)

---

## 6. CRITICAL NETWORK NOTE (Often Overlooked)

Choice of **branch/environment is ALSO a choice of network boundary**:

Environment   Network Scope
----------------------------
DEV           open / developer subnet
STAGING       restricted subnet
PRE-PROD      tightly controlled subnet
PROD          isolated subnet (no direct access)

Implications:
- Each environment maps to **different subnets**
- Firewall rules differ per environment
- Access paths (VPN, bastion hosts, service endpoints) are enforced per stage

⚠️ This is NOT optional:
- You cannot treat environments as logical-only separation
- Network isolation (subnets, routing, firewall rules) is part of the security model
- Deployment pipelines must honor these constraints

Example:
- DEV → public endpoints allowed
- STAGING → internal access only
- PROD → access via bastion / zero-trust / service mesh only

---

## 7. STRICT ACCESS SEPARATION NOTE ("dura lex, sed lex")

Even when it looks inefficient or "ridiculous", strict separation applies:

- A **Developer can commit code but cannot deploy it**
- A **Deployer (CI/CD or Ops) can deploy but should not modify code**
- A **Reviewer/Approver can approve but not bypass process controls**

This applies **even to DEV environments** in many regulated setups.

Implications:
- No "just let me deploy my fix quickly" shortcuts
- No shared credentials between roles
- No bypassing CI/CD with manual deployments
- Permissions are intentionally restrictive and sometimes inconvenient

"dura lex, sed lex" → *the law is harsh, but it is the law*

The goal is:
- Prevent accidental or malicious changes
- Ensure traceability and accountability
- Enforce consistent, repeatable delivery

---

## 8. Mental Model Summary

Three aligned but separate axes:

1. Code Lifecycle      → Git branches
2. Deployment Flow     → CI/CD pipeline stages
3. Security Boundaries → RBAC + Data + Network (subnets/firewalls)

These must be:
- Explicitly mapped
- Consistently enforced
- Auditable end-to-end
