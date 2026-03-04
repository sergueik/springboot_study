# GitHub Actions Maven Hang (Firewall / Maven Central) — Cure Variants

Problem:
Maven can hang indefinitely when dependency downloads are blocked by a firewall or proxy.
GitHub Actions will only terminate the job when its job timeout is reached (often 60 minutes).

Solution:
Fail fast and explicitly by:
1) Defining a job-level timeout.
2) Performing a preflight network check to Maven Central.
3) Using clear, self-documenting variable names.
This avoids reliance on hidden runner settings.xml files or Maven internal timeout flags
and makes CI behavior deterministic and auditable.

--------------------------------------------------------------------
Variant 1 — Job-level hard timeout (simple & best practice)
--------------------------------------------------------------------

jobs:
  build:
    runs-on: ubuntu-latest

    # CI guardrail: prevent hanging forever when Maven Central is blocked by firewall
    timeout-minutes: 10

    steps:
      - uses: actions/checkout@v4
      - name: Build with Maven
        run: mvn -B clean verify

--------------------------------------------------------------------
Variant 2 — Self-documenting variable + network probe (recommended)
--------------------------------------------------------------------

jobs:
  build:
    runs-on: ubuntu-latest
    timeout-minutes: 10

    env:
      FAIL_FAST_IF_MAVEN_CENTRAL_BLOCKED_MINUTES: 2

    steps:
      - uses: actions/checkout@v4

      - name: Check access to Maven Central (firewall safety net)
        run: |
          echo "Checking access to Maven Central (max ${FAIL_FAST_IF_MAVEN_CENTRAL_BLOCKED_MINUTES} minutes)..."
          curl -I --max-time $((FAIL_FAST_IF_MAVEN_CENTRAL_BLOCKED_MINUTES * 60)) \
            https://repo.maven.apache.org/maven2/ || exit 1

      - name: Build with Maven
        run: mvn -B clean verify

--------------------------------------------------------------------
Variant 3 — Extremely explicit naming (policy as code)
--------------------------------------------------------------------

jobs:
  build:
    runs-on: ubuntu-latest
    timeout-minutes: 10  # Prevent 1h hang on dependency download behind firewall

    env:
      CI_NETWORK_GUARD_MAVEN_CENTRAL_TIMEOUT_MINUTES: 2

    steps:
      - uses: actions/checkout@v4

      - name: CI Network Guard: Maven Central must be reachable
        run: |
          echo "CI guard: checking Maven Central connectivity..."
          curl -I --max-time $((CI_NETWORK_GUARD_MAVEN_CENTRAL_TIMEOUT_MINUTES * 60)) \
            https://repo.maven.apache.org/maven2/

      - name: Maven build
        run: mvn -B clean verify

--------------------------------------------------------------------
Variant 4 — Linux timeout wrapper (no curl, no Maven flags)
--------------------------------------------------------------------

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4
      - name: Maven build (fail fast if network stalls)
        run: timeout 5m mvn -B clean verify

--------------------------------------------------------------------
Variant 5 — Combined hardened pattern (canonical recommendation)
--------------------------------------------------------------------

jobs:
  build:
    runs-on: ubuntu-latest
    timeout-minutes: 10

    env:
      FAIL_FAST_IF_MAVEN_CENTRAL_BLOCKED_MINUTES: 2

    steps:
      - uses: actions/checkout@v4

      - name: Network preflight check
        run: |
          echo "Checking Maven Central reachability..."
          curl -I --max-time $((FAIL_FAST_IF_MAVEN_CENTRAL_BLOCKED_MINUTES * 60)) \
            https://repo.maven.apache.org/maven2/

      - name: Maven build (protected from infinite hang)
        run: mvn -B clean verify
