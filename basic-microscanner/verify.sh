#!/usr/bin/env bash
set -euo pipefail

IMAGE="${1:-}"
JAR_PATH_IN_IMAGE="${2:-/app/app.jar}"
TRUSTED_CA_CN="${3:-CN=MyTrustedCA}"

if [[ -z "$IMAGE" ]]; then
  echo "Usage:"
  echo "  $0 <image[:tag]> [jar-path] [trusted-ca-cn]"
  exit 2
fi

cleanup() { 
if [ -z "$MSYSTEM_PREFIX" ] ;then 
	rm -rf "$WORKDIR";
fi

}
# NOTE: mktemp will likely fail on Windows MinGW
if [ -z "$MSYSTEM_PREFIX" ] ;then 
  WORKDIR="$(mktemp -d)"
  trap cleanup EXIT
else
  WORKDIR='.'
fi

echo "▶ Image: $IMAGE"
echo "▶ JAR path: $JAR_PATH_IN_IMAGE"
echo "▶ Trusted CA: $TRUSTED_CA_CN"
echo

########################################
# 1. Download Trivy (if missing)
########################################
if ! command -v trivy >/dev/null 2>&1; then
  echo "▶ Downloading Trivy..."
  curl -sfL https://raw.githubusercontent.com/aquasecurity/trivy/main/contrib/install.sh \
    | sh -s -- -b "$WORKDIR"
  export PATH="$WORKDIR:$PATH"
fi

########################################
# 2. Trivy scan (default intelligence)
########################################
echo "▶ Running Trivy scan..."
trivy image \
  --quiet \
  --severity HIGH,CRITICAL \
  "$IMAGE" || {
    echo "✖ Trivy found high/critical vulnerabilities"
    exit 10
  }

echo "✔ Trivy scan passed"
echo

########################################
# 3. Create container WITHOUT running
########################################
echo "▶ Creating container (no execution)..."
CID=$(docker create "$IMAGE")
trap "docker rm -f $CID >/dev/null 2>&1 || true; cleanup" EXIT

########################################
# 4. Copy JAR out of image
########################################
echo "▶ Extracting JAR from image..."
docker cp "$CID:$JAR_PATH_IN_IMAGE" "$WORKDIR/app.jar" || {
  echo "✖ Failed to copy JAR from image"
  exit 20
}

########################################
# 5. Verify JAR signature (CORRECT)
########################################
echo "▶ Verifying JAR signature..."

jarsigner -verify -verbose -certs "$WORKDIR/app.jar" >"$WORKDIR/jar.sig" 2>&1 || true

# Case 1: Unsigned JAR
if grep -qi "jar is unsigned" "$WORKDIR/jar.sig"; then
  echo "✖ JAR is NOT signed"
  exit 30
fi

# Case 2: Signature verification failed
if grep -qiE "invalid signature|has been tampered" "$WORKDIR/jar.sig"; then
  echo "✖ JAR signature is INVALID"
  exit 31
fi

# Case 3: Signed but untrusted CA
if ! grep -q "$TRUSTED_CA_CN" "$WORKDIR/jar.sig"; then
  echo "✖ JAR signed but NOT by trusted authority: $TRUSTED_CA_CN"
  exit 32
fi

# Case 4: All good
if grep -qi "jar verified" "$WORKDIR/jar.sig"; then
  echo "✔ JAR signature valid and trusted"
else
  echo "✖ Unknown jarsigner state"
  exit 33
fi

echo "✔ JAR signature valid and trusted"
echo
echo "✅ IMAGE PASSED ALL CHECKS"

