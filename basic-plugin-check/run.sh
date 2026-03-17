run.cmd
[OK] Maven Help Plugin appears to be downloaded.`
#!/usr/bin/env bash

# -----------------------------
# Minimal Maven plugin presence check (Bash)
# -----------------------------

set -e

REPO="${TMPDIR:-/tmp}/maven-empty-repo"

# 1. Cleanup silently
rm -rf "$REPO" >/dev/null 2>&1

# 2. Create fresh repo
mkdir -p "$REPO"

# 3. Trigger Maven plugin download (silent)
mvn -Dmaven.repo.local="$REPO" help:help -ntp -B >/dev/null 2>&1

# 4. Check for plugin JAR (robust)
if find "$REPO" -type f -name "maven-help-plugin-*.jar" | head -n 1 | grep -q .; then
    echo "[OK] Maven Help Plugin appears to be downloaded."
    exit 0
else
    echo "[FAIL] Maven Help Plugin is missing."
    exit 1
fi
