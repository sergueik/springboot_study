#!/bin/bash
set -e

echo "=== Maven Wrapper Filing Probe (Bash) ==="

# 1. Shadow system Maven
OLD_PATH="$PATH"
export PATH=$(echo "$PATH" | tr ':' '\n' | grep -v 'apache-maven' | paste -sd ":" -)
echo "System Maven shadowed. PATH adjusted."

# 2. Purge Maven Wrapper cache
rm -rf ~/.m2/wrapper/dists/*
echo "Wrapper cache cleared."

# 3. Run Maven Wrapper
echo "Running ./mvnw -v ..."
./mvnw -v
# 4. Examine wrapper version, compare with distributionUrl 

dist=$(grep '^distributionUrl=' .mvn/wrapper/maven-wrapper.properties | sed -E 's/.*apache-maven-([0-9.]+)-bin.*/\1/')

mvnver=$(./mvnw -v 2>/dev/null | head -n1 | awk '{print $3}')

if [ "$dist" = "$mvnver" ]; then
  echo "OK: Maven Wrapper version matches ($mvnver)"
  exit 0
else
  echo "Mismatch: wrapper=$dist runtime=$mvnver" >&2
  exit 1
fi

# 5. Inspect distribution folder
echo "Inspecting ~/.m2/wrapper/dists/"
ls -l ~/.m2/wrapper/dists/apache-maven-* || echo "No distributions found!"

# 6. Optionally purge again for clean repeat
rm -rf ~/.m2/wrapper/dists/*
echo "Final purge done."

# Restore PATH
export PATH="$OLD_PATH"
echo "PATH restored."
