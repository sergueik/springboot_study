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

# 4. Inspect distribution folder
echo "Inspecting ~/.m2/wrapper/dists/"
ls -l ~/.m2/wrapper/dists/apache-maven-* || echo "No distributions found!"

# 5. Optionally purge again for clean repeat
rm -rf ~/.m2/wrapper/dists/*
echo "Final purge done."

# Restore PATH
export PATH="$OLD_PATH"
echo "PATH restored."
