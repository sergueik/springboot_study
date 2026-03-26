#!/bin/bash
# =============================================
# Proof-of-Guava for WSL RHEL Maven streams
# Includes module list, Maven lib, RPM ownership
# Defaults to working stream 3.5
# =============================================

STREAM=${1:-3.5}  # default to 3.5 happy case

echo "=== DNF module list for Maven ==="
dnf module list maven

echo
echo "Switching to Maven stream $STREAM..."
sudo dnf module reset maven -y >/dev/null
sudo dnf module enable maven:$STREAM -y >/dev/null
sudo dnf install maven -y >/dev/null

# --- Clean local Maven cache ---
echo "Cleaning local Maven repository (~/.m2/repository)..."
rm -rf ~/.m2/repository

# --- Check for Guava jar in Maven lib folder ---
MAVEN_LIB=/usr/share/maven/lib
GUAVA_JAR=$(ls $MAVEN_LIB/guava-*.jar 2>/dev/null || true)

if [ -z "$GUAVA_JAR" ]; then
    echo "✅ PROOF: Guava jar MISSING in $MAVEN_LIB"
else
    echo "❌ Guava jar FOUND: $GUAVA_JAR"
    OWNER=$(rpm -qf $GUAVA_JAR 2>/dev/null || echo "Unknown owner")
    echo "    Owned by RPM: $OWNER"
fi

# --- Attempt minimal Maven command ---
echo
echo "Running 'mvn -version' to demonstrate class loading..."
mvn -version || echo "✅ mvn failed due to missing classes (expected)"

# --- List all RPMs related to Maven + Guava ---
echo
echo "Installed Maven-related RPMs:"
rpm -qa | grep -i maven

echo
echo "Installed Guava-related RPMs:"
rpm -qa | grep -i guava

echo
echo "Done. Stream $STREAM tested."
