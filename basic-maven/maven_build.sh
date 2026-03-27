#!/bin/bash
# =============================================
# Maven 3.8.x bootstrap build in WSL
# NOTE: Uses isolated /tmp repo to avoid /tmp/.m2 clutter
# =============================================

SRC_DIR=/tmp/maven_src/apache-maven-3.8.5
ISOLATED_REPO=/tmp/maven_build/.m2/repository
JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk

export JAVA_HOME
export PATH=$JAVA_HOME/bin:$PATH

echo "Using isolated Maven repo at $ISOLATED_REPO"
mkdir -p $ISOLATED_REPO

# Use older Maven to bootstrap
BOOTSTRAP_MVN=/usr/share/maven-3.5/bin/mvn  # TODO: adjust path

cd $SRC_DIR || { echo "Source dir not found"; exit 1; }

echo "Building Maven 3.8.x using bootstrap Maven 3.5.4..."
$BOOTSTRAP_MVN -Dmaven.repo.local=$ISOLATED_REPO -Pbootstrap clean install -DskipTests

echo
echo "Bootstrap build complete."
echo "Verify Maven 3.8.5:"
$SRC_DIR/bin/mvn -version
