i#!/bin/bash
# Step 1: Set Maven env for a private repository
export MAVEN_LOCAL_REPO="$HOME/.m2/private-repo"
mkdir -p "$MAVEN_LOCAL_REPO"

export MAVEN_OPTS="-Dmaven.repo.local=$MAVEN_LOCAL_REPO"

echo "Maven local repo set to $MAVEN_LOCAL_REPO"

