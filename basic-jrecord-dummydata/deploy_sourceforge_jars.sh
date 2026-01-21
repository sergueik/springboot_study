#!/bin/bash
set -e

# -----------------------------
# Use environment variable for home
# -----------------------------
USER_HOME="$HOME"
MAVEN_LOCAL_REPO="$USER_HOME/.m2/private-repo"
mkdir -p "$MAVEN_LOCAL_REPO"

echo "Custom Maven local repo: $MAVEN_LOCAL_REPO"

# Deploy all three jars
mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecord -Dversion=0.93.2 -Dpackaging=jar -Dfile=./JRecord.jar -Durl=file://$MAVEN_LOCAL_REPO -DrepositoryId=local && \
mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecordCodeGen -Dversion=0.93.2 -Dpackaging=jar -Dfile=./JRecordCodeGen.jar -Durl=file://$MAVEN_LOCAL_REPO -DrepositoryId=local && \
mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=cb2xml -Dversion=0.93.2 -Dpackaging=jar -Dfile=./cb2xml.jar -Durl=file://$MAVEN_LOCAL_REPO -DrepositoryId=local

echo "✅ All JRecord jars deployed to $MAVEN_LOCAL_REPO"
