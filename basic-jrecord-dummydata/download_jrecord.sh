#!/bin/bash
# Step 2a: Download JRecord 0.93.3 SourceForge zip
JRECORD_ZIP="JRecord-0.93.3-src.zip"
JRECORD_URL="https://sourceforge.net/projects/jrecord/files/JRecord/0.93.3/$JRECORD_ZIP/download"

curl -L -o "$JRECORD_ZIP" "$JRECORD_URL"

# Step 2b: Unzip
unzip -o "$JRECORD_ZIP" -d JRecord-0.93.3

# Step 2c: Deploy to local Maven repository
mvn deploy:deploy-file \
    -Dfile=JRecord-0.93.3/JRecord-0.93.3.jar \
    -DgroupId=net.sf.jrecord \
    -DartifactId=JRecord \
    -Dversion=0.93.3 \
    -Dpackaging=jar \
    -Durl=file://$MAVEN_LOCAL_REPO \
    -DrepositoryId=local

