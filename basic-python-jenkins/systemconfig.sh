#!/bin/ash

DIRECTORYLIST="/usr/share/jenkins /usr/share/jenkins/ref ${JENKINS_SHARE} ${JENKINS_HOME} /usr/local/bin /usr/bin "

for DIR in $DIRECTORYLIST; do
    mkdir -p ${DIR}
    chown -R jenkins:jenkins ${DIR}
    chmod -R 775 ${DIR}
done
