FROM python:3.8.2-alpine
ARG FILENAME_ARG
# Environment Variables
ENV JENKINS_VERSION 1.625.2
ENV JENKINS_VERSION 2.346.2
ENV JENKINS_HOME /var/lib/jenkins
ENV JENKINS_SHARE /usr/share/jenkins
ENV JENKINS_SLAVE_AGENT_PORT 50000
ENV JENKINS_UC https://updates.jenkins-ci.org
ENV URL https://get.jenkins.io/war-stable/$JENKINS_VERSION
# NOTE: equal sign is optional in the following line
ENV FILENAME=$FILENAME_ARG
EXPOSE 8080

EXPOSE 50000

RUN addgroup jenkins && adduser -h $JENKINS_HOME -D -s /bin/ash -G jenkins jenkins
COPY systemconfig.sh /tmp/systemconfig.sh
RUN ash -c /tmp/systemconfig.sh
# install JDK
# RUN apk update
RUN apk add --no-cache openjdk8-jre coreutils zip git wget su-exec
RUN apk add --no-cache -t .build-deps curl ca-certificates jq
RUN rm -rf /var/cache/apk/*
RUN curl -fL $URL/jenkins.war -o $JENKINS_SHARE/jenkins.war


# https://hub.docker.com/layers/jenkins/jenkins/jenkins/alpine/images/sha256-8d3a07ab52c7e9384f3721d80a213e7aee20489037d1e7bb1563c612d3b8b0f8?context=explore
# the other way around: https://github.com/Zenika/alpine-jenkins-with-docker/blob/master/Dockerfile
# NOTE: cannot use exec format when passing argument
# need variable expansion
# /usr/local/bin/python: can't open file 'temp/${FILENAME}': [Errno 2] No such file or directory 
# CMD ["/usr/local/bin/python", "temp/${FILENAME}"]
# 
# CMD python temp/${FILENAME}
# https://github.com/HearstAT/docker-alpinejenkins
COPY jenkins.sh /usr/local/bin/jenkins
WORKDIR $JENKINS_HOME
ENTRYPOINT ["/usr/local/bin/jenkins"]
