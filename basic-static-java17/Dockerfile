FROM openjdk:17-alpine
WORKDIR /app
ARG REFRESHED_AT
ENV REFRESHED_AT $REFRESHED_AT

ARG MAVEN_VERSION=3.8.8

RUN apk add --no-cache \
  curl

RUN mkdir -p /usr/share/maven /usr/share/maven/ref \
  && curl -fsSL -o /tmp/apache-maven.tar.gz https://downloads.apache.org/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz \
  && tar -xzf /tmp/apache-maven.tar.gz -C /usr/share/maven --strip-components=1 \
  && rm -f /tmp/apache-maven.tar.gz \
  && ln -s /usr/share/maven/bin/mvn /usr/bin/mvn
# see also: https://medium.com/@migueldoctor/how-to-create-a-custom-docker-image-with-jdk8-maven-and-gradle-ddc90f41cee4

ENV MAVEN_HOME /usr/share/maven
RUN mkdir -p  "/tmp/maven/.m2"
ENV MAVEN_CONFIG "/tmp/maven/.m2"
