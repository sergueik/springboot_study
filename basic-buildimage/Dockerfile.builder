FROM azul/zulu-openjdk-alpine:11 as builder
WORKDIR application

ENV MAVEN_VERSION 3.8.1
ENV MAVEN_HOME /usr/lib/mvn
ENV PATH $MAVEN_HOME/bin:$PATH

RUN wget http://archive.apache.org/dist/maven/maven-3/$MAVEN_VERSION/binaries/apache-maven-$MAVEN_VERSION-bin.tar.gz && \
  tar -zxvf apache-maven-$MAVEN_VERSION-bin.tar.gz && \
  rm apache-maven-$MAVEN_VERSION-bin.tar.gz && \
  mv apache-maven-$MAVEN_VERSION /usr/lib/mvn

COPY pom.xml $WORKDIR
COPY src /application/src
# NOTE COPY src $WORKDIR/src
# does not work -  ended up with /src
# 
ARG JAR_FILE=/application/target/*.jar
# When using COPY with more than one source file, the destination must be a directory and end with a /

ARG JAR_FILE=/application/target/example.buildimage.jar 

RUN mvn clean package
# COPY ${JAR_FILE} application.jar
RUN java -Djarmode=layertools -jar $JAR_FILE extract
