FROM maven:3.9.5-eclipse-temurin-11-alpine AS build-cb2xml

ARG REPO=https://github.com/bmTas/cb2xml
ARG COMMIT=97f8dc8

WORKDIR /cb2xml

RUN apk add --no-cache git

RUN git clone ${REPO} cb2xml \
 && cd cb2xml \
 && git checkout ${COMMIT} \
 && mvn -B -DskipTests install

FROM maven:3.9.5-eclipse-temurin-11-alpine AS build-jrecord

ARG REPO=https://github.com/bmTas/JRecord
ARG COMMIT=f50ece71

WORKDIR /jrecord

RUN apk add --no-cache git

# bring forward the local Maven repository
COPY --from=build-cb2xml /root/.m2 /root/.m2

RUN git clone ${REPO} jrecord \
 && cd jrecord \
 && git checkout ${COMMIT} 

COPY pom.jrecord.xml /jrecord/pom.xml
RUN cd /jrecord \
    && mvn -B -DskipTests -f pom.xml install

FROM maven:3.9.5-eclipse-temurin-11-alpine AS build-app

ARG REPO=https://github.com/bmTas/CobolToJson
ARG COMMIT=99b0aa2

WORKDIR /CobolToJson

RUN apk add --no-cache git

# reuse full dependency graph
COPY --from=build-jrecord /root/.m2 /root/.m2

RUN mvn install:install-file \
      -Dfile=/root/.m2/repository/net/sf/cb2xml/1.01.08/cb2xml-1.01.08.jar \
      -DpomFile=/root/.m2/repository/net/sf/cb2xml/1.01.08/cb2xml-1.01.08.pom \
      -DgroupId=net.sf.cb2xml \
      -DartifactId=cb2xml \
      -Dversion=1.01.08 \
      -Dpackaging=jar

RUN git clone ${REPO} CobolToJson \
 && cd CobolToJson \
 && git checkout ${COMMIT}

COPY pom.cobol2json.xml pom.xml

RUN cd /CobolToJson \
    && mvn -B -DskipTests package

RUN ls -l target/* \
    && unzip -l target/cobolToJson-0.93.3.jar | grep CobolToJson

FROM eclipse-temurin:11-jre-alpine

WORKDIR /app

# NOTE: no glob here - the cobolToJson-0.93.3.jar was repackaged into runnable jar
COPY --from=build-app /CobolToJson/target/cobolToJson-0.93.3.jar /app/app.jar

ENTRYPOINT ["java","-jar","/app/app.jar"]

