FROM maven:3.9.5-eclipse-temurin-11-alpine AS cb2xml

ARG REPO=https://github.com/bmTas/cb2xml
ARG COMMIT=97f8dc8


RUN apk add --no-cache git

RUN git clone ${REPO} cb2xml \
 && cd cb2xml \
 && git checkout ${COMMIT}

WORKDIR /cb2xml

COPY pom.cb2xml.xml pom.xml

RUN mvn -B -DskipTests -f pom.xml install

FROM maven:3.9.5-eclipse-temurin-11-alpine AS jrecord

ARG REPO=https://github.com/bmTas/JRecord
ARG COMMIT=f50ece71


RUN apk add --no-cache git

# bring forward the local Maven repository
COPY --from=cb2xml /root/.m2 /root/.m2

RUN git clone ${REPO} jrecord \
 && cd jrecord \
 && git checkout ${COMMIT} 

WORKDIR /jrecord

COPY pom.jrecord.xml pom.xml

RUN mvn -B -DskipTests -f pom.xml install

FROM maven:3.9.5-eclipse-temurin-11-alpine AS cobol2json

ARG REPO=https://github.com/bmTas/CobolToJson
ARG COMMIT=99b0aa2


RUN apk add --no-cache git

# reuse full dependency graph
COPY --from=jrecord /root/.m2 /root/.m2

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

WORKDIR /CobolToJson

COPY pom.cobol2json.xml pom.xml

RUN mvn -B -DskipTests package

RUN ls -l target/*jar \
    && unzip -p target/cobolToJson-0.93.3.jar META-INF/MANIFEST.MF
    
FROM eclipse-temurin:11-jre-alpine

WORKDIR /app

# NOTE: no glob here - the cobolToJson-0.93.3.jar was repackaged into runnable jar
COPY --from=cobol2json /CobolToJson/target/cobolToJson-0.93.3.jar /app/app.jar

ENTRYPOINT ["java","-jar","/app/app.jar"]

