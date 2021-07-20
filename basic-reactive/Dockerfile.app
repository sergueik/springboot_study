# FROM frolvlad/alpine-oraclejdk8:slim
FROM openjdk:8-jre-alpine3.9

RUN mkdir workspace

WORKDIR workspace

ADD target/example.basic-tests.jar app.jar

EXPOSE 5005

EXPOSE 8085

CMD java -jar ${ADDITIONAL_OPTS} app.jar
