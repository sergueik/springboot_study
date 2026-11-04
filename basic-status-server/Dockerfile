FROM openjdk:8u191-jre-alpine3.9
ARG app_jar="example.status-server.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080
ENTRYPOINT java -cp app.jar example.App -ready ${READY:up}  -live ${LIVE:up}
