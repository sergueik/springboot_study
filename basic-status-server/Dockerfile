# FROM bellsoft/liberica-openjre-alpine:8
FROM alpine/java:11-jre
ARG app_jar="example.status-server.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080
ENTRYPOINT java -cp app.jar example.App -ready ${READY:up}  -live ${LIVE:up}
