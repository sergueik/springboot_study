FROM openjdk:8u191-jre-alpine3.9
WORKDIR /tmp
ARG app_jar="example.httpserver.jar"
ARG CHART=chart-0.1.0.tgz 
ENV CHART=$CHART
COPY "target/${app_jar}" app.jar
COPY $CHART index.yaml ./
EXPOSE 8080
ENTRYPOINT java -cp app.jar example.App -root /tmp  -dir package
