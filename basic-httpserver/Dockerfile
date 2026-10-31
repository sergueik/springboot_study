FROM alpine/java:11-jre
WORKDIR /tmp
ARG app_jar="example.httpserver.jar"
COPY "target/${app_jar}" app.jar
COPY chart-0.1.0.tgz index.yaml ./
EXPOSE 8080
ENTRYPOINT java -cp app.jar example.App -root /tmp  -dir package
