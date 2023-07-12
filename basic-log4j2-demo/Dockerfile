FROM openjdk:8-jre-alpine3.9
ARG WORKDIR="/work"
WORKDIR $WORKDIR
ARG app_jar="example.log4j2.jar"
ADD target "src/main/resources/log4j2.properties" $WORKDIR/
EXPOSE 8085
ENTRYPOINT [ "java", "-cp", "example.log4j2.jar:lib/*", "example.Example"]
