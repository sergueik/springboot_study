FROM openjdk:8-jre-alpine3.9
ARG workdir="/work"
RUN mkdir -p /work/logs
ARG app_jar="example.logback.jar"
ADD "target/${app_jar}" /work/app.jar
EXPOSE 8080
WORKDIR "/work"
ENTRYPOINT ["java", "-jar", "/work/app.jar"]
