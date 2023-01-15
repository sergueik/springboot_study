FROM openjdk:8-jre-alpine3.9
RUN apk add curl 
ARG app_jar="example.soap-service.jar"
ADD "target/${app_jar}" app.jar
ADD src/main/resources src/main/resources
EXPOSE 8888
ENTRYPOINT ["java", "-cp", "app.jar", "example.Application"]
