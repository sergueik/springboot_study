FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8085
ENTRYPOINT ["java", "-jar", "app.jar"]
