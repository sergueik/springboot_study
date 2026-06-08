# FROM openjdk:8-jre-alpine3.9
# FROM eclipse-temurin:11-jre-alpine
FROM eclipse-temurin:8u492-b09-jre-alpine

ARG app_jar="example.postgresql.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
