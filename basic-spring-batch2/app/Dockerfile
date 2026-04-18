FROM eclipse-temurin:11-jre-alpine
ARG app_jar="example.springboot-batch.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080

ENTRYPOINT ["java", "-jar", "/app.jar"]

