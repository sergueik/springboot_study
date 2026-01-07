FROM eclipse-temurin:11-jre-alpine
ARG app_jar="app.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8085
ENTRYPOINT ["java", "-jar", "app.jar"]
