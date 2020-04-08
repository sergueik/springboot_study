FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic-environment.jar"
ADD "target/${app_jar}" app.jar
ENTRYPOINT ["java", "-cp", "app.jar", "example.EnvironmentPrinter"]
