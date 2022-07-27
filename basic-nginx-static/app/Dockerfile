FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.static_page.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
