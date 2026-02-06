FROM eclipse-temurin:11-jre-alpine
WORKDIR /app
ARG app_jar="example.graphviz-java.jar"
ADD "target/${app_jar}" app.jar
ENTRYPOINT [ "java", "-jar", "app.jar" ]
