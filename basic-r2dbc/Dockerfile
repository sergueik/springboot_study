FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.r2dbc.jar"
WORKDIR /app
ADD "target/${app_jar}" app.jar
CMD ["java", "-jar", "-Ddatabase.host=postgres","app.jar"]
