# Copyright (c) 2020 Serguei Kouzmine
#
FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.todolist.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8086
ENTRYPOINT ["java", "-jar", "app.jar"]
