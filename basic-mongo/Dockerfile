# Copyright (c) 2020 Serguei Kouzmine
#
FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic-mongo-app.jar"
ADD "spring/target/${app_jar}" app.jar
EXPOSE 8085
ENTRYPOINT ["sh",  "-c", "echo \"Try service on $SERVICE_HOST $SERVICE_PORT\";while !( nc -z $SERVICE_HOST $SERVICE_PORT ) ; do echo \"Wait for service on $SERVICE_HOST $SERVICE_PORT\";sleep 10;  done ; java -jar app.jar"]
