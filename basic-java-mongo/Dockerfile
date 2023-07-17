# Copyright (c) 2020 Serguei Kouzmine
#
FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic-mongo-app.jar"
ADD "spring/target/${app_jar}" app.jar
ENV SERVICE_HOST=mongo-service
ENV SERVICE_PORT=27017
ENV SLEEP_INTERVAL=10
EXPOSE 8085
ENTRYPOINT ["sh",  "-c", "echo \"Try service on $SERVICE_HOST $SERVICE_PORT\";while !( nc -z $SERVICE_HOST $SERVICE_PORT ) ; do echo \"Wait for service on $SERVICE_HOST $SERVICE_PORT\";sleep $SLEEP_INTERVAL;  done ; java -jar app.jar"]
