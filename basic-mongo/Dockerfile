# Copyright (c) 2020 Serguei Kouzmine
#
FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic-mongo-app.jar"
ADD "target/${app_jar}" app.jar
COPY delayed_start.sh delayed_start.sh
EXPOSE 8085
ENTRYPOINT ["sh", "/delayed_start.sh"]
# NOTE: cannot use CMD and vanilla ENTRYPOINT
# CMD delayed_start.sh
# ENTRYPOINT ["java", "-jar", "app.jar"]
