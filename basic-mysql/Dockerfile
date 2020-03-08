# Copyright (c) 2020 Serguei Kouzmine
#
FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.mysql.jar"
ADD delayed_start.sh delayed_start.sh
ADD "target/${app_jar}" app.jar
EXPOSE 8086
ENTRYPOINT ["sh", "/delayed_start.sh"]
# NOTE: cannot use CMD and vanilla ENTRYPOINT
# CMD delayed_start.sh
# ENTRYPOINT ["java", "-jar", "app.jar"]
