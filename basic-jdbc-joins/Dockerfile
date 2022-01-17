FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.jdbc-joins.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080

ENTRYPOINT [ "sh", "-c", "umask 0027; java -jar /app.jar" ]
