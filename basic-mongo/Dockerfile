FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic-mongo-app.jar"
ADD "target/${app_jar}" app.jar
COPY delayed_start.sh delayed_start.sh
EXPOSE 8085
ENTRYPOINT ["sh", "/delayed_start.sh"]
