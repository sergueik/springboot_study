FROM openjdk:8-jre-alpine3.9
ENV APP_DIR /demo
WORKDIR ${APP_DIR}
ARG app_jar="example.prometheus-counter.jar"
ADD "target/${app_jar}" ${APP_DIR}/app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
