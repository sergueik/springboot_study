FROM openjdk:8-jre-alpine3.9
ENV APP_DIR /demo
WORKDIR ${APP_DIR}
ARG app_jar="example.https-basic-auth-prometheus-counter.jar"
ADD "target/${app_jar}" ${APP_DIR}/app.jar
ADD src/test/resources ${APP_DIR}/src/test/resources
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "app.jar"]
