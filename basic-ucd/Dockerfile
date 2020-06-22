FROM openjdk:8-jre-alpine3.9
ARG app_jar="example-ucdclient.jar"
RUN mkdir /app/lib/
ADD "target/${app_jar}" /app/example-ucdclient.jar
COPY "target/lib/" /app
EXPOSE 8085
ENTRYPOINT ["java", "-cp", "/app/lib/*jar", "-cp", "/app/example-ucdclient.jar","example.Client"]
