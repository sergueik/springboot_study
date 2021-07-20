FROM openjdk:8-jre-alpine3.9
RUN mkdir workspace
WORKDIR workspace
ADD target/example.basic-reactive.jar app.jar
EXPOSE 5005 8080

CMD java -jar ${ADDITIONAL_OPTS} app.jar
