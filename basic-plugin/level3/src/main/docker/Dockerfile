FROM openjdk:8-jre-alpine3.9
RUN apk add bash
WORKDIR /app
VOLUME ["/app"]
ADD maven/app.jar app.jar
ADD maven/start.sh /start.sh
ADD  maven/wait-for-it.sh /wait-for-it.sh
RUN sh -c 'touch app.jar'
# ENTRYPOINT ["./start.sh"]
