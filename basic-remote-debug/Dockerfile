FROM openjdk:8-jre-alpine3.9
ENV JAVA_APP_JAR example.remote-debug.jar
WORKDIR /app/
COPY target/$JAVA_APP_JAR .
EXPOSE 8080 5000

CMD java -XX:+PrintFlagsFinal -XX:+PrintGCDetails $JAVA_OPTIONS -jar $JAVA_APP_JAR
