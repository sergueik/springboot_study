## FROM jolokia/alpine-jre-8
FROM openjdk:8-jre-alpine3.9
VOLUME /tmp
ADD target/example.portafolio.jar app.jar
ENV JAVA_OPTS=""
EXPOSE 57116
ENTRYPOINT [ "sh", "-c", "java $JAVA_OPTS -Djava.security.egd=file:/dev/./urandom -jar /app.jar" ]
