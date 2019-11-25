# FROM openjdk:8-jdk-alpine
FROM jolokia/alpine-jre-8
# RUN apk update && apk add libc6-compat
VOLUME /tmp
ADD target/pricing-0.0.1.jar  app.jar
ENV JAVA_OPTS=""
EXPOSE 57116
ENTRYPOINT [ "sh", "-c", "java $JAVA_OPTS -Djava.security.egd=file:/dev/./urandom -jar /app.jar" ]
