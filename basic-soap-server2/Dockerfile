FROM openjdk:8-jdk-alpine
VOLUME /tmp /var/log
# ENV SPRING_PROFILES_ACTIVE=docker
COPY target/*.jar app.jar
EXPOSE 8081
ENTRYPOINT ["java","-Djava.security.egd=file:/dev/./urandom","-jar","/app.jar"]
