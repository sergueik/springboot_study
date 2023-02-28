FROM openjdk:8-jre-alpine3.9
VOLUME /app/log
ADD target/micro-eureka-service-0.0.1-SNAPSHOT.jar micro-eureka-service-0.0.1-SNAPSHOT.jar
EXPOSE 8380
ENTRYPOINT ["java", "-jar", "micro-eureka-service-0.0.1-SNAPSHOT.jar"]
