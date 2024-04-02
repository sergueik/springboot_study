FROM openjdk:8-jre-alpine3.9
EXPOSE 8080
ARG JAR_FILE=example.poi.jar
ADD target/${JAR_FILE} /poi.jar
ENTRYPOINT ["java", "-jar","/poi.jar"]
