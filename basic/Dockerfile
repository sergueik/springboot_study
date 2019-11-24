FROM jolokia/alpine-jre-8
ADD target/example.basic.jar example.basic.jar
EXPOSE 8085
ENTRYPOINT ["java", "-jar", "example.basic.jar"]
