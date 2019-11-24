FROM jolokia/alpine-jre-8
ADD target/example.mysql.jar example.mysql.jar
EXPOSE 8086
ENTRYPOINT ["java", "-jar", "example.mysql.jar"]
