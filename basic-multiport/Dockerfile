FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.multiple-ports.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8880
EXPOSE 8881
EXPOSE 8882
EXPOSE 8883
ENTRYPOINT ["java", "-jar", "app.jar"]
