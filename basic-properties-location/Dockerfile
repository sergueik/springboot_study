FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.properties-location.jar"
WORKDIR /work/place
ADD "target/${app_jar}" app.jar
EXPOSE 8085
RUN mkdir -p ~/.keys/ && echo 123| tee /root/.keys/key.txt
# ENTRYPOINT ["java", "-jar", "app.jar", "--spring.config.location=file:///work/directory" , "-Dlocation=../../root/.keys/"]
ENTRYPOINT ["java", "-Dlocation=../../root/.keys", "-jar", "app.jar"]
