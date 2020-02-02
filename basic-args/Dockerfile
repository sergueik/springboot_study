FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic-args.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080
ARG params
ENV params_env=$params 
ENTRYPOINT ["java", "-Dparams=${params_env}", "-jar", "app.jar" ]

