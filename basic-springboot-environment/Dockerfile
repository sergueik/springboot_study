FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.springboot-environment.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8085
ARG value_arg="dockerfile_value"
ARG name_arg="setting_env"
ENV name_env=$name_arg 
ENV value_env=$value_arg 
ENTRYPOINT [ "sh" , "-c", "java -D${name_env}=${value_env} -jar app.jar"]
