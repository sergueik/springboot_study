FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic-environment.jar"
ADD "target/${app_jar}" app.jar
ARG setting_arg="dockerfile_value"
ARG setting_name="setting_env"
ENV setting_env=$setting_arg 
# NOTE: probably need to resolve arg through env the other way around
# ENTRYPOINT ["java", "-cp", "app.jar", "-Dsetting=${setting_env}", "example.EnvironmentPrinter", "${setting_name}"]
ENTRYPOINT ["java", "-cp", "app.jar", "-Dsetting=${setting_env}", "example.EnvironmentPrinter"]
