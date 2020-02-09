FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic-xml.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080
ARG title
ENV title_env=$title 
ARG category
ENV category_env=$category 
ENTRYPOINT ["java", "-Dtitle=${title_env} -Dcategory=${category_env}", "-jar", "app.jar" ]

