FROM openjdk:8-jre-alpine3.9

# No real httpd server

RUN apk update && apk upgrade && \
    apk add perl && \
    rm -rf /var/cache/apk/* && \
    mkdir -p /var/www/localhost/cgi-bin

COPY cgi-bin /var/www/localhost/cgi-bin
COPY JSON /var/www/localhost/cgi-bin/JSON
COPY YAML /var/www/localhost/cgi-bin/YAML
COPY XML /var/www/localhost/cgi-bin/XML

RUN chmod 775 /var/www/localhost/cgi-bin/*
ARG app_jar="example.basic-perl-cgi-java.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8085
ENTRYPOINT ["java", "-jar", "app.jar"]
