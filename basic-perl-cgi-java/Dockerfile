FROM openjdk:8-jre-alpine3.9

# No real httpd server

RUN apk update && apk upgrade && \
    apk add perl && \
    rm -rf /var/cache/apk/* && \
    mkdir -p /var/www/localhost/cgi-bin

ADD cgi-bin /var/www/localhost/cgi-bin

RUN chmod 775 /var/www/localhost/cgi-bin/*.cgi
ARG app_jar="example.basic-perl-cgi-java.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8085
ENTRYPOINT ["java", "-jar", "app.jar"]
