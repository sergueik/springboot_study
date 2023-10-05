FROM openjdk:8-jre-alpine3.9

ARG app_jar="example.basic-perl-cgi-java.jar"
# No real httpd server will run on this host

RUN apk update && apk upgrade && \
    apk add jq && \
    apk add perl && \
    rm -rf /var/cache/apk/* && \
    mkdir -p /var/www/localhost/cgi-bin

ADD "target/${app_jar}" app.jar
ADD cgi-bin /var/www/localhost/cgi-bin

RUN sed -i 's|\r||g' /var/www/localhost/cgi-bin/*.* && \
    chmod 775 /var/www/localhost/cgi-bin/*.*
EXPOSE 8085
ENTRYPOINT ["java", "-jar", "app.jar"]
