FROM openjdk:8-jre-alpine3.9

COPY root /

RUN apk update && apk upgrade && \
    apk add apache2 libxml2-dev apache2-utils apache-mod-fcgid perl perl-fcgi && \
    rm -rf /var/cache/apk/* && \
    mkdir /var/run/mod_fcgid/ && chown -R apache.www-data /var/run/mod_fcgid  && \
    sed -i 's|AllowOverride [Nn]one|AllowOverride All|' /etc/apache2/httpd.conf && \
    sed -i 's|#LoadModule cgi|LoadModule cgi|g' /etc/apache2/httpd.conf && \
    sed -i 's|Options Indexes|Options |g' /etc/apache2/httpd.conf

COPY cgi-bin /var/www/localhost/cgi-bin
COPY JSON /var/www/localhost/cgi-bin/JSON
COPY YAML /var/www/localhost/cgi-bin/YAML
COPY XML /var/www/localhost/cgi-bin/XML

RUN chmod 775 /var/www/localhost/cgi-bin/*
ARG app_jar="example.basic-perl-cgi-java.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8085
ENTRYPOINT ["java", "-jar", "app.jar"]
