FROM alpine:3.9.5 AS generate-certs 
RUN apk add --no-cache openssl
WORKDIR /certs
RUN openssl req -x509 \
    -subj "/CN=example.com" \
    -nodes \
    -days 365 \
    -newkey rsa:4096 \
    -keyout /certs/server.key \
    -out /certs/server.crt


FROM nimmis/alpine-micro


RUN apk --allow-untrusted update && apk upgrade && \
    apk add apache2 libxml2-dev apache2-utils apache2-ssl curl jq apache-mod-fcgid perl perl-fcgi  && \
    rm -rf /var/cache/apk/*

COPY root/. /
COPY --from=generate-certs /certs /etc/ssl/apache2/
RUN cd /etc/service && rm apache2 && ln -fs ../sv/apache2 apache2 && \
    sed -i 's|\r||g' /etc/sv/apache2/run /etc/run_always/50-config-webdir /etc/run_always/55-fix-html-dir


RUN  mkdir /web/ && chown -R apache.www-data /web && \
    sed -i 's|^DocumentRoot ".*|DocumentRoot "/web/html"|g' /etc/apache2/httpd.conf && \
    sed -i 's|AllowOverride [Nn]one|AllowOverride All|' /etc/apache2/httpd.conf && \
    sed -i 's|^ServerRoot .*|ServerRoot /web|g'  /etc/apache2/httpd.conf && \
    sed -i 's/^|ServerName.*/ServerName webproxy/' /etc/apache2/httpd.conf && \
    sed -i 's|^IncludeOptional /etc/apache2/conf|IncludeOptional /web/config/conf|g' /etc/apache2/httpd.conf && \
    sed -i 's|PidFile "/run/.*|Pidfile "/web/run/httpd.pid"|g'  /etc/apache2/conf.d/mpm.conf && \
    sed -i 's|Directory "/var/www/localhost/htdocs.*|Directory "/web/html" >|g' /etc/apache2/httpd.conf && \
    sed -i 's|Directory "/var/www/localhost/cgi-bin.*|Directory "/web/cgi-bin" >|g' /etc/apache2/httpd.conf && \
    sed -i 's|/var/www/localhost/htdocs|/web/html|g' /etc/apache2/conf.d/ssl.conf && \
    sed -i 's|^SSLCertificateFile.*|SSLCertificateFile /etc/ssl/apache2/server.crt|g' /etc/apache2/conf.d/ssl.conf && \
    sed -i 's|/var/www/localhost/cgi-bin|/web/cgi-bin|g' /etc/apache2/conf.d/ssl.conf && \
    sed -i 's|/var/www/localhost/cgi-bin|/web/cgi-bin|g' /etc/apache2/httpd.conf && \
    sed -i 's|/var/log/apache2/|/web/logs/|g' /etc/logrotate.d/apache2 && \
    sed -i 's|Options Indexes|Options |g' /etc/apache2/httpd.conf && \
    mkdir /web/cgi-bin && \
    cp /var/www/localhost/cgi-bin/test-cgi /web/cgi-bin/test-cgi && \
    chmod +x /web/cgi-bin/test-cgi && \
    sed -i 's|^#|#!/bin/ash|' /web/cgi-bin/test-cgi && \
    mkdir /var/run/mod_fcgid/ && chown -R apache.www-data /var/run/mod_fcgid  && \
    sed -i 's|AllowOverride [Nn]one|AllowOverride All|' /etc/apache2/httpd.conf && \
    sed -i 's|#LoadModule cgi|LoadModule cgi|g' /etc/apache2/httpd.conf && \
    sed -i 's|Options Indexes|Options |g' /etc/apache2/httpd.conf 


COPY html /web/htdocs
COPY cgi-bin /web/cgi-bin
RUN sed -i 's|\r||g' /web/cgi-bin/*cgi
VOLUME /web

EXPOSE 80 443

