FROM nimmis/alpine-micro
# FROM alpine:3.9

COPY root/. /

RUN apk update && apk upgrade && \

    # Info file about build:
    printf "Build of kjetils/alpine-apache-perl-fcgi, date: %s\n"  `date -u +"%Y-%m-%dT%H:%M:%SZ"` >> /etc/BUILD && \

    apk add apache2 libxml2-dev apache2-utils apache-mod-fcgid perl perl-fcgi && \
    mkdir /var/run/mod_fcgid/ && chown -R apache.www-data /var/run/mod_fcgid  && \
    sed -i 's,^DocumentRoot ".*,DocumentRoot "/web/html",g' /etc/apache2/httpd.conf && \
    sed -i 's,AllowOverride [Nn]one,AllowOverride All,'     /etc/apache2/httpd.conf && \
    sed -i 's,^ServerRoot .*,ServerRoot /web,g'             /etc/apache2/httpd.conf && \
    sed -i 's/^#ServerName.*/ServerName webproxy/'          /etc/apache2/httpd.conf && \
    sed -i 's,^IncludeOptional /etc/apache2,IncludeOptional /web/config,g'     /etc/apache2/httpd.conf && \
    sed -i 's,PidFile "/run/.*,Pidfile "/web/run/httpd.pid",g'                 /etc/apache2/conf.d/mpm.conf && \
    sed -i 's,Directory "/var/www/localhost/htdocs.*,Directory "/web/html">,g' /etc/apache2/httpd.conf && \
    sed -i 's,/var/www/localhost/cgi-bin,/web/cgi-bin,g'  /etc/apache2/httpd.conf && \
    sed -i 's,#LoadModule cgi,LoadModule cgi,g'           /etc/apache2/httpd.conf && \
    sed -i 's,/var/www/localhost,/web,g'                  /etc/apache2/conf.d/mod_fcgid.conf && \
    sed -i 's,/var/log/apache2/,/web/logs/,g'             /etc/logrotate.d/apache2 && \
    sed -i 's,Options Indexes,Options ,g'                 /etc/apache2/httpd.conf && \
    rm -rf /var/cache/apk/*

VOLUME /web

EXPOSE 80 443
