FROM php:8.2.1-fpm-alpine3.17
WORKDIR /app
USER root
# RUN addgroup -g 1000 app && adduser -u 1000 -G app -s /bin/sh -D app
RUN  apk add shadow
RUN addgroup -g 1000 app && addgroup www-data app
RUN adduser -u 1000 -s /bin/sh -D -G app app
RUN sed -i 's|^ *user = .*|user = app|' /usr/local/etc/php-fpm.d/www.conf
RUN sed -i 's|^ *user = .*|user = app|' /usr/local/etc/php-fpm.d/www.conf
RUN usermod -a -G app www-data
USER www-data
# HEALTHCHECK --interval=5s --timeout=3s --start-period=1s CMD REDIRECT_STATUS=true SCRIPT_NAME=/ping SCRIPT_FILENAME=/ping REQUEST_METHOD=GET cgi-fcgi -bind -connect 127.0.0.1:9000 || exit 1
