FROM openjdk:8-jre-alpine3.9
#
ARG PUID=1000
ARG PGID=1000
#
RUN set -xe \
    && apk add --no-cache --purge -uU \
        curl icu-libs unzip zlib-dev musl \
        mesa-gl mesa-dri-swrast \
        libreoffice libreoffice-base libreoffice-lang-uk \
        ttf-freefont ttf-opensans ttf-ubuntu-font-family ttf-inconsolata \
	ttf-liberation ttf-dejavu \
        libstdc++ dbus-x11 \
    && echo "http://dl-cdn.alpinelinux.org/alpine/v3.9/main" >> /etc/apk/repositories \
    && rm -rf /var/cache/apk/* /tmp/*
# TODO:
#    && apk add --no-cache -U ttf-font-awesome ttf-mononoki ttf-hack 
#
VOLUME /home/alpine/
#
USER root
RUN apk add shadow
RUN addgroup -g 1000 alpine 
RUN adduser -u 1000 -s /bin/sh -D -G alpine alpine
USER alpine
WORKDIR /home/alpine/
#
# ENTRYPOINT ["/usr/bin/libreoffice"]
ENTRYPOINT ["ash"]
# ENTRYPOINT ["/bin/bash"]
# ENTRYPOINT ["/init"]
