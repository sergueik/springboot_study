FROM alpine:3.22
ARG PUID=1000
ARG PGID=1000
RUN addgroup -g 1000 alpine 
RUN adduser -u 1000 -s /bin/sh -D -G alpine alpine
USER root
RUN set -xe \
    && cat /etc/alpine-release \     
    && echo "http://dl-cdn.alpinelinux.org/alpine/v3.22/main" >> /etc/apk/repositories \
    && apk update \
    && apk add --no-cache --purge -uU \
    openjdk11-jre curl icu-libs unzip zlib-dev musl mesa-gl ttf-freefont ttf-opensans ttf-inconsolata font-awesome font-hack font-mononoki ttf-liberation ttf-dejavu mesa-dri-gallium libstdc++ dbus-x11 \
    && rm -rf /var/cache/apk/* /tmp/*

RUN apk add --no-cache \
  --repository=http://dl-cdn.alpinelinux.org/alpine/edge/community \
  libreoffice \
  && rm -rf /var/cache/apk/* /tmp/* 

USER alpine
WORKDIR /home/alpine/
ENTRYPOINT ["ash"]
