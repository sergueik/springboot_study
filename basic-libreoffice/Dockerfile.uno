# FROM openjdk:8-jre-alpine3.9
# FROM eclipse-temurin:11-jre
FROM alpine:3.19
ARG PUID=1000
ARG PGID=1000
# TODO: switch to Alpine 3.19
RUN set -xe \
    && cat /etc/alpine-release \     
    && echo "http://dl-cdn.alpinelinux.org/alpine/v3.19/main" >> /etc/apk/repositories \
    && apk update \
    && apk add --no-cache --purge -uU \
        curl icu-libs unzip zlib-dev musl python3 py3-pip\
        mesa-gl \
        libreoffice-common libreoffice-writer libreoffice-calc libreoffice-impress libreoffice-base libreoffice-lang-uk \
        ttf-freefont ttf-opensans ttf-inconsolata font-awesome font-hack font-mononoki \
	ttf-liberation ttf-dejavu mesa-dri-gallium \
        libstdc++ dbus-x11 \
    && rm -rf /var/cache/apk/* /tmp/*

# VOLUME /home/alpine/
USER root
RUN curl -svkLo /tmp/unoconv  https://raw.githubusercontent.com/dagwieers/unoconv/master/unoconv  \
    && mv /tmp/unoconv /usr/local/bin \
    && chmod +x /usr/local/bin/unoconv \
    &&   test -f /usr/bin/python || ln -stv /usr/bin/python3 /usr/bin/python

RUN pip install setuptools --break-system-packages
# TODO: downgrade
# You cannot run pip install distutils directly because distutils is not a standalone package on PyPI, and it was officially removed from the Python standard library in Python 3.12
WORKDIR /home/alpine/
RUN apk add shadow
RUN addgroup -g 1000 alpine 
RUN adduser -u 1000 -s /bin/sh -D -G alpine alpine

RUN  chown -R alpine:alpine /home/alpine
USER alpine
RUN ls -ld /home/alpine/ 
ENTRYPOINT ["/usr/bin/libreoffice"]
CMD ["--headless", "--nologo", "-env:UserInstallation=file:///home/alpine", "--nofirststartwizard", "--accept='socket,host=0.0.0.0,port=8100;urp'"]
