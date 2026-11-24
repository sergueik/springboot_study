FROM alpine:3.22
ARG PUID=1000
ARG PGID=1000
RUN addgroup -g 1000 alpine 
RUN adduser -u 1000 -s /bin/sh -D -G alpine alpine

RUN chown -R alpine:alpine /home/alpine
USER root
RUN set -xe \
    && cat /etc/alpine-release \     
    && echo "http://dl-cdn.alpinelinux.org/alpine/v3.22/main" >> /etc/apk/repositories \
    && apk update \
    && apk add --no-cache --purge -uU \
        curl icu-libs unzip zlib-dev musl python3 py3-pip\
        mesa-gl \
        libreoffice \
        ttf-freefont ttf-opensans ttf-inconsolata font-awesome font-hack font-mononoki \
	ttf-liberation ttf-dejavu mesa-dri-gallium \
        libstdc++ dbus-x11 \
    && rm -rf /var/cache/apk/* /tmp/*

RUN pip install setuptools unoconv==0.9.0 --break-system-packages
WORKDIR /home/alpine/
USER alpine
RUN unoconv --version
ENTRYPOINT ["/usr/bin/libreoffice"]

CMD ["--headless", "--nologo", "-env:UserInstallation=file:///home/alpine", "--nofirststartwizard", "--accept=\"socket,host=0.0.0.0,port=8100;urp\""]
