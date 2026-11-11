FROM alpine:3.15
RUN apk update \
    && apk add --update --no-cache mingw-w64-gcc make imagemagick \
    && rm -rf /var/cache/* \
    && mkdir /var/cache/apk

RUN x86_64-w64-mingw32-windres --version

WORKDIR /app

