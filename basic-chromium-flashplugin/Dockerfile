
FROM ubuntu:18.04

ARG DEBIAN_FRONTEND=noninteractive
ARG CHROMIUM_VERSION=65.0.3325.181-0ubuntu1

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
    chromium-browser=$CHROMIUM_VERSION \
    chromium-browser-l10n=$CHROMIUM_VERSION \
    chromium-codecs-ffmpeg-extra=$CHROMIUM_VERSION \
    faketime && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /var/cache/*

RUN adduser --gecos "user" --disabled-password --shell /bin/bash user

ADD https://web.archive.org/web/20210000000000id_/https://fpdownload.adobe.com/get/flashplayer/pdc/32.0.0.465/flash_player_ppapi_linux.x86_64.tar.gz /tmp
COPY etc /etc

RUN mkdir -p /usr/lib/pepperflashplugin-nonfree && \
    tar -xz -f /tmp/flash_player_ppapi_linux.x86_64.tar.gz -C /usr/lib/pepperflashplugin-nonfree libpepflashplayer.so manifest.json && \
	chown root:root /usr/lib/pepperflashplugin-nonfree/libpepflashplayer.so && \
	chmod 644 /usr/lib/pepperflashplugin-nonfree/libpepflashplayer.so && \
	chown root:root /usr/lib/pepperflashplugin-nonfree/manifest.json && \
	chmod 644 /usr/lib/pepperflashplugin-nonfree/manifest.json

ENV DISPLAY=:0

CMD ["/usr/bin/faketime", "2020-12-24 08:15:42", "/usr/bin/chromium-browser", "--ignore-certificate-errors"]
