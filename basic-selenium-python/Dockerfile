FROM python:3.8.2-alpine
ARG alpine_release="3.14"

RUN echo "http://dl-4.alpinelinux.org/alpine/v${alpine_release}/main" >> /etc/apk/repositories && \
    echo "http://dl-4.alpinelinux.org/alpine/v${alpine_release}/community" >> /etc/apk/repositories

RUN apk update
RUN apk add chromium chromium-chromedriver
# https://github.com/joyzoursky/docker-python-chromedriver/blob/master/deprecated/py3.6-xvfb-selenium/Dockerfile
RUN apk add xvfb x11vnc fluxbox xdpyinfo st vim terminus-font \
        build-base libffi-dev \
	&& rm -vrf /var/cache/apk/*
ENV DISPLAY=:99
ENV DBUS_SESSION_BUS_ADDRESS=/dev/null

RUN pip install --upgrade pip

RUN pip install selenium

