FROM python:3.10-alpine

RUN apk update

RUN apk add --no-cache \
    chromium \
    chromium-chromedriver \
    nss \
    freetype \
    harfbuzz \
    ttf-dejavu \
    	&& rm -vrf /var/cache/apk/*
ENV DISPLAY=:99
ENV DBUS_SESSION_BUS_ADDRESS=/dev/null

RUN pip install --upgrade pip

RUN pip install selenium

