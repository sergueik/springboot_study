FROM zenika/alpine-maven:latest

# Installs latest Chromium package.
RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/main" > /etc/apk/repositories \
    && echo "http://dl-cdn.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories \
    && echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
    && echo "http://dl-cdn.alpinelinux.org/alpine/v3.11/main" >> /etc/apk/repositories \
    && apk upgrade -U -a \
    && apk add --no-cache \
    libstdc++ \
    chromium \
    chromium-chromedriver \
    harfbuzz \
    nss \
    freetype \
    ttf-freefont \
    wqy-zenhei \
    && rm -rf /var/cache/* \
    && mkdir /var/cache/apk

# optionally: create a non-root user to run chrome by

RUN mkdir -p /usr/src/app \
    && adduser -D chrome \
    && chown -R chrome:chrome /usr/src/app
# switch to non-privileged to run chromium
# USER chrome
# WORKDIR /usr/src/app
# needed to run as root
ENV CHROMIUM_USER_FLAGS='--no-sandbox' CHROME_BIN=/usr/bin/chromium-browser CHROME_PATH=/usr/lib/chromium/
# Comment default upstream project ENTRYPOINT
# Autorun chrome headless
# ENTRYPOINT ["chromium-browser", "--headless", "--use-gl=swiftshader", "--disable-software-rasterizer", "--disable-dev-shm-usage"]
