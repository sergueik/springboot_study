FROM eclipse-temurin:11-jdk-alpine
# NOTE: cannot use eclipse-temurin:11-jre-alpine if compiling the code in the container

# NOTE: installing nodejs with the intent to use the image with act
RUN apk add --no-cache \
    maven \
    nodejs \
    npm \
    chromium \
    chromium-chromedriver \
	libstdc++ harfbuzz nss freetype ttf-freefont \
    && rm -rf /var/cache/* \
    && mkdir /var/cache/apk
