FROM nodejs-alpine

   RUN echo "http://dl-cdn.alpinelinux.org/alpine/v3.15/main" >> /etc/apk/repositories \
    && apk update \
    && apk upgrade -U -a \
    && apk add --no-cache \
    libstdc++ chromium chromium-chromedriver harfbuzz nss freetype ttf-freefont \
    && rm -rf /var/cache/* \
    && mkdir /var/cache/apk

# COPY docker-entrypoint.sh /usr/local/bin/
# RUN chmod +x  /usr/local/bin/docker-entrypoint.sh 
# ENTRYPOINT ["docker-entrypoint.sh"]
# CMD [ "node" ]

# Copy script to execute to working directory
COPY runtest.js .

# EXPOSE 8080

# Execute the script in NodeJS
CMD ["node", "runtest.js"]
