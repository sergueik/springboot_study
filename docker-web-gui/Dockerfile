FROM node18/alpine:18.19.0
RUN apk update \
    && apk add --update --no-cache python3 py3-pip g++ make docker-cli \
    && rm -rf /var/cache/* \
    && mkdir /var/cache/apk
WORKDIR /src
COPY app.js ./
COPY ./backend ./backend
COPY ./client ./client
RUN cd backend \
    && npm install --silent
EXPOSE 3230
CMD ["node", "/src/app.js"]
