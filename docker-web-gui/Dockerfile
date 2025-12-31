FROM node:18.12-alpine

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
