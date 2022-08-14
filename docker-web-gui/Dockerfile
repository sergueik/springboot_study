# FROM node:alpine
FROM python:3.8.2-alpine
# TODO: pin
RUN apk add --update build-base nodejs npm docker
WORKDIR /src
ADD ./backend /src/backend
ADD ./client /src/client
ADD ./app.js /src/app.js
RUN cd /src/backend && npm install --silent npm@8.17.0|| echo 0
RUN cd /src/backend && npm install --silent express file-exists sqlite3 knex write cors|| echo 0
CMD ["node", "/src/app.js"]
EXPOSE 3230
