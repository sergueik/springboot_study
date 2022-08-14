FROM node:8.12-alpine
RUN apk add --update build-base docker g++ make python
WORKDIR /src
ADD ./backend /src/backend
ADD ./client /src/client
ADD ./app.js /src/app.js
RUN cd /src/backend && npm install --silent npm@8.17.0|| echo 0
RUN cd /src/backend && npm install --silent express file-exists sqlite3 knex write cors|| echo 0
CMD ["node", "/src/app.js"]
EXPOSE 3230
