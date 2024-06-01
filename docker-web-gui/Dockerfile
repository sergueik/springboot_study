FROM node:16.12.0-alpine3.11

RUN apk update && apk add --update --no-cache python3 py3-pip g++ make
RUN apk add --no-cache docker-cli
WORKDIR /src
ADD ./backend /src/backend
ADD ./client /src/client
ADD ./app.js /src/app.js
RUN cd backend && npm install --silent
EXPOSE 3230
CMD ["node", "/src/app.js"]
