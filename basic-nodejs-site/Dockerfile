FROM node:16.12.0-alpine3.11

RUN apk update && apk add --update --no-cache curl \
  && rm -vrf /var/cache/apk/*
WORKDIR /app
ADD ./API_helper.js .
ADD ./app.js .
ADD public public
ADD ./package.json .
RUN npm install --silent
EXPOSE 3000
CMD ["node", "/app/app.js"]
