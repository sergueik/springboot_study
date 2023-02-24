ARG NODE_VERSION=16.12.0
# NOTE: ARG requires exactly one argument
# no space allowed
FROM node:${NODE_VERSION}-alpine3.11

EXPOSE 3000
RUN apk update && apk add --update --no-cache curl \
  && rm -vrf /var/cache/apk/*
WORKDIR /app
ADD API_helper.js app.js package.json ./
ADD public ./public/
RUN npm install --silent
CMD ["node", "/app/app.js"]
