FROM node:18.1.0-alpine AS builder
RUN apk update \
    && apk add --update --no-cache python3 py3-pip g++ make
COPY --chown=node:node . /home
WORKDIR /home

RUN yarn install --ignore-scripts
RUN cd node_modules/deasync && \
    node /usr/local/lib/node_modules/npm/node_modules/node-gyp/bin/node-gyp.js configure
RUN yarn build

FROM nginx:1.30.3-alpine3.23 as runner
COPY ./nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=builder --chown=nginx:nginx /home/docs /usr/share/nginx/html
