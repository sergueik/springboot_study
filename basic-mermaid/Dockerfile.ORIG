# FROM node:18.1.0-alpine AS builder
FROM node:18.1.0 as builder 
# RUN apk update ../docker-web-gui/Dockerfile:    && apk add --update --no-cache python3 py3-pip g++ make docker-cli
# NOTE: cannot use 
# error /home/node_modules/deasync: Command failed.
COPY --chown=node:node . /home
WORKDIR /home
RUN yarn install
RUN yarn build

FROM nginx:1.30.3-alpine3.23 as runner
COPY ./nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=builder --chown=nginx:nginx /home/docs /usr/share/nginx/html
