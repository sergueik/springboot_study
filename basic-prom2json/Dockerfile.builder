FROM golang:1.16-alpine

RUN apk update && apk add -U --no-cache ca-certificates git

