# TODO: try earlier versions
FROM golang:1.21-alpine

RUN apk update && apk add -U --no-cache ca-certificates git

