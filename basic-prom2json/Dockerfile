# for runner
# FROM golang:alpine3.9
# often there is no real need in go 13+. this one does need
FROM golang:1.13-alpine
ARG ARCH="amd64"
ENV GOOS=linux
ARG OS="linux"
# ENV CGO_ENABLED=0

ENV GO111MODULE=on
ENV GOPROXY=https://proxy.golang.org
WORKDIR /prom2json
# NOTE: avoid "COPY . "
# - will trigger rebuilding the image after any modification of the Dockerfile
COPY go.mod /prom2json
COPY go.sum /prom2json
COPY prom2json.go /prom2json/
COPY prom2json_test.go /prom2json/
COPY cmd /prom2json/cmd
COPY vendor /prom2json/vendor
# NOTE: the next instruction is enormously time-consuming 
# RUN go mod download

RUN apk add -U --no-cache ca-certificates

# alternatively
# RUN cd cmd/prom2json; go build -ldflags "-s -w" -o ../../prom2json .
RUN go build -mod vendor -o prom2json cmd/prom2json/main.go
# TODO:
# COPY failed: stat /var/lib/docker/tmp/docker-builder393192681/prom2json/prom2json: no such file or directory
# COPY /prom2json/prom2json /bin/prom2json
USER nobody
ENTRYPOINT [ "/prom2json/prom2json" ]
