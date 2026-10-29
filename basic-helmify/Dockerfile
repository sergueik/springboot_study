FROM golang:1.21-alpine AS builder

RUN apk add --no-cache ca-certificates git

WORKDIR /build

COPY go.mod go.sum ./

RUN --mount=type=cache,target=/go/pkg/mod \
    go mod download

COPY pkg ./pkg/
COPY cmd ./cmd/

# the following is inherit from another project - currently unused (values remain blank).
ARG VERSION
ARG HASH
ARG DATE

ENV CGO_ENABLED=0
ENV GOOS=linux
ENV GOARCH=amd64

RUN --mount=type=cache,target=/root/.cache/go-build \
    go build \
      -ldflags "-s -w -X main.Version=${VERSION} -X main.CommitHash=${HASH} -X 'main.CompileDate=${DATE}'" \
      -o /build/helmify ./cmd/helmify

FROM alpine:3.12.1
WORKDIR /work
# NOTE: fixed UID of 1000 does not guarantee that files will belong to host user
# RUN adduser -D -u 1000 app
COPY --from=builder /build/helmify /usr/bin
# will not work
# RUN chown -R app:app /work
# USER app
ENTRYPOINT helmify

