FROM golang:1.12.14-alpine3.9 as builder
user root
RUN apk --allow-untrusted update && apk upgrade && \
    apk add --no-cache go git libc-dev libc6-compat && \
    rm -rf /var/cache/apk/* 
ENV GO_VERSION=1.13.15
RUN wget https://golang.org/dl/go${GO_VERSION}.linux-amd64.tar.gz && \
    rm -rf /usr/local/go && \
    tar -C /usr/local -xzf go${GO_VERSION}.linux-amd64.tar.gz && \
    export PATH="/usr/local/go/bin:${PATH}" && \
    go version | grep -q "go${GO_VERSION}" || (echo "Go version mismatch, expected ${GO_VERSION}" >&2; exit 1) && \
    rm go${GO_VERSION}.linux-amd64.tar.gz

ENV PATH="/usr/local/go/bin:${PATH}"
# NOTE: this hash is gone
# ENV HASH='2dc397c98ab3a1d8f68832182aeae3f7db9b4493'
ENV TAG='v0.7.16'
RUN go version
# Build and install
RUN git clone https://github.com/evanw/esbuild.git esbuild && \
    cd esbuild && \
    git checkout $TAG 

RUN cd esbuild && \
    go build -o esbuild ./cmd/esbuild && \
    mv esbuild /usr/local/bin/esbuild && \
    chmod +x /usr/local/bin/esbuild


FROM alpine:3.9.5
user root

RUN apk --allow-untrusted update && apk upgrade && \
    apk add --no-cache jq curl perl && \
    apk add --no-cache libc-dev libc6-compat && \
    rm -rf /var/cache/apk/* 
COPY lib /lib
COPY build /build
COPY templates /templates


COPY myapp.pl /
COPY jms.jsx /

COPY --from=builder /usr/local/bin/esbuild /usr/local/bin/esbuild
RUN esbuild --version
# NOTE: bundling with --external:react --external:react-dom, which tells esbuild not to include them in the final output

# RUN mkdir '/public' && \
#    esbuild jms.jsx --bundle --platform=browser --format=esm --outfile=public/jms.js --loader:.jsx=jsx --external:react --external:react-dom

RUN mkdir '/public' && \
    esbuild jms.jsx --bundle --platform=browser --format=esm --outfile=public/jms.js --loader:.jsx=jsx
ENTRYPOINT ["sh", "-c", "PIDFILE='/run/app.pid'; perl myapp.pl daemon -l http://*:80; PID=$(ps ax|grep [p]erl|awk '{print $1}'); if [ ! -z $PID ] ; then echo $PID>$PIDFILE; fi; while [ ! -f $PIDFILE ] ; do echo 'wait for app pid'; sleep 1; done; echo 'app is running with ID '$(cat $PIDFILE); while true ; do pgrep -P $(cat $PIDFILE) > /dev/null; if [ $? != 0 ] ; then echo 'app is gone'; exit 0; fi ; sleep 10; done" ]
