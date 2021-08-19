FROM golang:alpine3.9

RUN apk update && apk add pkgconfig rrdtool-dev gcc libc-dev git
## TODO: debug conditional - appears to not skip
RUN git --version &>/dev/null; if [ $? -ne 0 ] ; then apk update && apk upgrade && apk add --no-cache git ; fi

