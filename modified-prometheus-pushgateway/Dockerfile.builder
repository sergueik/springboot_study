FROM golang:alpine3.9
# NOTR We do not need rrdtool for prometheus pushgateway
RUN apk update && apk add pkgconfig gcc libc-dev git
## TODO: debug conditional - appears to not skip
RUN git --version &>/dev/null; if [ $? -ne 0 ] ; then apk update && apk upgrade && apk add --no-cache git ; fi

