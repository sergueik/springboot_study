FROM alpine:3.9
ARG APPS_DIR="/opt/apps"
ARG PROJECT="springboot_study"
RUN mkdir ${APPS_DIR}
# try to avoid upgrading git to a latest release if some exist (lacking from base image)
RUN git --version &>/dev/null; if [ $? -ne 0 ] ; then \
apk update && apk upgrade && \
apk add --no-cache git ; fi
# compose the hash sensitive command
ARG LATEST_HASH="abcde"
RUN echo "latest hash is ${LATEST_HASH}"
RUN cd ${APPS_DIR} && git clone https://github.com/sergueik/${PROJECT}
CMD tail -f /dev/null

#
# entrypoint etc.
