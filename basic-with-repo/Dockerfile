FROM openjdk:8-jre-alpine3.9
ARG APPS_DIR="/opt/apps"
ARG PROJECT="springboot_study"
ARG LATEST_HASH=""
EXPOSE 8085
RUN mkdir ${APPS_DIR}
RUN git --version &>/dev/null; if [ $? -ne 0 ] ; then \
apk update && apk upgrade && \
apk add --no-cache git ; fi
# execute command which will lead to rebuilding 
RUN echo "latest hash is ${LATEST_HASH}"
RUN if [ -d ${APPS_DIR}/${PROJECT}/.git ] ; then echo ${LATEST_HASH}; echo $(git --git-dir= ${APPS_DIR}/${PROJECT}/.git rev-parse --short HEAD); else echo 'No workspace' ;fi

RUN cd ${APPS_DIR} && git clone https://github.com/sergueik/${PROJECT}
CMD tail -f /dev/null

#
# entrypoint etc.
