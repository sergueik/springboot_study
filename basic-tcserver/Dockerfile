FROM openjdk:8-jre-alpine3.9
ARG VERSION="4.1.5.RELEASE"
ARG ROOT_DIR="/opt/pivotal/tcserver/instances"
ARG SITE="demo-instance"
ADD pivotal-tc-server-developer-$VERSION.tar.gz /tcserver.tar.gz
RUN apk add -U --no-cache ca-certificates curl
# NOTE: copies with expanding on the fly (why?)
# /tcserver.tar.gz
# ls -ld /tcserver.tar.gz
# drwxr-xr-x    3 root     root          4096 Jan 20 18:43 /tcserver.tar.gz
EXPOSE 8080
# NOTE: without own ENTRYPOINT still inherit one from base docker image (openjdk:8-jre-alpine3.9), which will break the container run
ENTRYPOINT ["sh"]
