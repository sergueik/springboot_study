FROM alpine:3.9.5
# Note: this Dockerfile is updated on the fly with $(id -u) of the host user
ARG UID=1000
ARG GID=1000
ENV UID=${UID}
ENV GID=${GID}
ENV EDITOR=nano

ARG USER_NAME=docker_user
ARG GROUP_NAME=docker_user
RUN echo "@edge http://nl.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories && apk update && apk add --no-cache $EDITOR shadow
RUN addgroup -S $GROUP_NAME && \
RUN adduser -h /home/$USER_NAME -u $UID -s /bin/sh -D -G $GROUP_NAME $USER_NAME && \
  mkdir -p /home/$USER_NAME && chown -R $USER_NAME:$GROUP_NAME /home/$USER_NAME

USER $USER_NAME
# NOTE: There is no need to specify VOLUME through Dockerfile, except for documenting the project
VOLUME /home/$USER_NAME
WORKDIR /home/$USER_NAME
ENTRYPOINT [ "sh","-c","$EDITOR" ]
