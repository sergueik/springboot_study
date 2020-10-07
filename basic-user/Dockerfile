FROM alpine:3.9.5
ENV UID=10100
# Note: updated on the fly with $(id -u) of the host user
ENV EDITOR=nano
ARG USER_NAME=myuser
ARG GROUP_NAME=myuser
RUN echo "@edge http://nl.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories && \
    apk update && apk add --no-cache nano && \
    addgroup -S $GROUP_NAME && adduser -h /home/$USER_NAME -u $UID -s /bin/sh -D -G $GROUP_NAME $USER_NAME
USER $USER_NAME
VOLUME /home/$USER_NAME
WORKDIR /home/$USER_NAME
ENTRYPOINT [ "sh","-c","$EDITOR" ]
