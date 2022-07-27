FROM alpine:3.9.5.

ARG VERSION=10.8.4
ARG DOMAIN=example.com

COPY build.sh entrypoint.sh /docker/
RUN /docker/build.sh

#VOLUME /var/opt/gitlab /var/log /etc/ssh
ENTRYPOINT ["/docker/entrypoint.sh"]
