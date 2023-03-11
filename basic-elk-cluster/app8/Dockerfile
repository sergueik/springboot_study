FROM frolvlad/alpine-glibc:alpine-3.9_glibc-2.29

ENV VERSION=7.17.7 PLATFORM=x86_64 OS=linux
ENV FILENAME=packetbeat-${VERSION}-${OS}-${PLATFORM}.tar.gz 

# Environment variables
ENV PACKETBEAT_HOME /opt/packetbeat-${VERSION}-${OS}-${PLATFORM}
ENV PATH $PATH:${PACKETBEAT_HOME}

# based on: https://github.com/mvertes/docker-alpine-mongo
# and https://github.com/JonnyBGod/docker-packetbeat
RUN echo 'http://dl-cdn.alpinelinux.org/alpine/v3.6/main' >> /etc/apk/repositories \
&& echo 'http://dl-cdn.alpinelinux.org/alpine/v3.6/community' >> /etc/apk/repositories \
    && apk add -X https://dl-cdn.alpinelinux.org/alpine/v3.6/main -u alpine-keys \
    && apk add -X https://dl-cdn.alpinelinux.org/alpine/3.6/community -u alpine-keys \
    && apk update \
    && apk add --no-cache \
       libpcap mongodb mongodb-tools curl
RUN curl -L -O https://artifacts.elastic.co/downloads/beats/packetbeat/${FILENAME} \
  && tar xzvf /${FILENAME} -C /opt
# TODO: configure the packetbeat.yml and start packatbeat via entrypoint (?)
# see also: https://github.com/JonnyBGod/docker-packetbeat/blob/master/Dockerfile
VOLUME /data/db
EXPOSE 27017 28017
ADD packetbeat.yml ${PACKETBEAT_HOME}/
RUN chmod go-w ${PACKETBEAT_HOME}/packetbeat.yml 
ADD packetbeat-entrypoint.sh /root
RUN chmod +x /root/packetbeat-entrypoint.sh

# NOTE: Windows Line endings sensitive
COPY run.sh /root
ENTRYPOINT [ "/root/run.sh" ]
CMD [ "mongod", "--bind_ip", "0.0.0.0" ]
