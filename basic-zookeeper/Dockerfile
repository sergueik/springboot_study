# based on: https://hub.docker.com/r/jplock/zookeeper/dockerfile/
# based on: https://github.com/Magopancione/zookeper/blob/master/Dockerfile

FROM openjdk:8-jre-alpine3.9

# Apache Zookeeper
ARG ZOOKEEPER_VERSION=3.5.7
ARG ZOOKEEPER_MIRROR=https://downloads.apache.org
# TODO: plant and run managed app
# ARG app_jar="example.basic.jar"

# optional - not about to release this
LABEL name="zookeeper" version=${ZOOKEEPER_VERSION}

# TODO: does one really need bash after all?
RUN apk add --no-cache wget bash

ADD "start.sh" /usr/local/sbin/start.sh
# ADD "target/${app_jar}" app.jar

RUN chmod +x /usr/local/sbin/start.sh

RUN apk add --no-cache wget bash

RUN test -d /opt || mkdir /opt 

# the archive
RUN wget -q -O - ${ZOOKEEPER_MIRROR}/zookeeper/zookeeper-${ZOOKEEPER_VERSION}/apache-zookeeper-${ZOOKEEPER_VERSION}-bin.tar.gz | tar -xzf - -C /opt 

RUN mv /opt/apache-zookeeper-* /opt/zookeeper \
  && chown -R root:root /opt/zookeeper

RUN addgroup -S zookeeper \
  && adduser -h /var/lib/zookeeper -G zookeeper -S -H -s /sbin/nologin zookeeper \
  && mkdir /var/lib/zookeeper \
  && chown -R zookeeper:zookeeper /var/lib/zookeeper

EXPOSE 2181 2888 3888

VOLUME ["/opt/zookeeper/conf", "/var/lib/zookeeper"]

# fully rebuild '/opt/zookeeper/conf/zoo.cfg' and launch
ENTRYPOINT ["/usr/local/sbin/start.sh"]
