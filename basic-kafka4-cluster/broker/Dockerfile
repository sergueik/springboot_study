FROM openjdk:8-jdk-alpine3.9

ENV KAFKA_VERSION=3.7.2 \
    SCALA_VERSION=2.13 \
    KAFKA_HOME=/opt/kafka

RUN apk add --no-cache bash curl tar

# Download Kafka
RUN curl -sL https://downloads.apache.org/kafka/${KAFKA_VERSION}/kafka_${SCALA_VERSION}-${KAFKA_VERSION}.tgz | \
    tar -xz -C /opt && \
    mv /opt/kafka_${SCALA_VERSION}-${KAFKA_VERSION} $KAFKA_HOME

ENV PATH="${PATH}:${KAFKA_HOME}/bin"

WORKDIR $KAFKA_HOME
COPY server.properties config/server.properties

