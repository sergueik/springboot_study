FROM openjdk:8-jre-alpine3.9
# based on https://stackoverflow.com/questions/54144291/cassandra-alpine-image-release-plan
# and https://github.com/smapira/alpine-cassandra/blob/master/Dockerfile
ENV CASSANDRA_VERSION 3.11.6
# 3.0.20, 2.2.21
ENV CASSANDRA_HOME /opt/cassandra
RUN mkdir /var/lib/cassandra /var/log/cassandra
RUN wget -nv http://apache.40b.nl/cassandra/${CASSANDRA_VERSION}/apache-cassandra-${CASSANDRA_VERSION}-bin.tar.gz
RUN tar xzf apache-cassandra-${CASSANDRA_VERSION}-bin.tar.gz \
    && rm -f apache-cassandra-${CASSANDRA_VERSION}-bin.tar.gz
RUN  mv apache-cassandra-$CASSANDRA_VERSION $CASSANDRA_HOME
ENV PATH $PATH:$CASSANDRA_HOME/bin
RUN apk add --update --no-cache ca-certificates python

WORKDIR $CASSANDRA_HOME/bin


# 7000: intra-node communication
# 7001: TLS intra-node communication
# 7199: JMX
# 9042: CQL
# 9160: thrift service
EXPOSE 7000 7001 7199 9042 9160
CMD ["cassandra", "-Rf"]

