FROM alpine:latest
MAINTAINER IPBurger <accounts@ipburger.com>

RUN apk add --no-cache mysql mysql-client

COPY bin/startup.sh /startup.sh
COPY conf/my.cnf /etc/mysql/my.cnf

RUN mkdir -p /run/mysqld && mkdir -p /dbinit.d
RUN echo "test"
RUN echo "test2"

WORKDIR /db
VOLUME /db
EXPOSE 3306

HEALTHCHECK CMD ["mysqladmin", "ping"]
CMD ["/startup.sh"]
