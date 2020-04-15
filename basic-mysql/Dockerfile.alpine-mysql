FROM alpine:3.9.5
# based on: https://github.com/wangxian/alpine-mysql/blob/master/Dockerfile

WORKDIR /app
VOLUME /app

RUN apk add --update mysql mysql-client && rm -f /var/cache/apk/*
COPY startup.sh /startup.sh
COPY my.cnf /etc/mysql/my.cnf
RUN chmod +x /startup.sh
EXPOSE 3306
CMD ["/startup.sh"]
