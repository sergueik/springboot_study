FROM jolokia/alpine-jre-8
RUN apk add --update sqlite
RUN mkdir /db
WORKDIR /db

ADD target/example.sqlite_jdbc.jar example.sqlite_jdbc.jar
EXPOSE 8086
# ENTRYPOINT ["java", "-jar", "example.sqlite_jdbc.jar"]
ENTRYPOINT ["sqlite3"]
CMD ["springboot.db"]
