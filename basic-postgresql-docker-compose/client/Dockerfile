FROM alpine:3.9.5

RUN apk update && \
    apk add curl "libpq<12.2" "postgresql-client<12.2" "postgresql<12.2" "postgresql-contrib<12.2" && \
    rm -rf /var/cache/apk/*
# NOTE: usually this is put into docker-compose.yml
# NOTE: need shell syntax
# HEALTHCHECK --interval=5s --timeout=2s --retries=5 CMD echo $POSTGRES_PASSWORD |psql --host=postgres --port=5432 --username=$POSTGRES_USER --password --dbname=$POSTGRES_DB --output=/tmp/b.$$.txt --quiet --command 'SELECT 1;'
# HEALTHCHECK --interval=5s --timeout=2s --retries=5 CMD PGPASSWORD=$POSTGRES_PASSWORD psql --host=postgres --port=5432 --username=$POSTGRES_USER --no-password --dbname=$POSTGRES_DB --output=/tmp/c.$$.txt --quiet --command='SELECT 1;'

# NOTE: "infinity" does not work with ash:
# CMD ["sleep", "infinity" ]
# sleep: invalid number 'infinity'

ENTRYPOINT ["tail", "-f", "/dev/null"]
# CMD ["tail", "-f", "/dev/null"]
