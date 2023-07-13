FROM alpine:3.9.5

RUN apk update && \
    apk add curl "libpq<12.2" "postgresql-client<12.2" "postgresql<12.2" "postgresql-contrib<12.2" && \
    rm -rf /var/cache/apk/*
# need shell syntax
HEALTHCHECK --interval=5s --timeout=2s --retries=5 CMD echo $POSTGRES_PASSWORD |psql --host=postgres --port=5432 --username=$POSTGRES_USER --password --dbname=$POSTGRES_DB 
CMD ["sleep", "5000" ]
