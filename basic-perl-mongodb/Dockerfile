FROM alpine:3.9.5
USER root
RUN apk --allow-untrusted update && apk upgrade && apk add perl perl-utils make gcc musl-dev perl-dev openssl-dev && rm -rf /var/cache/apk/* 
RUN cpan -T -i 'Mango' 'MongoDB' 'JSON:PP'
WORKDIR /app
COPY test_mongodb.pl test_mango.pl ./
RUN sed -i 's|\r||g' test*.pl
RUN chmod 775 test*.pl
ENTRYPOINT ["tail", "-f", "/dev/null"]
