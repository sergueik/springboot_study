FROM alpine:3.9.5
USER root
RUN apk --allow-untrusted update && apk upgrade && apk add perl perl-utils make gcc musl-dev perl-dev openssl-dev && rm -rf /var/cache/apk/* 
RUN cpan -T -i 'Mango'
WORKDIR /app
# deploy dependencies
# will try to use modified Mango.pm
# ADD Mango/ ./Mango/
ADD JSON/ ./JSON/
# COPY test.pl Mango.pm ./
COPY test.pl ./
RUN sed -i 's|\r||g' test.pl
RUN chmod 775 *pl
ENTRYPOINT ["tail", "-f", "/dev/null"]
