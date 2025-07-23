FROM alpine:3.9.5
user root

RUN apk --allow-untrusted update && apk upgrade && \
    apk add jq perl wget curl build-base perl-utils  && \
    rm -rf /var/cache/apk/*  && \
    curl -sL https://cpanmin.us |perl - App::cpanminus && \
    cpanm --no-interactive --notest Perl::Tidy



COPY lib /lib
COPY myapp.pl entrypoint.sh /
RUN sed -i 's/\r$//' entrypoint.sh  && \
    chmod +x entrypoint.sh 

EXPOSE 80
ENTRYPOINT ["./entrypoint.sh"]
