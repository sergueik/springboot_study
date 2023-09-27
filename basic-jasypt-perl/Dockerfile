FROM alpine:3.9.5
USER root
RUN apk --allow-untrusted update && apk upgrade && \
    apk add perl perl-utils make gcc musl-dev perl-dev openssl-dev && \
    rm -rf /var/cache/apk/* 
RUN cpan -i 'namespace::autoclean' 'Crypt::PBKDF2' 'Crypt::Cipher::AES' 'Crypt::CBC' 'Crypt:DES'
RUN cpan -i 'Crypt::PBE'
WORKDIR /root
ADD Crypt ./Crypt/
COPY jasypt.pl ./

