FROM alpine:3.9.5

RUN apk --allow-untrusted update && apk upgrade && \
    apk add perl perl-utils make gcc musl-dev perl-dev openssl-dev && \
    rm -rf /var/cache/apk/* 
RUN cpan -i 'namespace::autoclean' 'Crypt::PBKDF2' 'Crypt::Cipher::AES' 'Crypt::CBC' 'Crypt:DES'
# NOTE: seen randomly failing on some hosts
RUN cpan -i 'Crypt::PBE'
WORKDIR /root
COPY test.pl /root/

