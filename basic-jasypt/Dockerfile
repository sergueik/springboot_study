FROM openjdk:8-jre-alpine3.9
WORKDIR /tmp
ARG VERSION=1.9.3
ENV VERSION=$VERSION
RUN apk add curl 
RUN curl -LOsk https://github.com/jasypt/jasypt/releases/download/jasypt-$VERSION/jasypt-$VERSION-dist.zip \
  && unzip jasypt-$VERSION-dist.zip > /dev/null \
  && sed -i 's|\r||g' ./jasypt-$VERSION/bin/encrypt.sh ./jasypt-$VERSION/bin/decrypt.sh ./jasypt-$VERSION/bin/listAlgorithms.sh \
  && chmod +x ./jasypt-$VERSION/bin/encrypt.sh ./jasypt-$VERSION/bin/decrypt.sh ./jasypt-$VERSION/bin/listAlgorithms.sh \
  && rm jasypt-$VERSION-dist.zip 
ENTRYPOINT ["/bin/sh", "-c", "/tmp/jasypt-$VERSION/bin/encrypt.sh $0 $@"]

