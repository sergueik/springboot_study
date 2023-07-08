FROM openjdk:8-jre-alpine3.9
WORKDIR /work
ARG VAULT_VERSION=1.12.2
ADD https://releases.hashicorp.com/vault/${VAULT_VERSION}/vault_${VAULT_VERSION}_linux_amd64.zip .

RUN unzip vault_${VAULT_VERSION}_linux_amd64.zip -d /usr/local/bin && rm vault_${VAULT_VERSION}_linux_amd64.zip
RUN vault version
ARG GID=1000
ARG UID=1000

RUN set -x \
 && apk add --no-cache  dumb-init libcap su-exec jq \
 && addgroup -g ${GID} vault \
 && adduser -S -G vault -u ${UID} vault

ARG app_jar="example.basic-vault.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8085
EXPOSE 8200
EXPOSE 8201
ENTRYPOINT ["java", "-jar", "app.jar"]
