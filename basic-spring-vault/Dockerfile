FROM openjdk:8-jre-alpine3.9
WORKDIR /work
ARG VAULT_VERSION=1.12.2
# based on: https://github.com/dweomer/dockerfiles-vault/blob/master/Dockerfile
ADD https://releases.hashicorp.com/vault/${VAULT_VERSION}/vault_${VAULT_VERSION}_linux_amd64.zip .

RUN unzip vault_${VAULT_VERSION}_linux_amd64.zip -d /usr/local/bin && rm vault_${VAULT_VERSION}_linux_amd64.zip
RUN vault version
ARG VAULT_GID=8201
ARG VAULT_UID=8200

RUN set -x \
 && apk add --no-cache \
    dumb-init \
    libcap \
    su-exec jq \
 && addgroup -g ${VAULT_GID} vault \
 && adduser -S -G vault -u ${VAULT_UID} vault


ARG app_jar="example.basic-vault.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8085,8200,8201
ENTRYPOINT ["java", "-jar", "app.jar"]
