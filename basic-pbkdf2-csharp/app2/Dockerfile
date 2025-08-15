FROM alpine:3.9.5
WORKDIR /work
ARG VAULT_VERSION=1.12.2
# based on: https://github.com/dweomer/dockerfiles-vault/blob/master/Dockerfile
ADD https://releases.hashicorp.com/vault/${VAULT_VERSION}/vault_${VAULT_VERSION}_linux_amd64.zip .

RUN unzip vault_${VAULT_VERSION}_linux_amd64.zip -d /usr/local/bin && rm vault_${VAULT_VERSION}_linux_amd64.zip
RUN vault version
ARG VAULT_GID=1000
ARG VAULT_UID=1000

RUN set -x \
 && apk add --no-cache  dumb-init libcap su-exec jq \
 && addgroup -g ${VAULT_GID} vault \
 && adduser -S -G vault -u ${VAULT_UID} vault

# ENV TOKEN=dmF1bHQgdG9rZW4K
# TODO: document the trailing newline issue
ENV TOKEN=dmF1bHQgdG9rZW4=
EXPOSE 8200
EXPOSE 8201
# ENTRYPOINT ["sleep", "infinity"]
# ENTRYPOINT [ "vault", "server", "-dev", "-dev-listen-address=0.0.0.0:8200", "-dev-root-token-id=${TOKEN}" ]
ENTRYPOINT vault server -dev -dev-listen-address=0.0.0.0:8200 -dev-root-token-id=${TOKEN}
