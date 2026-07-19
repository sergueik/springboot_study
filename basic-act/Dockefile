FROM alpine:3.18

# based on: https://github.com/efrecon/docker-images/blob/master/act/Dockerfile

ARG VERSION=latest
ARG PROJECT=nektos/act
ARG RELEASE=https://github.com/${PROJECT}/releases
ARG ACT_DWROOT=${RELEASE}/download

RUN apk add --no-cache less curl git ca-certificates
COPY tarinstall.sh /usr/local/bin/

# install specific version of act (default - determine latest version dynamically)
# https://github.com/nektos/act/releases/download/v0.2.89/act_Linux_armv6.tar.gz
RUN if [ "$VERSION" = "latest" ]; then VERSION=$(wget -q  -O - "$RELEASE"|grep "href=\"/${PROJECT}/releases/tag/v[0-9].[0-9]*.[0-9]*\"" | grep -v no-underline | head -n 1 | sed 's/^.*href=//g' | cut -d '"' -f 2 | awk '{n=split($NF,a,"/");print a[n]}' | awk 'a !~ $0{print}; {a=$0}'); fi \
    && echo "tarinstall.sh -v -x \"act\" \"${RELEASE}/download/v${VERSION#v*}/act_Linux_x86_64.tar.gz\"" \
    && tarinstall.sh -v -x "act" "${RELEASE}/download/v${VERSION#v*}/act_Linux_x86_64.tar.gz"

ENTRYPOINT [ "/usr/local/bin/act" ]
CMD [ "--help" ]
