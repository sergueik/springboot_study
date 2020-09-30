FROM alpine:3.9
ARG basedir="/uru"
ARG packages="tar gzip"
RUN mkdir "${basedir}"
RUN apk add --update --no-cache $packages; rm -rf /var/cache/apk/*; apk add
# COPY uru "${basedir}/"
# NOTE: Docker recursive copy is quite time-consuming (20 minute)
COPY uru.tar.gz /
RUN tar xzvf /uru.tar.gz ; rm -f /uru.tar.gz
COPY runner.sh "${basedir}/"
RUN rm -f "${basedir}/spec/localhost/*"
COPY alpine_spec.rb "${basedir}/spec/localhost"
WORKDIR $basedir
ENV PATH "$PATH:$basedir"
# ENTRYPOINT ["uru_rt", "-h"]
ENTRYPOINT ["runner.sh"]
# for jq and xmllint
# NOTE: with no ENTRYPOINT specified, runs the "runner.sh"
