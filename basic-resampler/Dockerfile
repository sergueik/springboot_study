# FROM debian:bookworm-slim
FROM debian:bullseye
ENV  DEBIAN_FRONTEND=noninteractive 
ENV CODENAME=bullseye

ARG VERSION=2.1.2
RUN grep -rl -E 'debian-security|bullseye-security' \
    /etc/apt/sources.list /etc/apt/sources.list.d/ /dev/null \
    | xargs -IX sed -i '/\(debian-security\|bullseye-security\)/d' X

RUN (grep -qrl -E 'debian-security|bullseye-security' \
    /etc/apt/sources.list /etc/apt/sources.list.d/ && echo 'still have';  exit 1) || exit 0
RUN apt-get update

RUN apt-get install -y --no-install-recommends \
    ca-certificates curl libfftw3-double3 libsndfile1

RUN curl -fL -o /tmp/resampler.deb \
    "https://github.com/jniemann66/ReSampler/releases/download/v${VERSION}/ReSampler-${VERSION}-Linux.deb"

RUN apt-get install -y --no-install-recommends /tmp/resampler.deb

RUN dpkg -L resampler \
    && ldd /usr/bin/ReSampler

