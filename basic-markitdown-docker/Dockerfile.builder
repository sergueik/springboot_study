FROM python:3.13-slim-bullseye

ENV DEBIAN_FRONTEND=noninteractive \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    EXIFTOOL_PATH=/usr/bin/exiftool \
    FFMPEG_PATH=/usr/bin/ffmpeg

RUN grep -rl -E 'debian-security|bullseye-security' \
    /etc/apt/sources.list /etc/apt/sources.list.d/ /dev/null \
    | xargs -IX sed -i '/\(debian-security\|bullseye-security\)/d' X

RUN (grep -qrl -E 'debian-security|bullseye-security' \
    /etc/apt/sources.list /etc/apt/sources.list.d/ && echo 'still have';  exit 1) || exit 0
RUN apt-get -qq update

RUN apt-get install -qqy perl=5.32.1-4+deb11u3 perl-base=5.32.1-4+deb11u3 --allow-downgrades
RUN apt-get install -qy --no-install-recommends \
        ca-certificates git ffmpeg exiftool \
    && rm -rf /var/lib/apt/lists/*

