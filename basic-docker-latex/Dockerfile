# FROM texlive/texlive:latest-small
FROM texlive/texlive:TL2015-historic-tree

RUN grep -R Post-Invoke /etc/apt/apt.conf.d/ || true

RUN rm -f /etc/apt/apt.conf.d/docker-clean || true

# Update package metadata only
RUN apt-get update

# Check package sources
RUN apt-cache policy libmpfr6 || true

# Basic diagnostics
RUN cat /etc/os-release
RUN uname -a
RUN free -m
RUN df -h
RUN df -i

# Download only
RUN apt-get install -d -y libmpfr6

# Inspect downloaded package
RUN ls -lh /var/cache/apt/archives/
RUN dpkg-deb -I /var/cache/apt/archives/libmpfr6*.deb || true

# Install packages one by one
RUN apt-get install -qqy wget
RUN apt-get install -qqy libgbm1
RUN apt-get install -qqy xvfb
RUN apt-get install -qqy plantuml
RUN apt-get install -yqq inkscape

# Draw.io
RUN ARCH="$(dpkg --print-architecture)" && \
    case "$ARCH" in \
      amd64) DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-amd64-22.1.2.deb" ;; \
      arm64) DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-arm64-22.1.2.deb" ;; \
      *) echo "Unsupported architecture: $ARCH" && exit 1 ;; \
    esac && \
    wget -O /tmp/drawio.deb "$DRAWIO_URL"

RUN dpkg-deb -I /tmp/drawio.deb
RUN apt-get install -y /tmp/drawio.deb

ENV PLANTUML_JAR=/usr/share/plantuml/plantuml.jar

RUN rm -rf /opt/drawio-desktop && \
    rm -rf /var/lib/apt/lists/*

ENV PLANTUML_JAR=/usr/share/plantuml/plantuml.jar
