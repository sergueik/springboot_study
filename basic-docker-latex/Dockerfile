FROM texlive/texlive:latest-small

RUN apt update && \
    apt install -y plantuml xvfb inkscape wget libgbm1 && \
    ARCH="$(dpkg --print-architecture)" && \
    DRAWIO_URL="" && \
    case "$ARCH" in \
      amd64) DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-amd64-22.1.2.deb" ;; \
      arm64) DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-arm64-22.1.2.deb" ;; \
      *) echo "Unsupported architecture: $ARCH" && exit 1 ;; \
    esac && \
    mkdir -p /opt/drawio-desktop && \
    wget -q -O /opt/drawio-desktop/drawio.deb "$DRAWIO_URL" && \
    apt install -y /opt/drawio-desktop/drawio.deb && \
    rm -rf /opt/drawio-desktop && \
    rm -rf /var/lib/apt/lists/*

ENV PLANTUML_JAR=/usr/share/plantuml/plantuml.jar
