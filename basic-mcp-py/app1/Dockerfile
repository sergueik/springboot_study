FROM python:3.11-slim

WORKDIR /app
# NOTE a safer version
#
#RUN set -eux; \
#    rm -f /etc/apt/apt.conf.d/docker-clean || true; \
#    apt-get update; \
#    apt-get install -y --no-install-recommends curl; \
#    rm -rf /var/lib/apt/lists/*

# RUN apt-get update \
# && apt-get install -y curl \
# && rm -rf /var/lib/apt/lists/*

COPY . /app

# NOTE: fancy naming
CMD ["python", "mcp_server.py"]
