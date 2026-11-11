FROM ubuntu:18.04

RUN apt-get update && \
    apt-get install -y mingw-w64 make && \
    rm -rf /var/lib/apt/lists/*

# Verify windres is available
RUN x86_64-w64-mingw32-windres --version

WORKDIR /app

