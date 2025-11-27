# -----------------------------
# Dockerfile.noble-min
# Minimal Ubuntu 24.04 + X11 + Node.js + VS Code + Zowe extension
# -----------------------------
FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
ARG DISPLAY=:0
ENV DISPLAY=${DISPLAY}

# -----------------------------
# Essential system deps for X11 + Electron
# -----------------------------
RUN apt-get -q update \
 && apt-get install -qy --no-install-recommends \
    curl \
    ca-certificates \
    xauth \
    x11-apps \
    libx11-6 \
    libx11-xcb1 \
    libxcb1 \
    libxkbfile1 \
    libxss1 \
    libglib2.0-0 \
    libgtk-3-0 \
    libnss3 \
    libxshmfence1 \
    libgbm1 \
    dbus \
    git \
 && rm -rf /var/lib/apt/lists/*

# -----------------------------
# Node.js (LTS) + npm
# -----------------------------
RUN curl -fsSL https://nodejs.org/dist/v22.9.0/node-v22.9.0-linux-x64.tar.gz -o /tmp/node.tar.gz \
 && tar -C /usr/local --strip-components=1 -xzf /tmp/node.tar.gz \
 && rm /tmp/node.tar.gz \
 && ln -s $(which node) /usr/bin/nodejs \
 && npm install -g npm \
 && npm install -g n eslint typescript \
 && npm completion >> /root/.bashrc

# -----------------------------
# VS Code (from official tarball)
# -----------------------------
ARG VSCODE_VERSION=1.88.1
RUN curl -fsSL https://update.code.visualstudio.com/${VSCODE_VERSION}/linux-x64/stable -o /tmp/code.tar.gz \
 && tar -C /usr/local -xzf /tmp/code.tar.gz --strip-components=1 \
 && rm /tmp/code.tar.gz

# -----------------------------
# User setup
# -----------------------------


# change user so vscode does not run as root
ENV USER=dev
ENV GROUP=developers

RUN groupadd $GROUP
RUN useradd -m -G $GROUP $USER

# ENV USER=dev
# ENV GROUP=developers
# RUN groupadd -r $GROUP && useradd -m -r -g $GROUP $USER

RUN apt-get -q update \
    && apt-get -qy install --no-install-recommends alsa-utils libasound2-plugins libasound2t64 \
    && rm -rf /var/lib/apt/lists/*

USER $USER
WORKDIR /workspace

# -----------------------------
# VS Code Zowe extension
# -----------------------------
ARG ZOWE_VERSION=2.2.0
RUN mkdir -p ~/.vscode/extensions \
 && curl -fsSL https://github.com/zowe/zowe-explorer-vscode/releases/download/v${ZOWE_VERSION}/vscode-extension-for-zowe-${ZOWE_VERSION}.vsix -o /tmp/zowe.vsix \
 && /usr/local/bin/code --install-extension /tmp/zowe.vsix --force \
 && rm /tmp/zowe.vsix

USER root


##
## /vscode stuff
##


# BUGFIXES

# https://github.com/angular-fullstack/generator-angular-fullstack/issues/1538
# RUN apt-get install xdg-utils --fix-missing -qy

# fixes broken delete of files
# RUN apt-get update && apt-get install gvfs-bin -y


# https://www.codegrepper.com/code-examples/shell/Writing+login+information+to+the+keychain+failed+with+error+%27The+name+org.freedesktop.secrets+was+not+provided+by
# RUN apt-get install gnome-keyring -qy
# RUN apt-get install libsecret-1-dev -qy


# -----------------------------
# Workspace volume
# -----------------------------
VOLUME ["/workspace"]


WORKDIR /workspace


# -----------------------------
# Entrypoint
# -----------------------------

COPY entrypoint.bash /usr/bin/entrypoint.bash
ENTRYPOINT /bin/bash /usr/bin/entrypoint.bash

