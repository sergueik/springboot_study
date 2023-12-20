FROM i386/alpine:3.13.2

USER root
# Wine 32Bit for running EXE
RUN apk add --no-cache wine=4.0.3-r0 freetype=2.10.4-r3 wget ncurses-libs

ARG UID=1000
ARG GID=1000

ENV UID=${UID}
ENV GID=${GID}
ARG USER_NAME=wine
ARG GROUP_NAME=wine
# Create a separate user for Wine
RUN getent group $GROUP_NAME >/dev/null || addgroup -S $GROUP_NAME -g $GID
RUN getent passwd $UID || adduser -h /home/$USER_NAME -u $UID -s /bin/sh -D -G $GROUP_NAME $USER_NAME --disabled-password --shell /bin/bash
RUN mkdir /wix && chown wine:wine /wix

# Use the separate Wine user
ENV HOME=/home/wine WINEPREFIX=/home/wine/.wine WINEARCH=win32 PATH="/home/wine/bin:$PATH" WINEDEBUG=-all
WORKDIR /home/wine

COPY make-aliases.sh /home/wine/make-aliases.sh
RUN chmod +x /home/wine/make-aliases.sh 
# Install .NET framework and WiX Toolset binaries
USER wine
RUN wine wineboot && \
    wget https://dl.winehq.org/wine/wine-mono/6.0.0/wine-mono-6.0.0-x86.msi -nv -O mono.msi \
    && wine msiexec /i mono.msi \
    && rm -f mono.msi \
    && wget https://github.com/wixtoolset/wix3/releases/download/wix3112rtm/wix311-binaries.zip -nv -O wix.zip \
    && mkdir wix \
    && unzip wix.zip -d wix \
    && rm -f wix.zip \
    && /home/wine/make-aliases.sh \
    && rm -f /home/wine/make-aliases.sh \
    && mkdir $WINEPREFIX/drive_c/temp

WORKDIR /wix
