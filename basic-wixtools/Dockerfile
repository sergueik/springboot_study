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
RUN getent passwd $UID || adduser -h /home/wine -u $UID -s /bin/sh -D -G $GROUP_NAME $USER_NAME --disabled-password --shell /bin/bash
ENV HOME=/home/wine WINEPREFIX=/home/wine/.wine WINEARCH=win32 PATH="/home/wine/bin:$PATH" WINEDEBUG=-all

COPY make-aliases.sh /home/wine/make-aliases.sh
ADD mono.msi /home/wine/
ADD wix /home/wine/wix/

RUN chmod +x /home/wine/make-aliases.sh \
    && /home/wine/make-aliases.sh \
    && rm -f /home/wine/make-aliases.sh
RUN chown -R $USER_NAME:$GROUP_NAME /home/wine

# Use the separate Wine user
USER wine
WORKDIR /home/wine

RUN  ls -l /home/wine

# Install .NET framework and WiX Toolset binaries
RUN wine wineboot \
    && wine msiexec.exe -i mono.msi \
    && rm -f mono.msi \
    && mkdir $WINEPREFIX/drive_c/temp

WORKDIR /home/wine/wix
