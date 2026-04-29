
# based on: https://github.com/syedhassaanahmed/node-wine/blob/main/Dockerfile
FROM debian:bullseye-slim
ENV WINEDEBUG=-all WINEARCH=win32

# Install wget, cabextract, Wine, Winetricks and .NET 4.5
RUN dpkg --add-architecture i386 && \
    mkdir -pm755 /etc/apt/keyrings && \
    apt-get update && \
    apt-get install -qqy net-tools netcat-openbsd wget cabextract && \
    wget -O /etc/apt/keyrings/winehq-archive.key https://dl.winehq.org/wine-builds/winehq.key && \
    wget -NP /etc/apt/sources.list.d/ https://dl.winehq.org/wine-builds/debian/dists/bullseye/winehq-bullseye.sources && \
    apt-get update && apt-get install -qqy --no-install-recommends winehq-stable && \
    rm -rf /var/lib/apt/lists/* && \
    wget https://raw.githubusercontent.com/Winetricks/winetricks/master/src/winetricks && \
    chmod +x winetricks && \
    mv -v winetricks /usr/local/bin

# Install Winetricks .NET 4.5
RUN   winetricks --unattended dotnet45

