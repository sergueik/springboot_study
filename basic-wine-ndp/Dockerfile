
# based on: https://github.com/syedhassaanahmed/node-wine/blob/main/Dockerfile
# FROM node:lts-bullseye-slim
FROM debian:bullseye-slim
ENV WINEDEBUG=-all WINEARCH=win32

# Install wget, cabextract, Wine, Winetricks and .NET 4.5
RUN dpkg --add-architecture i386 && \
    mkdir -pm755 /etc/apt/keyrings && \
    apt-get update && apt-get install -y wget cabextract 
# NOTE: VistualBox error
RUN  wget -O /etc/apt/keyrings/winehq-archive.key https://dl.winehq.org/wine-builds/winehq.key && \
    wget -NP /etc/apt/sources.list.d/ https://dl.winehq.org/wine-builds/debian/dists/bullseye/winehq-bullseye.sources
    
RUN apt-get update && apt-get install -y --no-install-recommends winehq-stable

RUN  wget https://raw.githubusercontent.com/Winetricks/winetricks/master/src/winetricks && \
    chmod +x winetricks && \
    mv -v winetricks /usr/local/bin

RUN  winetricks --unattended dotnet45

