FROM debian:bullseye
# FROM bitnami/minideb:bullseye
ENV  DEBIAN_FRONTEND=noninteractive 
ENV CODENAME=bullseye

RUN echo deb http://deb.debian.org/debian/ $CODENAME main contrib non-free | tee /etc/apt/sources.list  
RUN echo deb http://deb.debian.org/debian/ $CODENAME-updates main contrib non-free | tee -a /etc/apt/sources.list  
# RUN echo deb http://archive.debian.org/debian-security/ $CODENAME/updates main contrib non-free | tee -a /etc/apt/sources.list  
# E: The repository 'http://archive.debian.org/debian-security bullseye/updates Release' does not have a Release file.
RUN apt-get update  \
   && apt-get install  -q -y  apt-utils curl
RUN apt-get install  -q -y libfftw3-double3
RUN apt-get install  -q -y libsndfile1 --fix-missing
ARG VERSION=2.1.2
ENV VERSION=$VERSION
RUN curl -skLo /tmp/ReSampler-Linux.deb https://github.com/jniemann66/ReSampler/releases/download/v${VERSION}/ReSampler-${VERSION}-Linux.deb
ADD ReSampler-Linux.deb /tmp
RUN dpkg -i /tmp/ReSampler-Linux.deb
