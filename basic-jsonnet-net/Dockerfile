# based on: https://github.com/qinyuanpei/HttpServer/blob/master/Dockerfile
FROM ubuntu:22.04

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y -qq git zip gnupg2 unzip apt-transport-https wget
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF

# NOTE: They commonly expect you to use:
# stable-focal instead of: stable-jammy
# even on Ubuntu 22.04.

RUN echo "deb https://download.mono-project.com/repo/ubuntu stable-focal main" | tee /etc/apt/sources.list.d/mono-official-stable.list
RUN apt-get update
RUN apt-get install -f
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC \
apt-get install -qqy mono-complete

RUN cert-sync /etc/ssl/certs/ca-certificates.crt

ARG NUGET_VERSION=6.8.0
RUN mkdir /usr/nuget/ && \
    wget -O /usr/nuget/nuget.exe https://dist.nuget.org/win-x86-commandline/v${NUGET_VERSION}/nuget.exe 
ENV NUGET="/usr/nuget/nuget.exe"

ARG RUNNER_VERSION=3.12.0
ARG CONSOLERUNNER_VERSION=3.9.0

RUN mono ${NUGET} install NUnit.Runners -Version $RUNNER_VERSION -OutputDirectory ./TestRunner
ENV NUNIT="/TestRunner/NUnit.ConsoleRunner.$RUNNER_VERSION/tools/nunit3-console.exe"

WORKDIR /WorkSpace
COPY JsonnetBinding /WorkSpace/JsonnetBinding/
COPY packages /WorkSpace/packages/
COPY JsonnetBinding.Tests /WorkSpace/JsonnetBinding.Tests/
COPY JsonnetBinding.sln /WorkSpace/
# NOTE: the workspace is NuGetScratchroot
RUN msbuild /p:Configuration=Debug ./JsonnetBinding.sln /t:Clean,Build
WORKDIR /tmp
# NOTE: the exec form is specifically designed to bypass the shell entirely
CMD mono $NUNIT /WorkSpace/JsonnetBinding.Tests/bin/Debug/Tests.dll --labels=All --trace=Debug
