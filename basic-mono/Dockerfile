# based on: https://github.com/qinyuanpei/HttpServer/blob/master/Dockerfile
FROM frolvlad/alpine-mono:5.20-glibc

ARG NUGET_VERSION=6.8.0
RUN mkdir /usr/nuget/ && \
    wget -O /usr/nuget/nuget.exe https://dist.nuget.org/win-x86-commandline/v${NUGET_VERSION}/nuget.exe 
ENV NUGET="/usr/nuget/nuget.exe"

ARG RUNNER_VERSION=3.12.0
ARG CONSOLERUNNER_VERSION=3.9.0

RUN mono ${NUGET} install NUnit.Runners -Version $RUNNER_VERSION -OutputDirectory ./TestRunner
ENV NUNIT="./TestRunner/NUnit.ConsoleRunner.$CONSOLERUNNER_VERSION/tools/nunit3-console.exe"

WORKDIR /WorkSpace
COPY Server /WorkSpace/Server/
COPY Utils /WorkSpace/Utils/
COPY basic-httpserver.sln /WorkSpace/

ARG MSBUILD_VERSION=17.3.0
RUN mono ${NUGET} install Microsoft.Build  -Version $MSBUILD_VERSION -OutputDirectory ./MsBuild

RUN xbuild /p:Configuration=Release ./basic-httpserver.sln
WORKDIR /tmp
ENTRYPOINT ["mono", "/WorkSpace/Server/bin/Release/HttpServer.exe" ]
