# https://hub.docker.com/_/microsoft-dotnet
FROM mcr.microsoft.com/dotnet/sdk:6.0-alpine AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY *.sln .
COPY AspNetCoreSerilogExample.Web/*.csproj ./AspNetCoreSerilogExample.Web/
COPY Elastic.Apm.SerilogEnricher/*.csproj ./Elastic.Apm.SerilogEnricher/
RUN dotnet restore -r linux-musl-x64

# copy everything else and build app
COPY AspNetCoreSerilogExample.Web/. ./AspNetCoreSerilogExample.Web/
COPY Elastic.Apm.SerilogEnricher/. ./Elastic.Apm.SerilogEnricher/
WORKDIR /source/Elastic.Apm.SerilogEnricher
RUN dotnet build -c release -o /app -r linux-musl-x64 --self-contained false --no-restore
WORKDIR /source/AspNetCoreSerilogExample.Web
RUN dotnet build -c release -o /app -r linux-musl-x64 --self-contained false --no-restore

