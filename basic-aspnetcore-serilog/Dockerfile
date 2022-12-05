# https://hub.docker.com/_/microsoft-dotnet
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS basic-aspnetapp-serilog-build
WORKDIR /source
ARG RID=linux-x64 

# copy csproj and restore as distinct layers
COPY *.sln .
COPY AspNetCoreSerilogExample.Web/*.csproj ./AspNetCoreSerilogExample.Web/
COPY Elastic.Apm.SerilogEnricher/*.csproj ./Elastic.Apm.SerilogEnricher/
RUN dotnet restore -r $RID

# copy everything else and build app
COPY AspNetCoreSerilogExample.Web/. ./AspNetCoreSerilogExample.Web/
COPY Elastic.Apm.SerilogEnricher/. ./Elastic.Apm.SerilogEnricher/
WORKDIR /source/Elastic.Apm.SerilogEnricher
RUN dotnet build -c release -o /app -r $RID --self-contained false --no-restore
WORKDIR /source/AspNetCoreSerilogExample.Web
RUN dotnet build -c release -o /app -r $RID --self-contained false --no-restore

FROM mcr.microsoft.com/dotnet/aspnet:6.0
WORKDIR /app

COPY --from=basic-aspnetapp-serilog-build /app/ ./
# NOTE the ENTRYPOINT ["./AspNetCoreSerilogExample.Web"]
# occasionally does not work
ENTRYPOINT ["dotnet", "/app/AspNetCoreSerilogExample.Web.dll"]
