FROM mcr.microsoft.com/dotnet/aspnet:6.0 AS base

WORKDIR /app
EXPOSE 80
EXPOSE 443
 
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
# NOTE: .Net fragile directory layout
COPY json.grafana.datasources/json.grafana.datasources.csproj json.grafana.datasources/
# 
# RUN dotnet clean json.grafana.datasources/json.grafana.datasources.csproj
RUN dotnet restore json.grafana.datasources/json.grafana.datasources.csproj
COPY json.grafana.datasources json.grafana.datasources/
WORKDIR /src/json.grafana.datasources
RUN dotnet build json.grafana.datasources.csproj -c Release -o /app/build
 
FROM build AS publish
RUN dotnet publish json.grafana.datasources.csproj -c Release -o /app/publish
 
FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Json.Grafana.DataSources.dll"]
