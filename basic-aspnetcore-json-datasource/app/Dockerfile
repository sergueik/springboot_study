FROM mcr.microsoft.com/dotnet/aspnet:6.0 AS base

WORKDIR /app
EXPOSE 80
EXPOSE 443
 
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
# NOTE: .Net fragile directory layout
COPY Program/Program.csproj Program/
# 
# RUN dotnet clean Program/Program.csproj
RUN dotnet restore Program/Program.csproj
COPY Program Program/
WORKDIR /src/Program
RUN dotnet build Program.csproj -c Release -o /app/build
 
FROM build AS publish
RUN dotnet publish Program.csproj -c Release -o /app/publish
 
FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Json.Grafana.DataSources.dll"]
