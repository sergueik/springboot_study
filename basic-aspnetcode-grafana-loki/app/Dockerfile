FROM mcr.microsoft.com/dotnet/aspnet:6.0 AS base

WORKDIR /app
EXPOSE 80
EXPOSE 443
 
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
# NOTE: .Net fragile directory layout
COPY WeatherForecast.Web.Api/WeatherForecast.Web.Api.csproj WeatherForecast.Web.Api/
RUN dotnet restore WeatherForecast.Web.Api/WeatherForecast.Web.Api.csproj
COPY WeatherForecast.Web.Api WeatherForecast.Web.Api/
WORKDIR /src/WeatherForecast.Web.Api
RUN dotnet build WeatherForecast.Web.Api.csproj -c Release -o /app/build
 
FROM build AS publish
RUN dotnet publish WeatherForecast.Web.Api.csproj -c Release -o /app/publish
 
FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "WeatherForecast.Web.Api.dll"]
