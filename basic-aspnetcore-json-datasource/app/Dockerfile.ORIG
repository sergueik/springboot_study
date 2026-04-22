FROM microsoft/dotnet:2.2-aspnetcore-runtime AS base
WORKDIR /app
EXPOSE 80
EXPOSE 443
VOLUME ["/home/data"]

FROM microsoft/dotnet:2.2-sdk AS build
WORKDIR /src
COPY ["json.grafana.datasources/json.grafana.datasources.csproj", "json.grafana.datasources/"]
RUN dotnet restore "json.grafana.datasources/json.grafana.datasources.csproj"
COPY . .
WORKDIR "/src/json.grafana.datasources"
RUN dotnet build "json.grafana.datasources.csproj" -c Release -o /app

FROM build AS publish
RUN dotnet publish "json.grafana.datasources.csproj" -c Release -o /app

FROM base AS final
WORKDIR /app
COPY --from=publish /app .
ENTRYPOINT ["dotnet", "Json.Grafana.DataSources.dll"]