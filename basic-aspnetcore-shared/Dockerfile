FROM mcr.microsoft.com/dotnet/sdk:6.0-alpine AS build-app
# Build app proj
ARG RID=linux-musl-x64 
WORKDIR /source
# copy csproj and restore as distinct layers
COPY ["app/app.csproj", "./app/"]
RUN dotnet restore "./app/app.csproj" -r $RID
# copy everything else and build app
ADD app ./app/
WORKDIR /source/app

RUN dotnet publish "./app.csproj" -c release -o /app/service -r $RID --self-contained false --no-restore

# Create final image
FROM mcr.microsoft.com/dotnet/aspnet:6.0-alpine-amd64
WORKDIR /app
COPY --from=build-app /app/service .
ENTRYPOINT ["dotnet", "/app/app.dll"]
