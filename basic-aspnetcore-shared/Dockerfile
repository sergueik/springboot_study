FROM mcr.microsoft.com/dotnet/sdk:6.0-alpine AS build-shared
WORKDIR /source
ARG RID=linux-musl-x64 
# Build Shared lib
# copy csproj and restore as distinct layers
COPY ["./shared/shared.csproj", "./shared/"]
RUN dotnet restore "./shared/shared.csproj" -r $RID
# copy everything else and build app
ADD ["/shared/", "./shared/"]
WORKDIR /source/shared
RUN dotnet publish -c release -o /app/shared -r $RID --self-contained false --no-restore

# Build app proj
FROM mcr.microsoft.com/dotnet/sdk:6.0-alpine AS build-app
# NOTE: need to repeat ARG after FROM
ARG RID=linux-musl-x64 
WORKDIR /source
# copy csproj and restore as distinct layers
COPY ["app/app.csproj", "./app/"]
RUN dotnet restore "./app/app.csproj" -r $RID
ADD app ./app/
COPY --from=build-shared /app/shared ./shared
WORKDIR /source/app

# RUN dotnet build "./app/app.csproj" -c Release -o /app/service
RUN dotnet publish "./app.csproj" -c release -o /app/service -r $RID --self-contained false --no-restore

# Create final image
FROM mcr.microsoft.com/dotnet/aspnet:6.0-alpine-amd64
WORKDIR /app
COPY --from=build-app /app/service .
ENTRYPOINT ["dotnet", "/app/app.dll"]
# ENTRYPOINT ["./app"]
