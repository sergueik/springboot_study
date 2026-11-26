FROM mcr.microsoft.com/dotnet/sdk:8.0-alpine AS build
WORKDIR /source
ARG PROJECT=AccessVBAInspect
COPY example/${PROJECT}/${PROJECT}.csproj ./${PROJECT}/${PROJECT}.csproj
RUN dotnet restore ./${PROJECT}/${PROJECT}.csproj --runtime linux-musl-x64
COPY example ./
WORKDIR /source/${PROJECT}
RUN dotnet build -c Release -o /app

# make a release build
RUN dotnet publish --no-self-contained --runtime linux-musl-x64 \
  --configuration Release --output /app/linux-musl-x64 \
  --no-restore
FROM mcr.microsoft.com/dotnet/runtime:8.0-alpine AS publish
WORKDIR /app
COPY --from=build /app .

