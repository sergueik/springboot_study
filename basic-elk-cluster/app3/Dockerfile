FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build

WORKDIR /app/src

ADD ./src/basic-aspnetcore-sqlite.sln ./
ADD ./src/Api/Api.csproj ./Api/
ADD ./src/Data/Data.csproj ./Data/

RUN dotnet restore --runtime linux-x64

ADD ./src/ ./

# make a release build
RUN dotnet publish --no-self-contained --runtime linux-x64 \
    --configuration Release \
    --output /app/bin/ --no-restore

FROM mcr.microsoft.com/dotnet/aspnet:6.0
WORKDIR /app/bin
COPY --from=build /app/bin /app/bin

ENTRYPOINT ["dotnet", "Api.dll"]
