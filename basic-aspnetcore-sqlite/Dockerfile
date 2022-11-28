
# use the official Microsoft .NET 5 build image
FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build-env

# move to the src target dir
WORKDIR /app/src

# copy the solution and project files
ADD ./src/EfcoreTest.sln ./EfcoreTest.sln
ADD ./src/EfcoreTest.Api/EfcoreTest.Api.csproj ./EfcoreTest.Api/EfcoreTest.Api.csproj
ADD ./src/EfcoreTest.Data/EfcoreTest.Data.csproj ./EfcoreTest.Data/EfcoreTest.Data.csproj

# restore the NuGet packages (for caching)
RUN dotnet restore --runtime linux-x64

# copy the source code
ADD ./src/ ./

# TODO: run the unit tests here ...
RUN dotnet test --runtime linux-x64 --configuration Release --no-restore

# make a release build
RUN dotnet publish --runtime linux-x64 --configuration Release \
                   --output /app/bin/ --no-restore

# define the .NET 6 runtime image
# final stage/image
FROM mcr.microsoft.com/dotnet/aspnet:6.0
WORKDIR /app/bin
COPY --from=build-env /app/bin /app/bin

# launch the daemon test service on container startup
ENTRYPOINT ["dotnet", "EfcoreTest.Api.dll"]
