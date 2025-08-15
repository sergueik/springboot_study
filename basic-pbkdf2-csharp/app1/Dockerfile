FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY pbkdf2-csharp.sln ./
ADD Server Server
# ADD Program Program
ADD Utils Utils
# NOTE: omitted Test and Benchmarks folders
# ADD Test Test

# Restore for alpine musl
RUN dotnet restore pbkdf2-csharp.sln --runtime linux-musl-x64

# Build + (optional) test
RUN dotnet build pbkdf2-csharp.sln -c Release --no-restore
# RUN dotnet test Test -c Release --no-build

# Publish self-contained webapp\  
# trimming is risky for web due to reflection
RUN dotnet publish Server/Server.csproj \
    -c Release -r linux-musl-x64 --self-contained true \
    /p:PublishSingleFile=true \
    /p:PublishTrimmed=false \
    -o /app


FROM mcr.microsoft.com/dotnet/runtime-deps:6.0-alpine3.16
WORKDIR /app
ENV ASPNETCORE_URLS=http://0.0.0.0:8080 \
    DOTNET_EnableDiagnostics=0 \
    DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1
EXPOSE 8080

COPY --from=build /app .
ENTRYPOINT ["./Server"]

