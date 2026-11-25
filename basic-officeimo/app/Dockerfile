FROM  mcr.microsoft.com/dotnet/sdk:8.0  AS build
WORKDIR /source
COPY ["example/AccessVBAInspect/AccessVBAInspect.csproj", "./AccessVBAInspect/AccessVBAInspect.csproj"]
RUN dotnet restore "./AccessVBAInspect/AccessVBAInspect.csproj"
COPY example ./
WORKDIR "/source/AccessVBAInspect"
RUN dotnet build -c Release -o /app

FROM build AS publish
RUN dotnet publish -c Release -o /app

