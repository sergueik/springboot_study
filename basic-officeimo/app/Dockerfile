FROM  mcr.microsoft.com/dotnet/sdk:8.0  AS build
WORKDIR /source
COPY ["OfficeIMO.Word.FromHtml/PdfDocGenerator.CLI/PdfDocGenerator.CLI.csproj", "./PdfDocGenerator.CLI/PdfDocGenerator.CLI.csproj"]
RUN dotnet restore "./PdfDocGenerator.CLI/PdfDocGenerator.CLI.csproj"
COPY OfficeIMO.Word.FromHtml ./
WORKDIR "/source/PdfDocGenerator.CLI"
RUN dotnet build -c Release -o /app

FROM build AS publish
RUN dotnet publish -c Release -o /app

