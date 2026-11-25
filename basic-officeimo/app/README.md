### Info

https://github.com/EvotecIT/OfficeIMO/blob/master/OfficeIMO.Access.Tests/AccessVbaRecreationTests.cs
https://github.com/EvotecIT/OfficeIMO/blob/master/OfficeIMO.Access/README.md
### Usage

### Info

https://github.com/EvotecIT/OfficeIMO/blob/master/OfficeIMO.Access.Tests/AccessVbaRecreationTests.cs
https://github.com/EvotecIT/OfficeIMO/blob/master/OfficeIMO.Access/README.md
### Usage
```
docker pull mcr.microsoft.com/dotnet/sdk:8.0
```
```
docker build -f Dockerfile -t test .
```
```
docker run -it test sh
```

```sh
./bin/Release/net8.0/PdfDocGenerator.CLI
```

```
# ls -l Sal*
-rw-r--r-- 1 root root 11529 Nov 25 21:53 Sales.docx

```
```sh
docker run -it -w  /source/PdfDocGenerator.CLI -v $(pwd):/source/PdfDocGenerator.CLI test /app/PdfDocGenerator.CLI
```
### Troubleshooting


Docker Toolbox

```
Step 4/9 : RUN dotnet restore "./PdfDocGenerator.CLI/PdfDocGenerator.CLI.csproj"
 ---> Running in 1fa2c527bcf8
Failed to create CoreCLR, HRESULT: 0x80070008
The command '/bin/sh -c dotnet restore "./PdfDocGenerator.CLI/PdfDocGenerator.CLI.csproj"' returned a non-zero code: 137

```
```
Inspecting error code: 2147942408
Not enough memory resources are available to process this command. (Exception from HRESULT: 0x80070008)
```


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


```
docker pull mcr.microsoft.com/dotnet/sdk:8.0

```
```
docker build -f Dockerfile -t test .

```
```
docker run -it test sh
```

```sh
./bin/Release/net8.0/PdfDocGenerator.CLI
```

```
# ls -l Sal*
-rw-r--r-- 1 root root 11529 Nov 25 21:53 Sales.docx

```
### Troubleshooting


Docker Toolbox

```
Step 4/9 : RUN dotnet restore "./PdfDocGenerator.CLI/PdfDocGenerator.CLI.csproj"
 ---> Running in 1fa2c527bcf8
Failed to create CoreCLR, HRESULT: 0x80070008
The command '/bin/sh -c dotnet restore "./PdfDocGenerator.CLI/PdfDocGenerator.CLI.csproj"' returned a non-zero code: 137

```
```
Inspecting error code: 2147942408
Not enough memory resources are available to process this command. (Exception from HRESULT: 0x80070008)
```


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
