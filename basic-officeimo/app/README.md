### Info

demo of the scenario [Inspect application objects and VBA](https://github.com/EvotecIT/OfficeIMO/tree/master/OfficeIMO.Access#inspect-application-objects-and-vba)
supported by [EvitectIT/OfficeIMO](https://github.com/EvotecIT/OfficeIMO)

### Background

The [OfficeIMO](https://officeimo.com/) – Free cross-platform Microsoft Word .NET library

Assemblies for COM-free .NET app for creating, reading, editing, converting, rendering, and extracting Office, Visio, CHM, PDF, and other formants also including email, OneNote

### Usage

```sh
docker pull mcr.microsoft.com/dotnet/sdk:8.0
```
```sh
docker build -f Dockerfile -t test .
```
```text
tep 1/9 : FROM  mcr.microsoft.com/dotnet/sdk:8.0  AS build
 ---> 45d16602f24a
Step 2/9 : WORKDIR /source
 ---> Running in d0350ff96cf1
 ---> Removed intermediate container d0350ff96cf1
 ---> c936e3c5c02c
Step 3/9 : COPY ["example/AccessVBAInspect/AccessVBAInspect.csproj", "./AccessVBAInspect/AccessVBAInspect.csproj"]
 ---> ba731c891a34
Step 4/9 : RUN dotnet restore "./AccessVBAInspect/AccessVBAInspect.csproj"
 ---> Running in 54c14f9302c8
  Determining projects to restore...
  Restored /source/AccessVBAInspect/AccessVBAInspect.csproj (in 43.56 sec).
 ---> Removed intermediate container 54c14f9302c8
 ---> 76580d344a40
Step 5/9 : COPY example ./
 ---> 5107bb8c910d
Step 6/9 : WORKDIR "/source/AccessVBAInspect"
 ---> Running in 00ee7a178ab0
 ---> Removed intermediate container 00ee7a178ab0
 ---> 0c88ee238cab
Step 7/9 : RUN dotnet build -c Release -o /app
 ---> Running in f207d3ee3559
  Determining projects to restore...
  All projects are up-to-date for restore.
  AccessVBAInspect -> /app/AccessVBAInspect.dll

Build succeeded.
    0 Warning(s)
    0 Error(s)

Time Elapsed 00:00:05.93
 ---> Removed intermediate container f207d3ee3559
 ---> a6b9a36e2517
Step 8/9 : FROM build AS publish
 ---> a6b9a36e2517
Step 9/9 : RUN dotnet publish -c Release -o /app
 ---> Running in bcf56c0598b7
  Determining projects to restore...
  All projects are up-to-date for restore.
  AccessVBAInspect -> /source/AccessVBAInspect/bin/Release/net8.0/AccessVBAInspect.dll
  AccessVBAInspect -> /app/
 ---> Removed intermediate container bcf56c0598b7
 ---> 640d7a51bfab
Successfully built 640d7a51bfab
Successfully tagged test:latest

```
```sh
git clone --depth 1 https://github.com/EvotecIT/OfficeIMO ../OfficeIMO
find ../OfficeIMO/ -iname '*.accdb'
cp ../OfficeIMO/OfficeIMO.Access.Tests/Fixtures/Application/objects-ace12.accdb application.accdb
```
```sh
docker run -it -w  /source/AccessVBAInspect -v $(pwd):/source/AccessVBAInspect test /app/AccessVBAInspect
```
```text
FixtureForm: 
Forms/0/Blob: 915 bytes
Forms/0/TypeInfo: 48 bytes
Forms/0/BlobDelta: 0 bytes
Forms/0/PropData: 22 bytes
Attribute VB_Name = "FixtureModule"
Option Compare Database
Option Explicit
Public Function FixtureValue() As Long
    FixtureValue = 42
End Function

stdole: *\G{00020430-0000-0000-C000-000000000046}#2.0#0#C:\Windows\System32\stdole2.tlb#OLE Automation
DAO: *\G{4AC9E1DA-5BAD-4AC7-86E3-24F4CDCECA28}#c.0#0#C:\Program Files\Common Files\Microsoft Shared\OFFICE16\ACEDAO.DLL#Microsoft Office 16.0 Access database engine Object Library
```
### Troubleshooting


NOTE: specific to __Docker Toolbox__ - may need to improve the VM setting

```text
Step 4/9 : RUN dotnet restore "./PdfDocGenerator.CLI/PdfDocGenerator.CLI.csproj"
 ---> Running in 1fa2c527bcf8
Failed to create CoreCLR, HRESULT: 0x80070008
The command '/bin/sh -c dotnet restore "./PdfDocGenerator.CLI/PdfDocGenerator.CLI.csproj"' returned a non-zero code: 137
```
```text
Inspecting error code: 2147942408
Not enough memory resources are available to process this command. (Exception from HRESULT: 0x80070008)
```
```sh
docker-machine ssh
   ( '>')
  /) TC (\   Core is distributed with ABSOLUTELY NO WARRANTY.
 (/-_--_-\)           www.tinycorelinux.net

docker@default:~$ free
```
```text
              total        used        free      shared  buff/cache   available
Mem:        4039664       74056     3654428      281428      311180     3637052
Swap:       1949948           0     1949948
```
---

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

