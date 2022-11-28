###  Info

replica of a boilerplate __ASP.NET Web API + Entity Framework__
[test repository](https://github.com/Bonifatius94/AspnetEfcoreTest)
with SSL disabled, and added added `Elastic.Apm.NetCoreAll` to dependenies, and to `Startup.cs` for APM testing


![ASP.Net Core Events](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-aspnetcore-events.png)

### Usage


#### Before Build

NOTE: the certificate created following the original project steps does not appear to work, at the end. Can be skipped, and ASP.Net Core appliction tested using REST calls over http.

The error reported by the application:
```sh
docker-compose up
```

```text
Creating network "basic-aspnetcore-sqlite_default" with the default driver
Creating basic-aspnetcore-sqlite_basic-aspnetcore-sqlite_1 ... done
Attaching to basic-aspnetcore-sqlite_basic-aspnetcore-sqlite_1
basic-aspnetcore-sqlite_1  | Unhandled exception. System.Security.Cryptography.CryptographicException: The certificate data cannot be read with the provided password, the password may be incorrect.
basic-aspnetcore-sqlite_1  |  ---> System.Security.Cryptography.CryptographicException: The certificate data cannot be read with the provided password, the password may be incorrect.
basic-aspnetcore-sqlite_1  |    at Internal.Cryptography.Pal.UnixPkcs12Reader.VerifyAndDecrypt(ReadOnlySpan`1 password, ReadOnlyMemory`1 authSafeContents)c
basic-aspnetcore-sqlite_1  |    at Internal.Cryptography.Pal.UnixPkcs12Reader.Decrypt(SafePasswordHandle password, Boolean ephemeralSpecified)
basic-aspnetcore-sqlite_1  |    --- End of inner exception stack trace ---
basic-aspnetcore-sqlite_1  |    at Internal.Cryptography.Pal.UnixPkcs12Reader.Decrypt(SafePasswordHandle password, Boolean ephemeralSpecified)
basic-aspnetcore-sqlite_1  |    at Internal.Cryptography.Pal.PkcsFormatReader.TryReadPkcs12(OpenSslPkcs12Reader pfx, SafePasswordHandle password, Boolean single, Boolean ephemeralSpecified, ICertificatePal& readPal, List`1& readCerts)
basic-aspnetcore-sqlite_1  |    at Internal.Cryptography.Pal.PkcsFormatReader.TryReadPkcs12(ReadOnlySpan`1 rawData, SafePasswordHandle password, Boolean single, Boolean ephemeralSpecified, ICertificatePal& readPal, List`1& readCerts, Exception& openSslException)
basic-aspnetcore-sqlite_1  |    at Internal.Cryptography.Pal.OpenSslX509CertificateReader.FromFile(String fileName, SafePasswordHandle password, X509KeyStorageFlags keyStorageFlags)
basic-aspnetcore-sqlite_1  |    at System.Security.Cryptography.X509Certificates.X509Certificate..ctor(String fileName, String password, X509KeyStorageFlags keyStorageFlags)
basic-aspnetcore-sqlite_1  |    at System.Security.Cryptography.X509Certificates.X509Certificate2..ctor(String fileName, String password)
basic-aspnetcore-sqlite_1  |    at Microsoft.AspNetCore.Server.Kestrel.Core.Internal.Certificates.CertificateConfigLoader.LoadCertificate(CertificateConfig certInfo, String endpointName)
basic-aspnetcore-sqlite_1  |    at Microsoft.AspNetCore.Server.Kestrel.KestrelConfigurationLoader.LoadDefaultCert()
basic-aspnetcore-sqlite_1  |    at Microsoft.AspNetCore.Server.Kestrel.KestrelConfigurationLoader.Reload()
basic-aspnetcore-sqlite_1  |    at Microsoft.AspNetCore.Server.Kestrel.KestrelConfigurationLoader.Load()
basic-aspnetcore-sqlite_1  |    at Microsoft.AspNetCore.Server.Kestrel.Core.KestrelServerImpl.BindAsync(CancellationToken cancellationToken)
basic-aspnetcore-sqlite_1  |    at Microsoft.AspNetCore.Server.Kestrel.Core.KestrelServerImpl.StartAsync[TContext](IHttpApplication`1 application, CancellationToken cancellationToken)
basic-aspnetcore-sqlite_1  |    at Microsoft.AspNetCore.Hosting.GenericWebHostService.StartAsync(CancellationToken cancellationToken)
basic-aspnetcore-sqlite_1  |    at Microsoft.Extensions.Hosting.Internal.Host.StartAsync(CancellationToken cancellationToken)
basic-aspnetcore-sqlite_1  |    at Microsoft.Extensions.Hosting.HostingAbstractionsHostExtensions.RunAsync(IHost host, CancellationToken token)
basic-aspnetcore-sqlite_1  |    at Microsoft.Extensions.Hosting.HostingAbstractionsHostExtensions.RunAsync(IHost host, CancellationToken token)
basic-aspnetcore-sqlite_1  |    at Microsoft.Extensions.Hosting.HostingAbstractionsHostExtensions.Run(IHost host)
basic-aspnetcore-sqlite_1  |    at EfcoreTest.Api.Program.Main(String[] args) in /app/src/EfcoreTest.Api/Program.cs:line 16
basic-aspnetcore-sqlite_basic-aspnetcore-sqlite_1 exited with code 139
```
original pre-build configuration steps:

* define the password for the SSL certificate to be generated

```sh
export HTTPS_SSL_PW=cd6dafc5bfbc3e9d114fadcef9042679
```

* create self-signed certs, leave fields and password empty

```sh
cd certs
openssl req -x509 -nodes -days 365 -newkey rsa:2048  -keyout localhost.key -out localhost.crt -config localhost.conf  -passout pass:$HTTPS_SSL_PW
openssl pkcs12 -export -out localhost.pfx -inkey localhost.key -in localhost.crt
```
* verify the keygen worked
```sh
openssl verify -CAfile localhost.crt localhost.crt
```
* verify the certificate registeration was successful (expected to fail)
```sh
openssl verify localhost.crt
```
* register the self-signed certificate on the local machine, so the browser will trust it
```sh
sudo cp localhost.crt /usr/local/share/ca-certificates/
sudo update-ca-certificates
```
* verify the certificate registeration was successful (expected to pass now)
```sh
cat /etc/ssl/certs/localhost.pem
openssl verify localhost.crt
```
#### Build

* pull the Docker images for runtime and SDK
```sh
docker pull mcr.microsoft.com/dotnet/sdk:6.0
docker pull mcr.microsoft.com/dotnet/aspnet:6.0
```

* build the application (NOTE: time comsuming)
```sh
IMAGE=basic-aspnetcore-sqlite
docker build -t $IMAGE -f Dockerfile .
```
* run the app in foreground
```
NAME=basic-aspnetcore-sqlite
ELK_NETWORK=basic-elk-cluster_elastic
docker run --name $NAME --network $ELK_NETWORK -it -p 5000:80 $IMAGE
docker run -e ASPNETCORE_URLS="http://+" -e ASPNETCORE_HTTP_PORT=80 -p 5000:80 --network $ELK_NETWORK -it $IMAGE
```

alternatively,
```sh
docker-compose up -d
```
* TODO: update the `docker-compose.yml` to connect to the network created earlier in `basic-elk-cluster`

####  Verify

NOTE: operate through HTTP -  currently docker-compose fails with loading certificate

* define the base URL

```sh
URL=http://localhost:5000/todo
```
* query empty todo items list
```sh
curl -X GET $URL
```
* create a todo item
```sh
curl -d '{"Id":0,"Text":"Buy 2 bottles of milk","Due":"2021-08-23"}' -H 'Content-Type: application/json' -X POST $URL
```

* create one other todo item
```
curl -d '{"Id":0,"Text":"Buy a brezel and butter","Due":"2021-08-24"}' -H 'Content-Type: application/json' -X POST $URL
```
* query todo items list just created
```
curl -X GET $URL
```

* update the second todo item
```sh
curl -d '{"Id":2,"Text":"Buy a brezel and buttermilk","Due":"2021-08-26"}' -H 'Content-Type: application/json' -X PUT "$URL/2"
```
* query the updated todo item
```sh
curl -X GET "$URL/2"
```
* delete all todo items from database
```sh
curl -X DELETE "$URL/1"
curl -X DELETE "$URL/2"
```

* query todo items list (make sure deleted items are gone)
```sh
curl -X GET $URL
```

* REST call are captured

![HTTP Event Transaction](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-rest-call.png)

* Entity Framework calls initiated by REST call are captured

![SQL Event Transaction](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-apm-entityframework-sqlite.png)


### See Also

  * https://stackoverflow.com/questions/55485511/how-to-run-dotnet-dev-certs-https-trust
  * __APM .NET Agent__

    + [documentation](https://github.com/elastic/apm-agent-dotnet/blob/main/docs/index.asciidoc)
    + [releases](https://github.com/elastic/apm-agent-dotnet/releases)
    + [packaged](https://www.nuget.org/packages/Elastic.Apm.NetCoreAll/)
	
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
