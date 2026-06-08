
### Info

creating a TIBCO MFT Docker image involves these steps:

* Preparation: Install a base Linux operating system and the required Java JDK
  * Base Installation: Perform a standard installation of the TIBCO MFT component to create a "source" directory
  * Hotfix Application: Install any necessary TIBCO hotfixes to the base installation before containerizing
  * Configure Scripts: Edit the startall.sh or similar startup scripts to ensure they work within a container environment
  * Build Image: Use the provided `Dockerfile` (often found in the `$MFT_Install/cloud` directory) to build the image:
  + Example: sudo docker build -f Dockerfile.mftcc -t library/mftcc_8.5.0:v1 
  * .Persistence: Configure persistent storage (volumes)
  * Repository: Save the final image to a private registry; public registries are generally prohibited by __TIBCO__ licensing

### Background

__TIBCO__ __Managed File Transfer__ (__MFT__)  often abbreviated as __TIBCO__ __MFT__ __Internet Server__ - contains __TIBCO__ __MAILBOX__  that acts as a secure, temporary holding area or repository. It allows you to upload files to a server and securely notify external end-users with a link, allowing them to log in and download the file securely

### API Access

From the TIBCO documentation, MFT Internet Server exposes programmatic APIs, and those APIs are primarily HTTP/HTTPS-based REST services, not SFTP commands.

The architecture is roughly:
```text
MFT Client Application
      |
      | HTTPS REST API
      v
+-------------------+
| TIBCO MFT         |
| Internet Server   |
+-------------------+
      |
      +--> Mailbox storage
      +--> File Share
      +--> SFTP servers
      +--> FTPS servers
      +--> Partner systems
```
The REST API documentation shows operations such as:

  * List directories
  * Create directories
  * Rename files
  * Delete files
  * Upload files
  * Download files
  * Manage transfer sessions
  * Query available transfers

using URLs such as:

  * `https://server:8443/cfcc/rest/ft/v5/navigation/...`
  * `https://server:8443/cfcc/rest/ft/v5/transfer/...`

with standard `HTTP` methods (`GET`, `POST`, `PUT`, `DELETE`).

For example, a program could:

  * Authenticate to __MFT__ over `HTTPS`.
  * Query a mailbox directory.
  * Download a file via `REST`.
  * Upload a new file via `REST`.
  * Delete or move processed files.

all without ever speaking `SFTP` directly.

What's interesting is that __TIBCO__ also documents a "create custom file transfer interface" capability and historically exposed `JSON` and `SOAP` APIs in addition to the web UI. The API guide contains sections on JSON APIs, REST APIs, HTTP transfers, and custom transfer interfaces.

So if your question is:

  * *Can I write a Java/Python/Perl/PowerShell program that treats a TIBCO mailbox like a filesystem?*

the answer is generally yes, via the __MFT REST API__ over `HTTPS`.

  * *Does the API itself operate over SFTP?*

Generally no. The API is `HTTP/HTTPS`. TIBCO may use `SFTP` internally or as a connector to external systems,
but the application-facing API is `REST` over `HTTPS`.

Given your background with PowerShell and integration work, the most likely approach would be:

```powershell
<# authenticate # >
Invoke-RestMethod ...
```

```powershell
<# list mailbox #>
Invoke-RestMethod `
   -Method GET `
   -Uri "https://mftserver/cfcc/rest/ft/v5/navigation/inbox"
```
```powershell
<# download file #>
Invoke-WebRequest `
   -Uri "https://mftserver/cfcc/rest/ft/v5/transfer/inbox/report.csv" `
   -OutFile report.csv
```
The exact URLs and authentication scheme depend on how that particular MFT instance is configured.

If there is access to the __MFT server__ of specific version (__8.x__, __7.x__, etc.), one can find whether the mailbox itself is exposed through the `REST` __API__ or whether mailbox access requires one of the older `JSON`/`SOAP` interfaces



### See Also 
  * https://docs.tibco.com/pub/mftis/8.5.1/doc/pdf/TIB_mftis_8.5.1_container-deployment.pdf
