
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


TIBCO explicitly documents container deployment for MFT Internet Server and provides sample Dockerfiles and Kubernetes deployment artifacts as part of the product distribution. TIBCO states that MFT Internet Server "supports Docker and Kubernetes" and includes a Dockerfile under the installation's cloud/container deployment samples


However, there is an important distinction:

TIBCO is not distributing a self-contained Docker image on Docker Hub that you simply docker pull.
The documented process is typically:
Install MFT.
Use the supplied Dockerfile and scripts.
Build your own image containing your licensed MFT installation.
Connect it to an external database.
Run it under Docker or Kubernetes.

From the deployment guide, a running container still requires:

Database configuration.
Environment variables.
Port mappings for HTTPS, SFTP, FTP, etc.
Persistent storage for logs and configuration.

For your use case—evaluating the Mailbox API and experimenting with uploads/downloads—I would recommend Docker if:

You already use Docker Compose.
You want a disposable test environment.
You do not want to maintain a dedicated VM.
You are comfortable with externalizing the database and persistent volumes.

I would be somewhat cautious if your goal is simply:

"I want to quickly play with the Mailbox REST API."

Because MFT is an enterprise product with a non-trivial setup. Even in containers you still need licensing, database connectivity, and product configuration before the mailbox functionality becomes useful. The container deployment guide assumes familiarity with Docker/Kubernetes and explicitly notes that orchestration support is largely the customer's responsibility.

For a proof of concept, my preference would be:

Docker
 ├── mftis
 └── postgres/oracle/sqlserver

using bind-mounted volumes so that:

configuration survives container restarts, mailbox data survives image rebuilds, you can easily inspect logs from the host.

One thing I would verify before starting is which MFT component you have access to:

* MFT Internet Server (MFTIS) — exposes the web UI, mailboxes, REST APIs, HTTPS endpoints.

* MFT Command Center (MFTCC) — administration and orchestration.
Platform Server — managed transfer engine.

If your interest is specifically "browser mailbox + REST API to list/upload/download files", then MFT Internet Server is the component you want to containerize first.

If you have the installation media available, I can help determine whether the package you received already contains the Docker deployment samples and what the minimum Docker Compose stack would look like.

### See Also 
  * https://docs.tibco.com/pub/mftis/8.5.1/doc/pdf/TIB_mftis_8.5.1_container-deployment.pdf
  * https://docs.tibco.com/products/tibco-managed-file-transfer-internet-server-8-7-0
  * https://docs.tibco.com/pub/mftis/8.7.0/doc/pdf/TIB_mftis_8.7.0_container-deployment.pdf?id=7

