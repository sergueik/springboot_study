
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


### See Also 
  * https://docs.tibco.com/pub/mftis/8.5.1/doc/pdf/TIB_mftis_8.5.1_container-deployment.pdf
