# Overview:

The Syncfusion&reg; PDF Viewer control allows you to view, print, form-fill, and annotate PDF files in your web applications. This PDF Viewer control requires a server-side backend Web API service to render PDF contents.

This Docker image is the predefined Docker container of Syncfusion’s PDF Viewer backend. You can deploy it quickly to your infrastructure.

# Prerequisites 

Have [Docker](https://www.docker.com/products/container-runtime#/download) installed in your environment: 

•	On Windows, install [Docker for Windows](https://hub.docker.com/editions/community/docker-ce-desktop-windows). 

•	On macOS, install [Docker for Mac](https://hub.docker.com/editions/community/docker-ce-desktop-windows).


# Configure the PDF Viewer server-side service 
 
This PDF Viewer component uses a server-side backend (web service) to render the pages and extract the PDF contents. We have provided the server-side backend (web service) as a docker image to quickly get started with our PDF Viewer component. 
 
Step 1: Pull the pdfviewer-server image from Docker Hub. 

```sh
docker pull syncfusion/pdfviewer-server 
```

**NOTE:** PDF Viewer is a commercial product, and it requires a valid license to use it in a production environment [request license or trial key](https://help.syncfusion.com/common/essential-studio/licensing/licensing-faq/where-can-i-get-a-license-key). 
 
Step 2: Create the docker-compose.yml file with the following code in your file system. 
  
```sh
version: '3.4'  
 
services:  
 pdfviewer-server: 
    image: syncfusion/pdfviewer-server:latest 
    environment:  
      #Provide your license key for activation 
       SYNCFUSION_LICENSE_KEY: YOUR_LICENSE_KEY 
    volumes:  
      -  C:\Docker\Data:/app/Data 
    ports: 
    - "6001:80" 
```

**NOTE:** You should mention the folder path which contains pdf files in the volumes section of compose file. 
 
Step 3: In a terminal tab, navigate to the directory where you’ve placed the docker-compose.yml file and execute the following. 

```sh
docker-compose up 
```

Also, you can run the Docker container along with the license key using this docker run command. 

```sh
docker run -d -p 6001:80 –e SYNCFUSION_LICENSE_KEY=YOUR_LICENSE_KEY syncfusion/pdfviewer-server:latest 
```

For Ex: docker run -d -p 6001:80 –e SYNCFUSION_LICENSE_KEY=Mzg1ODMzQDMxMzgyZTM0MmUzMGdFRGJvUno1MUx4Tit4S09CeS9xRHZzZU4ySVBjQVFuT0VpdWpHUWJ6aXM9 syncfusion/pdfviewer-server:latest 
Now the PDF Viewer server Docker instance runs in the localhost with the provided port number http://localhost:6001. Open this link in the browser and navigate to the PDF Viewer Web API control http://localhost:6001/api/pdfviewer. It returns the default get method response. 
 
