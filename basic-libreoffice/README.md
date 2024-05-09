### Info

This directory contains a replica of alpine based
[alpine based libreoffice container](https://github.com/woahbase/alpine-libreoffice) with fixes.

This container can be used to run `libreoffice` in batch mode, headlessly. 
The modified `Dockerfile` from the original project is provided,
with fixes to `ENTRYPOINT` and `DISPLAY`
in order the container to be long running server and have the headless libreoffice conversion runs be triggered from the Springboot app

### History

[Libreoffice](https://documentation.libreoffice.org/en/english-documentation/) recognizes the `--headless` option, 
which lets its process without requiring an X windows otherwise needed
to render the graphical user interface.
This is often used in combination with other switches when converting files between formats, including PDF, HTML, DOC, DOCX, EPUB, plain text, etc.

### Usage

* pull the external custom image
```sh
export BASE_IMAGE=woahbase/alpine-libreoffice:x86_64
docker pull $BASE_IMAGE
```
this image will take a whopping 700 Mb of disk space

```sh
docker images $BASE_IMAGE
```
```text
woahbase/alpine-libreoffice            x86_64                 1136a70e3a54   21 months ago   708MB
```


so if runing on __Docker Toolbox__ on downlevel Windows make sure to back up the VM first:
```sh
cd ~/.docker/machine/machines/default/
ls -hl disk.vmdk*
``
```text
-rw-r--r-- 1 Serguei 197609 5.0G Apr 13 12:26 disk.vmdk
-rw-r--r-- 1 Serguei 197609 3.1G Jan  2 10:33 disk.vmdk.SAVED
``` 
The VM is configured has dynamic VDI which does not shrink after purging the image

* execute in batch mode
   + the command 
```sh
export BASE_IMAGE=woahbase/alpine-libreoffice:x86_64
export NAME=docker_libreoffice
docker run --rm -it --name $NAME --hostname libreoffice -e PGID=1000 -e PUID=1000 -c 512 -m 2096m --headless -v /usr/share/fonts:/usr/share/fonts:ro -v data:/home/alpine\ --convert-to pdf /home/alpine/test.doc $BASE_IMAGE
```

would fail because it has the LibreOffice arguments in the wrong placed

  + the command
```sh
export BASE_IMAGE=woahbase/alpine-libreoffice:x86_64
export NAME=docker_libreoffice
docker run --rm -it --name $NAME --hostname libreoffice -e PGID=1000 -e PUID=1000 -c 512 -m 2096m -v /usr/share/fonts:/usr/share/fonts:ro -v data:/home/alpine $BASE_IMAGE /usr/bin/libreoffice --headless --convert-to pdf /home/alpine/test.doc
```
will fail complaining
```text
Error: source file could not be loaded
```
  
  + the command
```sh
export NAME=docker_libreoffice
export BASE_IMAGE=woahbase/alpine-libreoffice:x86_64
docker run --rm --entrypoint sh -it --name $NAME --hostname libreoffice -e PGID=1000 -e PUID=1000 -c 512 -m 2096m -v /usr/share/fonts:/usr/share/fonts:ro -v data:/home/alpine $BASE_IMAGE
```
this will open shell in the container

* continue inside the container
```sh
/usr/bin/libreoffice --headless --convert-to pdf /home/alpine/test.doc
```

this will succeed

```text
convert /home/alpine/test.doc -> /home/alpine/test.pdf using filter : writer_pdf_Export
```
   + the fixed command, running from Linux host using directory mappings for `fonts`

```sh
export NAME=docker_libreoffice
export BASE_IMAGE=woahbase/alpine-libreoffice:x86_64
docker run --rm -it --name $NAME --hostname libreoffice -e PGID=1000 -e PUID=1000 -c 512 -m 2096m -v /usr/share/fonts:/usr/share/fonts:ro -v $(pwd)/data:/home/alpine $BASE_IMAGE /usr/bin/libreoffice --headless --convert-to pdf /home/alpine/test.doc
```
works but the Libreoffice process does not terminate after conversion is  complete - one can copy the result file into the host and terminate container


  + the fixed command
```sh
export NAME=docker_libreoffice
export BASE_IMAGE=woahbase/alpine-libreoffice:x86_64
docker run --entrypoint sh --rm -it --name $NAME --hostname libreoffice -e PGID=1000 -e PUID=1000 -c 512 -m 2096m -v /usr/share/fonts:/usr/share/fonts:ro -v $(pwd)/data:/home/alpine $BASE_IMAGE /usr/bin/libreoffice --headless --convert-to pdf /home/alpine/test.doc
```
works successtully and does not suffer from this defect

### Building Container

use the stock alpine JRE image, temporarily skip certain fonts by commenting the line
```sh
RUN apk add --no-cache -U ttf-font-awesome ttf-mononoki ttf-hack
```

```sh
export IMAGE=basic-libreoffice
docker build -f Dockerfile -t $IMAGE .
```
this wll create image of slightly smaller size
```sh
docker images $IMAGE
```
```text
basic-libreoffice   latest              5b6660254780        28 seconds ago 671MB
```
```sh
export NAME=docker_libreoffice
export IMAGE=basic-libreoffice

docker run --entrypoint sh --rm -it --name $NAME --hostname libreoffice -e PGID=1000 -e PUID=1000 -c 512 -m 2096m -v /usr/share/fonts:/usr/share/fonts:ro -v $(pwd)/data:/home/alpine $IMAGE /usr/bin/libreoffice --headless --convert-to pdf /home/alpine/test.doc
```
currently prints few errors but completes the task:
```text
WARNING: Your kernel does not support swap limit capabilities or the cgroup is not mounted. Memory limited without swap.
javaldx: Could not find a Java Runtime Environment!
Warning: failed to read path from javaldx
convert /home/alpine/test.doc -> /home/alpine/test.pdf using filter : writer_pdf_Export
```
![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-libreoffice/screenshots/capture-libreoffice-container.png)

```sh
docker-machine ssh
```
```text
   ( '>')
  /) TC (\   Core is distributed with ABSOLUTELY NO WARRANTY.
 (/-_--_-\)           www.tinycorelinux.net
docker@default:~$
```
```sh
df -h
```
```text
Filesystem                Size      Used Available Use% Mounted on
tmpfs                   886.9M    274.8M    612.0M  31% /
tmpfs                   492.7M         0    492.7M   0% /dev/shm
/dev/sda1                17.8G      1.5G     15.4G   9% /mnt/sda1
cgroup                  492.7M         0    492.7M   0% /sys/fs/cgroup
/c/Users                437.1G    389.6G     47.5G  89% /c/Users
/dev/sda1                17.8G      1.5G     15.4G   9% /mnt/sda1/var/lib/docker
```
after pulling those heavy images:

```
Filesystem                Size      Used Available Use% Mounted on
Filesystem                Size      Used Available Use% Mounted on
tmpfs                   886.9M    274.8M    612.0M  31% /
tmpfs                   492.7M         0    492.7M   0% /dev/shm
/dev/sda1                17.8G      3.7G     13.2G  22% /mnt/sda1
cgroup                  492.7M         0    492.7M   0% /sys/fs/cgroup
/c/Users                437.1G    394.6G     42.5G  90% /c/Users
/dev/sda1                17.8G      3.7G     13.2G  22% /mnt/sda1/var/lib/docker
```

```
```sh
docker image rm  ad35bb55199c
```
```text
Untagged: lscr.io/linuxserver/libreoffice:amd64-7.3.7
Untagged: lscr.io/linuxserver/libreoffice@sha256:ff724cc03ca3806f0e67209c83404d5
04d5a4e12644d1ad5eb5f20b8808341ff
Deleted: sha256:ad35bb55199c332504e11e32fd2ccddd5779ce8015cb6529612092f0d9f36e32
Deleted: sha256:71c6bf08c6cbb86f69a04d1d1e89d90b279ac106c7240b0dee98dcd35c8944ce
Deleted: sha256:286380aa63581fdfbacd504fa0708b1c1e6c0f7a214912a80d031ed96d70c376
Deleted: sha256:ef3039586665a8a9da14ca05282913bb8a1e4a00c3c62468456aed2b2244565c
Deleted: sha256:020d206031c63d8ab7da0b09198b979869c6c7e4a84f7b51f772738219bc678c
Deleted: sha256:06aa7adf2c0486e35bd78be60c0ade39498544172b6b3f54a605e96d843bc5dc
Deleted: sha256:c04cb7bce0c866ccebf0048415ec61b640bd8ed9308cb8c548ff864546808abe
Deleted: sha256:0e37facecb34ef0e0a3a997d8829b365926a010dfd24438f0103af6cae0bf0fa
Deleted: sha256:14d4b19237890b36b4a12c1e2181aacb6a1bbd3c20d39af6745f93785d0e6366
Deleted: sha256:79230efc626bc15103d7af95480abde8c940e87c85eb1a32adaa3de448c4405d
Deleted: sha256:20babedeaba158d7b452dc272424d7423a3237671a81b4226f103b4c7fbf7f3f
```
```sh
docker image rm  1136a70e3a54
```
```text
Untagged: woahbase/alpine-libreoffice:x86_64
Untagged: woahbase/alpine-libreoffice@sha256:2b6a58f32991c5911b329a946effd6142be
2bab8db6f739fb9c6531536dfdaa9
Deleted: sha256:1136a70e3a547070249d952c65c39b7b0d1e8f8c3c0b0ca2c18045dbef36f62b
Deleted: sha256:a54b1ea5ddcee9ba4ea554aa57c188cd60c5b7ddd654e49495464b7654d72fa9
Deleted: sha256:77af0313ebfa763d88ac4e188ebb6b4b34295caf4db259d21bd7fb855a328f4c
Deleted: sha256:ab0b4e33219fb35521f7b67d6210233960f41e75d142569880dac0be1ae7fd1b
Deleted: sha256:41faffcae32f0e289369c33af284701c59e7f5d4a0912e871c9749ae5c0e85a7
Deleted: sha256:b33ce8f780a175f77fe863de867677bcc548ae4609679cb427f1adbd8bd18bf3
Deleted: sha256:c31851344fefb51e359457cb8f7cd8426aa1be95121a9816220dfd80622e1d0d
Deleted: sha256:1bfeebd65323b8ddf5bd6a51cc7097b72788bc982e9ab3280d53d3c613adffa7
```


the disk usage observed on Docker Machine Linux host will go back but the VDI  disk size observed on Windows Machine host will not
Furthermore if the image is pulled, removed and pulled again, the VDI size grows both times:

```sh
ls -hl disk.vmdk*
```
```text
-rw-r--r-- 1 Serguei 197609 6.2G Apr 13 12:26 disk.vmdk
-rw-r--r-- 1 Serguei 197609 3.1G Jan  2 10:33 disk.vmdk.SAVED

```
### Pre-built Web UI LibreOffice Container

The Libreoffice container with Web UI `lscr.io/linuxserver/libreoffice` is already Alpine based. It is also quite heavy, so pin of some specific past release version is recommended over `latest`
```sh
docker images $BASE_IMAGE
```
```text
lscr.io/linuxserver/libreoffice   amd64-7.3.7         ad35bb55199c        11 months ago       1.52GB
```


https://hub.docker.com/r/linuxserver/libreoffice/tags


* pull the external vendor old stable release image
```sh
export VERSION=amd64-7.3.7
export BASE_IMAGE=lscr.io/linuxserver/libreoffice:$VERSION
docker pull $BASE_IMAGE
```
```sh
docker run -d -p 3000:3000 $BASE_IMAGE
```

the link `http://localhost:3000/` 
or 

```sh
http://$(docker-machine ip):3000/
```
will display the desktop


![Libreoffice Web UI](https://github.com/sergueik/springboot_study/blob/master/basic-libreoffice/screenshots/capture-libreoffice-webui.png)

![Libreoffice Document Web UI](https://github.com/sergueik/springboot_study/blob/master/basic-libreoffice/screenshots/capture-libreoffice-document-webui.png)

### Building a Web UI

* keep the container running and build the application
```sh
mvn spring-boot:run
```

### Posting Upload via curl

* the correct way
```sh
curl -s -XPOST -F 'operation=send' -F 'param=data' -F "file=@$(pwd)/data.txt" http://localhost:8085/basic/upload
```
* NOTE: cannot combine request parameters in `-F`:
```sh
curl -s -XPOST -F "operation=send&param=data&file=@$(pwd)/data.txt" http://localhost:8085/basic/upload
```
```JSON
{
  "timestamp": "2023-01-19T00:36:10.177+00:00",
  "status": 400,
  "error": "Bad Request",
  "message": "",
  "path": "/basic/upload"
}
```
with application console log saying:
```text
ParameterException: Required String parameter 'param' is not present
```
* NOTE: cannot issue both `-d` and  `-F`:


```sh
curl -s -X POST -F "file=$(pwd)/data.txt" http://localhost:8085/basic/upload -d "operation=send"
```
```text
Warning: You can only select one HTTP request method! You asked for both POST
Warning: (-d, --data) and multipart formpost (-F, --form).
```
with request not being sent

* download binary file, instruct the server to encode it when logging
```sh
base64 1x1.png
```
```text
iVBORw0KGgoAAAANSUhEUgAAAAEAAAABAQMAAAAl21bKAAAAA1BMVEUAAACnej3aAAAAAXRSTlMAQObYZgAAAApJREFUCNdjYAAAAAIAAeIhvDMAAAAASUVORK5CYII=
```
invoke as
```sh
 curl -XPOST -F "operation=send" -F "param=data" -F "encode=true" -F "file=@$(pwd)/1x1.png" http://localhost:8085/basic/upload
```
on the server end see
```text
herServlet        : Complet
Processing 1x1.png
size:95/95
raw data(base64 encoded):
[B@45e171a6
```
 - not quite right, but close enough

### Windows Automation

* using Libreoffics in headless mode is popular on Windows for PDF conversion is practically the same:

```cmd
"c:\Program Files\LibreOffice\program\soffice.exe" --headless --convert-to pdf --outdir "%USERPROFILE%\Downloads"  "%USERPROFILE%\downloads\a.docx"
```
* the background run and / user ACL is a bit of a challenge

* interop is a [popular](https://www.cyberforum.ru/powershell/thread3166872.html#post17301582) alternative

```powershell
  pushd $shared_assemblies_path

  @(
'Spire.Doc.dll',
'Spire.Pdf.dll'

) | foreach-object {
    if ($host.Version.Major -gt 2) {
      unblock-file -Path $_
    }
    write-debug $_
    add-type -Path $_
  }

popd
$doc = new-object Spire.Doc.Document
$doc.LoadFromFile("${env:USERPROFILE}\a.doc")
$doc.SaveToFile("a.pdf",[Spire.Doc.FileFormat]::PDF )

```
### See Also



  * https://stackoverflow.com/questions/50982064/converting-docx-to-pdf-with-pure-python-on-linux-without-libreoffice
  * https://stackoverflow.com/questions/72434000/running-libreoffice-converter-on-docker  

  * https://docs.linuxserver.io/images/docker-libreoffice
  * https://medium.com/codex/libreoffice-on-docker-1a64245468c
  * [known bug](https://bugs.documentfoundation.org/show_bug.cgi?id=37531&redirected_from=fdo) of Libreoffice will not run in "batch mode" when there is another instance open
  * [document](https://www.baeldung.com/spring-file-upload) and [source](https://github.com/eugenp/tutorials/tree/master/spring-web-modules/spring-mvc-java) on Spring App file Upoad - too many projects in the same umbrella
  * [upload files through curl](https://medium.com/@petehouston/upload-files-with-curl-93064dcccc76)
  * [spring uloading gettin files started](https://spring.io/guides/gs/uploading-files/)
  * [1 pixel png used as spacer](https://commons.wikimedia.org/wiki/File:1x1.png)
  * https://stackoverflow.com/questions/35406213/how-to-copy-data-from-docker-volume-to-host
  * [standalone Office Development Suites for .Net](https://www.e-iceblue.com/Introduce/spire-office-for-net-free.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
