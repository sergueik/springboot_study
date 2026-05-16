### Info

Upgraded replica of [rodossaenz/java-ftp-client-quickstart](https://github.com/rodossaenz/java-ftp-client-quickstart)
Java samples to download and upload files using FTP and SFTP
tested with "A dead simple SFTP server in Docker" [pacnpal/simple-sftp-server](https://hub.docker.com/r/pacnpal/simple-sftp-server)

Confirmed Upload /Download file from SFTP using authentication Keys scenarios

### Usage

Follow the instructions from [docker hub](https://hub.docker.com/r/pacnpal/simple-sftp-server)

```sh
IMAGE='pacnpal/simple-sftp-server'
docker pull $IMAGE
```

```sh
mkdir -p /home/$USER/.ssh_keys/simple-sftp /home/$USER/.ssh_keys/simple-sftp-host /home/$USER/sftp_data
chmod 700 /home/$USER/.ssh_keys /home/$USER/.ssh_keys/simple-sftp /home/$USER/.ssh_keys/simple-sftp-host
rm /home/$USER/.ssh_keys/simple-sftp/* /home/$USER/.ssh_keys/simple-sftp-host/*
```

```sh
NAME='sftp-server'
docker run -d --name $NAME \
  --user "$(id -u):$(id -g)" \
  --cap-add SYS_CHROOT \
  -p 2222:2022 \
  -e PUID="$(id -u)" \
  -e PGID="$(id -g)" \
  -e SSH_KEY_DIR=/keys \
  -e HOST_KEY_DIR=/host_keys \
  -e SFTP_CHROOT=false \
  -v /home/$USER/.ssh_keys/simple-sftp:/keys \
  -v /home/$USER/.ssh_keys/simple-sftp-host:/host_keys \
  -v /home/$USER/sftp_data:/home/sftpuser/data \
  $IMAGE
```
> NOTE changed argumens to have `SFTP_CHROOT=false` - the opposite was leading to runtime error possily due to older docker daemon.

```sh
ls ~/.ssh_keys/simple-sftp
```
```text
authorized_keys  sftpuser_key  sftpuser_key.pub
```
* test
```sh
sftp -i ~/.ssh_keys/simple-sftp/sftpuser_key -P 2222 sftpuser@localhost
```
```sh
Connected to localhost.
sftp> exit
```
> NOTE if running multiple times will see host key error:
```text
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@    WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!     @
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
IT IS POSSIBLE THAT SOMEONE IS DOING SOMETHING NASTY!
Someone could be eavesdropping on you right now (man-in-the-middle attack)!
It is also possible that a host key has just been changed.
The fingerprint for the ED25519 key sent by the remote host is
SHA256:EjGKbMD2iRX8cRTeLN7GGoHyWh9H9ZefyDqD41xJyjk.
Please contact your system administrator.
Add correct host key in /home/sergueik/.ssh/known_hosts to get rid of this message.
Offending ECDSA key in /home/sergueik/.ssh/known_hosts:7
```
remove the old key:
```sh
sed -i '7d' ~/.ssh/known_hosts
```
you may need to repeat
```txt
The authenticity of host '[localhost]:2222 ([127.0.0.1]:2222)' can't be established.
ED25519 key fingerprint is SHA256:EjGKbMD2iRX8cRTeLN7GGoHyWh9H9ZefyDqD41xJyjk.
This key is not known by any other names
Are you sure you want to continue connecting (yes/no/[fingerprint])? 
```
```text
yes
```
```text
Connected to localhost.
sftp> exit
```
> NOTE: That warning only appears when using native CLI sftp, not Java.

And it happens because:

* Docker container regenerates host keys on restart
* OpenSSH sees a different ED25519 key each time
* it assumes MITM risk → refuses silently unless overridden


The java code already has
```java
SftpFileSystemConfigBuilder.getInstance()
    .setStrictHostKeyChecking(opts, "no");
```
This fully disables:

* verification
* persistence
* prompts

So test becomes stateless.
One can also:
```sh
sftp \
  -o StrictHostKeyChecking=no \
  -o UserKnownHostsFile=/dev/null \
  -i ~/.ssh_keys/simple-sftp/sftpuser_key \
  -P 2222 \
  sftpuser@localhost
```
```sh
mvn clean package
```
```sh
touch SFTP_UPLOADED_WITH_KEY.txt
java -cp target/java-ftp-client-quickstart-1.3-SNAPSHOT.jar:target/lib/* example.SFTPKeyClientUpload -filepath SFTP_UPLOADED_WITH_KEY.txt
```
```text
INFO: Authentication succeeded (publickey).
File upload successful
```
```sh
java -cp target/java-ftp-client-quickstart-1.3-SNAPSHOT.jar:target/lib/* example.SFTPKeyClientDownload -filepath SFTP_UPLOADED_WITH_KEY.txt

```
```text
INFO: Authentication succeeded (publickey).
File download successfully
```
### Troubleshooting

```sh
java -cp target/java-ftp-client-quickstart-1.0.jar:target/lib/* com.rodosaenz.ftp.client.SFTPKeyClientUpload
```
```text
org.apache.commons.vfs2.FileSystemException: Badly formed URI "sftp://sftpuser:@localhost:2222/home/sftpuser/data/SFTP_UPLOADED_WITH_KEY.txt".
org.apache.commons.vfs2.FileSystemException: Badly formed URI "sftp://sftpuser:@localhost:2222/home/sftpuser/data/SFTP_UPLOADED_WITH_KEY.txt".
	at org.apache.commons.vfs2.provider.url.UrlFileProvider.findFile(UrlFileProvider.java:90)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:823)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:726)
	at com.rodosaenz.ftp.client.SFTPKeyClientUpload.main(SFTPKeyClientUpload.java:60)
Caused by: java.net.MalformedURLException: unknown protocol: sftp
	at java.base/java.net.URL.<init>(URL.java:652)
	at java.base/java.net.URL.<init>(URL.java:541)
	at java.base/java.net.URL.<init>(URL.java:488)
	at org.apache.commons.vfs2.provider.url.UrlFileProvider.findFile(UrlFileProvider.java:71)
	... 3 more

```
```text 
java.net.MalformedURLException: unknown protocol: sftp
```
solved by adding missing commons-vfs2-sftp dependency
```xml
<dependency>
  <groupId>com.jcraft</groupId>
  <artifactId>jsch</artifactId>
  <version>0.1.55</version>
</dependency>
```
* cannot read private key:
```text
INFO: Using "/tmp/vfs_cache" as temporary files store.
org.apache.commons.vfs2.FileSystemException: Could not connect to SFTP server at "sftp://sftpuser@localhost:2222/".
org.apache.commons.vfs2.FileSystemException: Could not connect to SFTP server at "sftp://sftpuser@localhost:2222/".
	at org.apache.commons.vfs2.provider.sftp.SftpFileProvider.doCreateFileSystem(SftpFileProvider.java:108)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.getFileSystem(AbstractOriginatingFileProvider.java:102)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.findFile(AbstractOriginatingFileProvider.java:80)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.findFile(AbstractOriginatingFileProvider.java:64)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:804)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:726)
	at com.rodosaenz.ftp.client.SFTPKeyClientUpload.main(SFTPKeyClientUpload.java:60)
Caused by: org.apache.commons.vfs2.FileSystemException: Could not load private key from "org.apache.commons.vfs2.provider.sftp.IdentityInfo@58651fd0".
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.addIndentity(SftpClientFactory.java:208)
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.addIdentities(SftpClientFactory.java:183)
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.createConnection(SftpClientFactory.java:89)
	at org.apache.commons.vfs2.provider.sftp.SftpFileProvider.doCreateFileSystem(SftpFileProvider.java:97)
	... 6 more
Caused by: com.jcraft.jsch.JSchException: invalid privatekey: [B@396a51ab
	at com.jcraft.jsch.KeyPair.load(KeyPair.java:664)
	at com.jcraft.jsch.KeyPair.load(KeyPair.java:561)
	at com.jcraft.jsch.IdentityFile.newInstance(IdentityFile.java:40)
	at com.jcraft.jsch.JSch.addIdentity(JSch.java:423)
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.addIndentity(SftpClientFactory.java:204)
	... 9 more


```
solved by upgrading to
```xml
<dependency>
  <groupId>com.github.mwiede</groupId>
  <artifactId>jsch</artifactId>
  <version>0.2.20</version>
</dependency>
```
- newer do not natively
```text
org.apache.commons.vfs2.FileSystemException: Could not connect to SFTP server at "sftp://sftpuser@localhost:2222/".
org.apache.commons.vfs2.FileSystemException: Could not connect to SFTP server at "sftp://sftpuser@localhost:2222/".
	at org.apache.commons.vfs2.provider.sftp.SftpFileProvider.doCreateFileSystem(SftpFileProvider.java:108)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.getFileSystem(AbstractOriginatingFileProvider.java:102)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.findFile(AbstractOriginatingFileProvider.java:80)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.findFile(AbstractOriginatingFileProvider.java:64)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:804)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:726)
	at com.rodosaenz.ftp.client.SFTPKeyClientUpload.main(SFTPKeyClientUpload.java:60)
Caused by: org.apache.commons.vfs2.FileSystemException: Could not load private key from "org.apache.commons.vfs2.provider.sftp.IdentityInfo@58651fd0".
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.addIndentity(SftpClientFactory.java:208)
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.addIdentities(SftpClientFactory.java:183)
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.createConnection(SftpClientFactory.java:89)
	at org.apache.commons.vfs2.provider.sftp.SftpFileProvider.doCreateFileSystem(SftpFileProvider.java:97)
	... 6 more
Caused by: com.jcraft.jsch.JSchException: invalid privatekey: [B@396a51ab
	at com.jcraft.jsch.KeyPair.load(KeyPair.java:664)
	at com.jcraft.jsch.KeyPair.load(KeyPair.java:561)
	at com.jcraft.jsch.IdentityFile.newInstance(IdentityFile.java:40)
	at com.jcraft.jsch.JSch.addIdentity(JSch.java:423)
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.addIndentity(SftpClientFactory.java:204)
	... 9 more

```
* check keys:
```sh
ssh-keygen -lf ~/.ssh_keys/simple-sftp/sftpuser_key
```
```text
256 SHA256:olI6CprnnjMYRy2/kLsdHuqMRueaZ/JEsmsjGEUqOf4 sftpuser@sftp-server (ED25519)
```
```sh
ssh-keygen -lf ~/.ssh_keys/simple-sftp/sftpuser_key.pub 
```
```text
256 SHA256:olI6CprnnjMYRy2/kLsdHuqMRueaZ/JEsmsjGEUqOf4 sftpuser@sftp-server (ED25519)
```
note
```text
org.apache.commons.vfs2.provider.sftp.SftpClientFactory log INFO: ssh-ed25519 is not available.
...
org.apache.commons.vfs2.FileSystemException: Could not connect to SFTP server at "sftp://sftpuser@localhost:2222/".
	at org.apache.commons.vfs2.provider.sftp.SftpFileProvider.doCreateFileSystem(SftpFileProvider.java:108)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.getFileSystem(AbstractOriginatingFileProvider.java:102)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.findFile(AbstractOriginatingFileProvider.java:80)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.findFile(AbstractOriginatingFileProvider.java:64)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:804)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:726)
	at com.rodosaenz.ftp.client.SFTPKeyClientUpload.main(SFTPKeyClientUpload.java:60)
Caused by: org.apache.commons.vfs2.FileSystemException: Could not connect to SFTP server at "localhost".
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.createConnection(SftpClientFactory.java:170)
	at org.apache.commons.vfs2.provider.sftp.SftpFileProvider.doCreateFileSystem(SftpFileProvider.java:97)
	... 6 more
Caused by: com.jcraft.jsch.JSchException: Auth fail for methods 'publickey'
	at com.jcraft.jsch.Session.connect(Session.java:531)
	at com.jcraft.jsch.Session.connect(Session.java:209)
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.createConnection(SftpClientFactory.java:166)
	... 7 more
```

* missing algorithm 

client cannot actually sign with `ED25519`

fixed by adding 
[BouncyCastle](https://en.wikipedia.org/wiki/Bouncy_Castle_(cryptography)) provider
```xml
<dependency>
  <groupId>org.bouncycastle</groupId>
  <artifactId>bcprov-jdk18on</artifactId>
  <version>1.78.1</version>
</dependency>
```
 
```text
[INFO] --- maven-dependency-plugin:2.8:copy-dependencies (default) @ java-ftp-client-quickstart ---
[INFO] Copying bcprov-jdk18on-1.78.1.jar to /home/sergueik/src/springboot_study/basic-sftp/target/lib/bcprov-jdk18on-1.78.1.jar
[INFO] commons-net-3.9.0.jar already exists in destination.
[INFO] commons-vfs2-2.9.0.jar already exists in destination.
[INFO] commons-logging-1.2.jar already exists in destination.
[INFO] okhttp-2.7.5.jar already exists in destination.
[INFO] jsch-2.27.7.jar already exists in destination.
[INFO] jackson-databind-2.10.5.1.jar already exists in destination.
[INFO] hadoop-hdfs-client-3.3.1.jar already exists in destination.
[INFO] jackson-core-2.10.5.jar already exists in destination.
[INFO] commons-lang3-3.12.0.jar already exists in destination.
[INFO] jackson-annotations-2.10.5.jar already exists in destination.
[INFO] okio-1.6.0.jar already exists in destination.
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  14.746 s
[INFO] Finished at: 2026-05-16T08:56:34-04:00
[INFO] ------------------------------------------------------------------------
```
* virtual directory error:

```sh
java -cp target/java-ftp-client-quickstart-1.0.jar:target/lib/* com.rodosaenz.ftp.client.SFTPKeyClientUpload 2>&1 | tee a.log
```
```text
May 16, 2026 8:56:39 AM org.apache.commons.vfs2.impl.StandardFileSystemManager info
INFO: Using "/tmp/vfs_cache" as temporary files store.
May 16, 2026 8:56:40 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: Connecting to localhost port 2222
May 16, 2026 8:56:40 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: Connection established
May 16, 2026 8:56:40 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: Remote version string: SSH-2.0-OpenSSH_9.7

...
INFO: server proposal: KEX algorithms: sntrup761x25519-sha512@openssh.com,curve25519-sha256,curve25519-sha256@libssh.org,ecdh-sha2-nistp256,ecdh-sha2-nistp384,ecdh-sha2-nistp521,diffie-hellman-group-exchange-sha256,diffie-hellman-group16-sha512,diffie-hellman-group18-sha512,diffie-hellman-group14-sha256,ext-info-s,kex-strict-s-v00@openssh.com
May 16, 2026 8:56:42 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: server proposal: host key algorithms: rsa-sha2-512,rsa-sha2-256,ecdsa-sha2-nistp521,ssh-ed25519
...
INFO: SSH_MSG_NEWKEYS sent
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: SSH_MSG_NEWKEYS received
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: Reset incoming sequence number after receiving SSH_MSG_NEWKEYS for strict KEX
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: SSH_MSG_EXT_INFO sent
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: SSH_MSG_SERVICE_REQUEST sent
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: SSH_MSG_EXT_INFO received
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: server-sig-algs=<ssh-ed25519,ecdsa-sha2-nistp256,ecdsa-sha2-nistp384,ecdsa-sha2-nistp521,sk-ssh-ed25519@openssh.com,sk-ecdsa-sha2-nistp256@openssh.com,rsa-sha2-512,rsa-sha2-256>
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: SSH_MSG_SERVICE_ACCEPT received
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: SSH_MSG_EXT_INFO received
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: server-sig-algs=<ssh-ed25519,ecdsa-sha2-nistp256,ecdsa-sha2-nistp384,ecdsa-sha2-nistp521,sk-ssh-ed25519@openssh.com,sk-ecdsa-sha2-nistp256@openssh.com,rsa-sha2-512,rsa-sha2-256>
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: Authentications that can continue: publickey,keyboard-interactive,password
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: Next authentication method: publickey
May 16, 2026 8:56:43 AM org.apache.commons.vfs2.provider.sftp.SftpClientFactory log
INFO: Authentication succeeded (publickey).
org.apache.commons.vfs2.FileSystemException: Could not copy "file:///home/sergueik/src/springboot_study/basic-sftp/SFTP_UPLOADED_WITH_KEY.txt" to "sftp://sftpuser@localhost:2222/home/sftpuser/data/SFTP_UPLOADED_WITH_KEY.txt".
org.apache.commons.vfs2.FileSystemException: Could not copy "file:///home/sergueik/src/springboot_study/basic-sftp/SFTP_UPLOADED_WITH_KEY.txt" to "sftp://sftpuser@localhost:2222/home/sftpuser/data/SFTP_UPLOADED_WITH_KEY.txt".
	at org.apache.commons.vfs2.provider.AbstractFileObject.copyFrom(AbstractFileObject.java:347)
	at com.rodosaenz.ftp.client.SFTPKeyClientUpload.main(SFTPKeyClientUpload.java:63)
Caused by: org.apache.commons.vfs2.FileSystemException: Could not create folder "sftp://sftpuser@localhost:2222/home".
	at org.apache.commons.vfs2.provider.AbstractFileObject.createFolder(AbstractFileObject.java:436)
	at org.apache.commons.vfs2.provider.AbstractFileObject.createFolder(AbstractFileObject.java:419)
	at org.apache.commons.vfs2.provider.AbstractFileObject.createFolder(AbstractFileObject.java:419)
	at org.apache.commons.vfs2.provider.AbstractFileObject.getOutputStream(AbstractFileObject.java:1392)
	at org.apache.commons.vfs2.provider.DefaultFileContent.getOutputStream(DefaultFileContent.java:479)
	at org.apache.commons.vfs2.provider.DefaultFileContent.getOutputStream(DefaultFileContent.java:457)
	at org.apache.commons.vfs2.provider.DefaultFileContent.write(DefaultFileContent.java:825)
	at org.apache.commons.vfs2.provider.DefaultFileContent.write(DefaultFileContent.java:849)
	at org.apache.commons.vfs2.FileUtil.copyContent(FileUtil.java:90)
	at org.apache.commons.vfs2.provider.AbstractFileObject.copyFrom(AbstractFileObject.java:338)
	... 1 more
Caused by: 3: Permission denied
	at com.jcraft.jsch.ChannelSftp.throwStatusError(ChannelSftp.java:2830)
	at com.jcraft.jsch.ChannelSftp.mkdir(ChannelSftp.java:2119)
	at org.apache.commons.vfs2.provider.sftp.SftpFileObject.doCreateFolder(SftpFileObject.java:210)
	at org.apache.commons.vfs2.provider.AbstractFileObject.createFolder(AbstractFileObject.java:425)
	... 10 more

```
fixed by modifying virtual path:
```java
            //Create the SFTP URI using the host name, userid, no password,  remote path and file name
-            String sftpUri = "sftp://" + user + ":@" + server + ":2222" + "/home/sftpuser/data/" + filepath;
+            String sftpUri = "sftp://" + user + ":@" + server + ":2222" + "/data/" + filepath;
```
because with `setUserDirIsRoot(opts, true)` the `sftp` user home already is `/`.

* verify

```sh
docker exec -it bebf ls /home/sftpuser/data
```
```text
SFTP_UPLOADED_WITH_KEY.txt
```
```sh
java -cp target/java-ftp-client-quickstart-1.1-SNAPSHOT.jar:target/lib/* com.rodosaenz.ftp.client.SFTPKeyClientUpload
```
```text
org.apache.commons.vfs2.FileSystemException: Could not connect to SFTP server at "sftp://sftpuser@localhost:2222/".
org.apache.commons.vfs2.FileSystemException: Could not connect to SFTP server at "sftp://sftpuser@localhost:2222/".
	at org.apache.commons.vfs2.provider.sftp.SftpFileProvider.createSession(SftpFileProvider.java:72)
	at org.apache.commons.vfs2.provider.sftp.SftpFileProvider.doCreateFileSystem(SftpFileProvider.java:92)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.getFileSystem(AbstractOriginatingFileProvider.java:92)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.findFile(AbstractOriginatingFileProvider.java:71)
	at org.apache.commons.vfs2.provider.AbstractOriginatingFileProvider.findFile(AbstractOriginatingFileProvider.java:55)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:788)
	at org.apache.commons.vfs2.impl.DefaultFileSystemManager.resolveFile(DefaultFileSystemManager.java:835)
	at com.rodosaenz.ftp.client.SFTPKeyClientUpload.main(SFTPKeyClientUpload.java:60)
Caused by: org.apache.commons.vfs2.FileSystemException: Could not connect to SFTP server at "localhost".
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.createConnection(SftpClientFactory.java:228)
	at org.apache.commons.vfs2.provider.sftp.SftpFileProvider.createSession(SftpFileProvider.java:65)
	... 7 more
Caused by: java.lang.ClassCastException: class java.lang.Integer cannot be cast to class java.time.Duration (java.lang.Integer and java.time.Duration are in module java.base of loader 'bootstrap')
	at org.apache.commons.vfs2.FileSystemConfigBuilder.getDuration(FileSystemConfigBuilder.java:281)
	at org.apache.commons.vfs2.provider.sftp.SftpFileSystemConfigBuilder.getSessionTimeout(SftpFileSystemConfigBuilder.java:386)
	at org.apache.commons.vfs2.provider.sftp.SftpClientFactory.createConnection(SftpClientFactory.java:164)
	... 8 more

```
fixed by
```java
-            SftpFileSystemConfigBuilder.getInstance().setTimeout(opts, 10000);
+SftpFileSystemConfigBuilder.getInstance()
+    .setConnectTimeout(^M
+        opts,
+        java.time.Duration.ofSeconds(10)
+    );
 
+SftpFileSystemConfigBuilder.getInstance()
+    .setSessionTimeout(
+        opts,
+        java.time.Duration.ofSeconds(10)
+    );
```
root cause: incremetal maven runs were not necessarily running a fully consistent build graph every time.
### Cleanup
```
ID=$(docker container ls | grep $IMAGE | awk '{print $1}')
docker stop $ID
docker container rm $ID
docker volume prune -f
docker image rm $IMAGE
```
### See Also

   * https://github.com/emberstack/docker-sftp - .net core (__.Net__ __6.0__) SFTP Server in Docker

---

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
