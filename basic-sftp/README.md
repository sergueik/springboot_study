### Info
replica of https://github.com/rodossaenz/java-ftp-client-quickstart

FTP CLIENT JAVA EXAMPLE
This sample demonstrates how to make basic client to FTP/SFTP Server. It has 6 main classes:

Upload file to FTP
Upload file to SFTP
Upload file to SFTP using authentication Keys
Download file from FTP
Download file from SFTP
Download file from SFTP using authentication Keys
For libraries required see maven pom.xml

See also [](https://hub.docker.com/r/atmoz/sftp)
```sh
docker pull atmoz/sftp
export USER=foo
docker run \
    -v $PWD/keys/id_rsa.pub:/home/$USER/.ssh/keys/id_rsa.pub:ro \
    -v $PWD/share:/home/$USER/share \
    -p 2222:22 -d atmoz/sftp \
    $USER::1001

```

### Troublshoting
with `atmoz/sftp:alpine`:

```text
[/usr/local/bin/create-sftp-user] Parsing user data: "foo::1001"
cat: read error: Is a directory
/usr/local/bin/create-sftp-user: Error on line 90: cat "$publickey" >> "$userKeysAllowedFileTmp"
/entrypoint: Error on line 60: create-sftp-user "$user"

```
https://hub.docker.com/r/pacnpal/simple-sftp-server
```sh
docker pull pacnpal/simple-sftp-server
```

```sh
mkdir -p /home/$USER/.ssh_keys/simple-sftp /home/$USER/.ssh_keys/simple-sftp-host /home/$USER/sftp_data
chmod 700 /home/$USER/.ssh_keys /home/$USER/.ssh_keys/simple-sftp /home/$USER/.ssh_keys/simple-sftp-host
```

```sh
docker run -d --name simple-sftp-server \
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
  pacnpal/simple-sftp-server
```

```
ls ~/.ssh_keys/simple-sftp
```
```text
authorized_keys  sftpuser_key  sftpuser_key.pub
```
```sh
sftp -i ~/.ssh_keys/simple-sftp/sftpuser_key -P 2222 sftpuser@localhost
```
```sh
Connected to localhost.
sftp> exit
```
https://hub.docker.com/r/pacnpal/simple-sftp-server

```
java -cp target/java-ftp-client-quickstart-1.0.jar:target/lib/* com.rodosaenz.ftp.client.SFTPKeyClientUpload
```
```
May 15, 2026 8:59:00 PM org.apache.commons.vfs2.impl.StandardFileSystemManager info
INFO: Using "/tmp/vfs_cache" as temporary files store.


SFTP_DOWNLOADED_WITH_KEY.txt
sftp -i ~/.ssh_keys/simple-sftp/sftpuser_key -P 2222 sftpuser@localhost
Connected to localhost.
sftp> put SFTP_DOWNLOADED_WITH_KEY.txt
Uploading SFTP_DOWNLOADED_WITH_KEY.txt to /home/sftpuser/SFTP_DOWNLOADED_WITH_KEY.txt
dest open "/home/sftpuser/SFTP_DOWNLOADED_WITH_KEY.txt": Permission denied

 put SFTP_DOWNLOADED_WITH_KEY.txt
Uploading SFTP_DOWNLOADED_WITH_KEY.txt to /home/sftpuser/data/SFTP_DOWNLOADED_WITH_KEY.txt
SFTP_DOWNLOADED_WITH_KEY.txt                  100%    0     0.0KB/s   00:00    
sftp> ls
SFTP_DOWNLOADED_WITH_KEY.txt            
sftp> 


### See Also

   * https://github.com/emberstack/docker-sftp - stadard SFTP Server for Docker, plus a .Net 6.0 application proxying it for unknown purpose.
