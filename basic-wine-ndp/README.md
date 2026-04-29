### Usage

```sh
docker pull gaestraidr/winetricks-alpine
```

```
docker run -it --name 'basic-wine-ndp' gaestraidr/winetricks-alpine sh 
```
in the shell run
```sh
winetricks --unattended dotnet45
```
ignore the warning 
```text
warning: taskset/cpuset not available on your platform!
```
> NOTE the app appears to be running forever:
```text
Preparing: C:\af69ea22571ec65635e2b6f24e944a6a\netfx_Full.mzz...
exit status 194 - normal, user selected 'restart later'
Using native override for following DLLs: mscoree
Executing wine regedit /S C:\windows\Temp\_dotnet45\override-dll.reg
------------------------------------------------------
Setting Windows version to 2003, otherwise applications using .NET 4.5 will subtly fail
------------------------------------------------------
The operation completed successfully
Setting Windows version to win2k3
Executing wine regedit /S C:\windows\Temp\_dotnet45\set-winver.reg
------------------------------------------------------
Running /usr/bin/wineserver -w. This will hang until all wine processes in prefix=/home/container/.wine terminate
------------------------------------------------------

```


```sh
docker images ls 
```
```text
gaestraidr/winetricks-alpine:latest
                                6629b44292cb        864MB             0B 
```
```
docker ps
```
```text
CONTAINER ID   IMAGE          COMMAND                  CREATED          STATUS          PORTS     NAMES
0ea3fdacb3c8   19e669e6ac2d   "/bin/sh -c 'winetri…"   11 minutes ago   Up 11 minutes             nifty_shaw
```
copy binaries
```
pushd Server\bin\Debug
scp * sergueik@192.168.12.151:src/springboot_study/basic-wine-ndp/app
```
```sh
IMAGE='gaestraidr/winetricks-alpine'
ID=$(docker container ls |grep $IMAGE | awk '{print $1'})
```
```sh
docker cp ./app $ID:/tmp
```
```
docker exec -it $ID sh
```
in container shell
```sh
cd /tmp/app
wine HttpServer.exe
```
this will reply with 
```text
Sever is running at http://0.0.0.0:4050
```


```cmd
IMAGE='gaestraidr/winetricks-alpine'
ID=$(docker container ls |grep $IMAGE | awk '{print $1'})
docker exec -it $ID netstat -ant
```
```text
Active Internet connections (servers and established)
Proto Recv-Q Send-Q Local Address           Foreign Address         State
tcp        0      0 0.0.0.0:4050            0.0.0.0:*               LISTEN`:w
```
```sh
IMAGE='gaestraidr/winetricks-alpine'
ID=$(docker container ls |grep $IMAGE | awk '{print $1'})
docker exec -it $ID curl -s -H 'Content-Type: application/json' http://localhost:4050/data.json
```
```text
<html><body><h1>404 - Not Found</h1></body></html>
```
```sh
echo '{"foo": "bar"}' > data.json
```
```sh
docker cp data.json $ID:/tmp/app
```
```text
Successfully copied 2.05kB to c9df5669806f:/tmp/app
```
```sh
docker exec -it $ID curl -s -H 'Content-Type: application/json' http://localhost:4050/data.json
```
```json
{"foo": "bar"}
```
### Cleanup
```sh
docker stop $ID
docker container prune -f
docker image rm $IMAGE
```

### Alternative Application
`simple-minihttpserver`
```cmd
pushd Tests\bin\Debug
scp * sergueik@192.168.12.151:src/springboot_study/basic-wine-ndp/app```cmd
```
in container 

```sn
wine MiniHttpdConsole.exe
```
```text
error: XDG_RUNTIME_DIR not set in the environment.
Server: MiniHttpd/1.2.0.19439
CLR: 4.0.30319.17929
Server running at http://dc87e98c210f:9091/

Main Menu
---------
?       Show menu items
u       Show server status
a       Toggle auto start server on load (True)
s       Stop server
b       Browse file tree
r       Toggle root folder type (VirtualDirectory)
t       Set root path (None)
i       Toggle index page style (IndexPage)
n       Set host name (dc87e98c210f)
p       Set port (9091)
h       Help
ls      Log to screen (True)
lf      Log to file (False)
lc      Log connections (False)
lr      Log requests (False)
w       Save settings
wq      Save settings and quit
q!      Discard changes and quit
Selection: u

MiniHttpd
---------
Running
Server URL: http://dc87e98c210f:9091/
Root type: VirtualDirectory
Auto start: True
Index page: IndexPage
Log to screen: True
Log to file: False
Log connections: False
Log requests: False

Selection:
```
```sh
docker exec -it basic-wine-ndp sh
```
in container
```sh
apt-get install net-tools
apt-get install curl
```
```sh
netstat -ant
```
```text
Active Internet connections (servers and established)
Proto Recv-Q Send-Q Local Address           Foreign Address         State
tcp        0      0 0.0.0.0:9091            0.0.0.0:*               LISTEN
tcp        0      0 172.17.0.2:55318        151.101.130.132:80      TIME_WAIT
```
```sh
curl http://localhost:9091
```
```text
<html>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<head><title>Index of /</title></head>
<body>
<h2>Index of /</h2>
<hr>MiniHttpd/1.2.0.19439
</body></html>
</body>
</html>
```
```sh
 curl http://localhost:9091/xx
```
```text
<html><head><title>404 Not Found</title></head>
<body><h2>404 Not Found</h2>
<hr>MiniHttpd/1.2.0.19439
</body></html>
```


### Related Info
```
WINEPREFIX=~/.wine_dotnet WINEARCH=win32 winetricks -q dotnet48
```

.Net Framework Wine [compatibility matrix](https://appdb.winehq.org/objectManager.php?sClass=application&iId=2586) 
.Net Framework 4.6.1 [ Offline installer](https://download.microsoft.com/download/E/4/1/E4173890-A24A-4936-9FC9-AF930FE3FA40/NDP461-KB3102436-x86-x64-AllOS-ENU.exe)  (NOTE: 64.5 MB)

### See Also

 Docker hub [docker-wine](https://hub.docker.com/r/scottyhardy/docker-wine/) and [repository](https://github.com/scottyhardy/docker-wine)
 by Scotty Hardy, with Wine и Winetricks. 
 Can be configured to run headless (RDP server mode) - based on ubuntu (through [scottyhardy/docker-remote-desktop](https://github.com/scottyhardy/docker-remote-desktop/blob/master/Dockerfile)) 
 NOTE: 1.2GB heavy packed image.

The better alternative is to start with docker image with wine and .net included
  * [chambm/wine-dotnet:wine7-net4.8-x64](https://hub.docker.com/layers/chambm/wine-dotnet/wine7-net4.8-x64/images/sha256-289fa007f8fe6d2fe34a5774c52ad0816208036dfe53a1aa3fc5a80042901983)
  * [fireant456/hellion-wine-dotnet452](https://hub.docker.com/layers/fireant456/hellion-wine-dotnet452/pterodactyl/images/sha256-eaa1d8263e5c3e324d6d161382b6e7be42a6b56375d1a32ecdb38b3c7b346529)
  * [nyamisty/docker-wine-dotnet](https://hub.docker.com/layers/nyamisty/docker-wine-dotnet/win32-stable-9.0/images/sha256-6aaa91819faac820db7ac9362e533492abcda62e2ce8fa2f891914631555169a) - both 32 and 64 bit available (over __1.9 GB__)
  * [wine-dotnet/stable-45-vnc](https://hub.docker.com/layers/shiftinv/wine-dotnet/stable-45-vnc/images/sha256-2dbf61b8636142ad45ef23ebbb41747912f88f9543e3d310c9b80772cc5355e1P) 
  * [shiftinv/wine-dotnet](https://hub.docker.com/layers/shiftinv/wine-dotnet/stable-45-vnc/images/sha256-2dbf61b8636142ad45ef23ebbb41747912f88f9543e3d310c9b80772cc5355e1) - ideal, wine-dotnet `stable-45-vnc` tag
  
yet another alternative is use one of the __WIX Toolset__ images

