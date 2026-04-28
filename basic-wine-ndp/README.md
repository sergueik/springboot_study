:


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


```sh
docker pull shiftinv/wine-dotnet:stable-45-vnc
docker pull nyamisty/docker-wine-dotnet:win32-stable-9.0

```
```
WINEPREFIX=~/.wine_dotnet WINEARCH=win32 winetricks -q dotnet48
```

.Net Framework Wine [compatibility matrix](https://appdb.winehq.org/objectManager.php?sClass=application&iId=2586) 
.Net Framework 4.6.1 [ Offline installer](https://download.microsoft.com/download/E/4/1/E4173890-A24A-4936-9FC9-AF930FE3FA40/NDP461-KB3102436-x86-x64-AllOS-ENU.exe)  (NOTE: 64.5 MB)


Alternatively
```sh
docker pull marinobantli/alpine-wine:latest
```

```
docker run -it --name 'basic-wine-ndp' marinobantli/alpine-wine:latest sh 
```
ignore the messages from startup
```text
Starting Xvnc...

Xvnc TigerVNC 1.13.1 - built Jun  5 2023 20:51:54
Copyright (C) 1999-2022 TigerVNC Team and many others (see README.rst)
See https://www.tigervnc.org for information on TigerVNC.
Underlying X server release 12101007


Sun Apr 26 17:42:22 2026
 vncext:      VNC extension running!
 vncext:      Listening for VNC connections on all interface(s), port 5900
 vncext:      created VNC server for screen 0
The XKEYBOARD keymap compiler (xkbcomp) reports:
> Warning:          Could not resolve keysym XF86CameraAccessEnable
> Warning:          Could not resolve keysym XF86CameraAccessDisable
> Warning:          Could not resolve keysym XF86CameraAccessToggle
> Warning:          Could not resolve keysym XF86NextElement
> Warning:          Could not resolve keysym XF86PreviousElement
> Warning:          Could not resolve keysym XF86AutopilotEngageToggle
> Warning:          Could not resolve keysym XF86MarkWaypoint
> Warning:          Could not resolve keysym XF86Sos
> Warning:          Could not resolve keysym XF86NavChart
> Warning:          Could not resolve keysym XF86FishingChart
> Warning:          Could not resolve keysym XF86SingleRangeRadar
> Warning:          Could not resolve keysym XF86DualRangeRadar
> Warning:          Could not resolve keysym XF86RadarOverlay
> Warning:          Could not resolve keysym XF86TraditionalSonar
> Warning:          Could not resolve keysym XF86ClearvuSonar
> Warning:          Could not resolve keysym XF86SidevuSonar
> Warning:          Could not resolve keysym XF86NavInfo
Errors from xkbcomp are not fatal to the X server
[mi] mieq: warning: overriding existing handler 0 with 0x55b2a51f6a31 for event 2
[mi] mieq: warning: overriding existing handler 0 with 0x55b2a51f6a31 for event 3
Starting window manager...
Ready.
Processing specified CMD...
/ # Obt-Message: Xinerama extension is not present on the server
/usr/libexec/openbox-autostart: line 34: /usr/libexec/openbox-xdg-autostart: not found


```

```sh

    winetricks --unattended dotnet45
```
NOTE: [O[DEPRECATION NOTICE] Docker Image Format v1 and Docker Image manifest version 2, schema 1 support is disabled by default and will be removed in an upcoming release. Suggest the author of docker.io/boggart/alpine-apk-static-32bit:latest to upgrade the image to the OCI Format or Docker Image manifest v2, schema 2. More information at https://docs.docker.com/go/deprecated-image-specs/
sergueik@sergueik47:~/src/springboot_study/basic-wine-ndp$ 

### Usage

```sh
docker build -f Dockerfile  -t basic-wine-ndp .
```
```sh
docker images basic-wine-ndp:latest
```
                                                            i Info →   U  In Use
```text
IMAGE                   ID             DISK USAGE   CONTENT SIZE   EXTRA
basic-wine-ndp:latest   0b0303f84ee4       3.52GB             0B
```
```sh
docker run --name basic-wine-ndp -it basic-wine-ndp sh
```
```sh
docker cp . basic-wine-ndp:/tmp
```
in container
```sh
cd /tmp
# wine MiniHttpdConsole.exe
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

