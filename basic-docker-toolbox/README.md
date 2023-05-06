### Info

### Notes
if seeing the communication error
```text
//./pipe/docker_engine: The system cannot find the file specified
```

apply the [resolution from forums.docker.com](https://forums.docker.com/t/docker-on-windows-fails-with-pipe-docker-engine-the-system-cannot-find-the-file-specified/28479/4)

* query environment

```sh
docker-machine env default
```

```sh
export DOCKER_TLS_VERIFY="1"
export DOCKER_HOST="tcp://192.168.99.101:2376"
export DOCKER_CERT_PATH="C:\Users\Serguei\.docker\machine\machines\default"
export DOCKER_MACHINE_NAME="default"
export COMPOSE_CONVERT_WINDOWS_PATHS="true"
```
 * convert to equivalent Windows environment setting commands:

```cmd
set DOCKER_TLS_VERIFY=1
set DOCKER_HOST=tcp://192.168.99.100:2376
set DOCKER_CERT_PATH=%USERPROFILE%\.docker\machine\machines\default
set DOCKER_MACHINE_NAME=default
set COMPOSE_CONVERT_WINDOWS_PATHS=true
```
run in the clean command shell
```cmd
@FOR /f "tokens=*" %i IN ('echo commands.txt') DO @%i
```


followed by the command from the original `Docker Quickstart Terminal` desktop shortcut

```cmd
"C:\Program Files\Git\bin\bash.exe" --login -i "C:\Program Files\Docker Toolbox\start.sh"
```
Alternatively make these settings permanently via __Control Panel__. The __Docker Toolbox__ installer apparently does not manage to do that on some machines.

### Challenge with Upgrade Docker compose to Certain 2.x Versions

Note: From the end of June 2023 Compose V1, is about to not be supported, and the last archive build of Docker ToolBox comes with Docker-compose version `1.24.1`

To upgrade via curl, need to use Windows account with write permission for `Program Files` directories.

First in git bash that is launched by __Docker Toolbox__

```sh
cd /c/Program Files/Docker Toolbox
cd $TEMP
VERSION=2.14.0
curl -sL https://github.com/docker/compose/releases/download/v$VERSION/docker-compose-windows-x86_64.exe -o docker-compose.exe
```
verify
```sh
./docker-compose.exe  --version
```
```text
Docker Compose version v2.14.0
```
second, in elevated CMD prompt
```cmd
copy /y %TEMP%\docker-compose.exe "c:\Program Files\Docker Toolbox"
```
NOTE the directory path

finally start a new __Docker Toolbox__ bash shell. Verify version of the `docker-compose` in the `PATH`:
```sh
docker-compose --version
```

```text
Docker Compose version v2.14.0
```
navigate to project directory and run a basic `docker-compose.yml`:
```sh
docker-compose up --build
```
it will show the normal console output
```txt
[+] Building 0.5s (12/12) FINISHED
 => [internal] load build definition from Dockerfile                       0.0s
 => => transferring dockerfile: 32B                                        0.0s
 => [internal] load .dockerignore                                          0.0s
 => => transferring context: 2B                                            0.0s
 => [internal] load metadata for docker.io/library/python:3.8.2-alpine     0.5s
 => [1/7] FROM docker.io/library/python:3.8.2-alpine@sha256:745fac134e7ea  0.0s
 => [internal] load build context                                          0.0s
 => => transferring context: 63B                                           0.0s
 => CACHED [2/7] WORKDIR /app                                              0.0s
 => CACHED [3/7] RUN apk add curl                                          0.0s
 => CACHED [4/7] RUN pip install --upgrade pip                             0.0s
 => CACHED [5/7] COPY ./requirements.txt ./                                0.0s
 => CACHED [6/7] RUN pip install -r requirements.txt                       0.0s
 => CACHED [7/7] COPY app.py ./                                            0.0s
 => exporting to image                                                     0.0s
 => => exporting layers                                                    0.0s
 => => writing image sha256:50c117d81a2cc68b121ec050edb7081533ed10a49f7b2  0.0s
 => => naming to docker.io/library/basic-docker-toolbox-app                0.0s
[+] Running 1/1
 - Container app  Created                                                  0.0s
Attaching to app
app  |  * Serving Flask app 'app' (lazy loading)
app  |  * Environment: production
app  |    WARNING: This is a development server. Do not use it in a production deployment.
app  |    Use a production WSGI server instead.
app  |  * Debug mode: off
app  | WARNING: This is a development server. Do not use it in a production deployment. Use a production WSGI server instead.
app  |  * Running on all addresses (0.0.0.0)
app  |  * Running on http://127.0.0.1:5000
app  |  * Running on http://172.18.0.2:5000
app  | Press CTRL+C to quit
app  | 127.0.0.1 - - [23/Apr/2023 22:02:51] "GET / HTTP/1.1" 200 -
app  | 127.0.0.1 - - [23/Apr/2023 22:03:01] "GET / HTTP/1.1" 200 -
app  | 127.0.0.1 - - [23/Apr/2023 22:03:11] "GET / HTTP/1.1" 200 -
```

NOTE: on some mashines `TERM=cygwin` nor `TERM=xtexm` nor `TERM=xterm-256color` does not fix the colors (console ASCII escape sequence doesn't clear window content and [is "displaying strange characters" instead](https://stackoverflow.com/questions/35387667/git-bash-is-displaying-strange-characters-on-windows-7)):

```sh
docker-compose up --build
```
```text
?[1A?[1B?[0G?[?25l[+] Building 0.0s (0/0)

?[?25h?[1A?[0G?[?25l[+] Building 0.0s (0/0)

?[?25h?[1A?[0G?[?25l[+] Building 0.0s (0/0)

?[?25h?[1A?[0G?[?25l[+] Building 0.0s (0/0)

?[?25h?[1A?[0G?[?25l[+] Building 0.0s (0/0)

?[?25h?[1A?[0G?[?25l[+] Building 0.0s (0/0)

?[?25h?[1A?[0G?[?25l[+] Building 0.0s (0/0)

?[?25h?[1A?[0G?[?25l[+] Building 0.0s (0/2)

 => [internal] load build definition from Dockerfile                       0.0s
 => => transferring dockerfile: 30B                                        0.0s
 => [internal] load .dockerignore                                          0.0s
?[36m => => transferring context: 2B
0.0s
?[0m?[?25h?[1A?[1A?[1A?[1A?[1A?[0G?[?25l[+] Building 0.2s (11/12)

?[36m => [internal] load build definition from Dockerfile
0.1s
?[0m?[36m => => transferring dockerfile: 32B
    0.0s
?[0m?[36m => [internal] load .dockerignore
    0.1s
?[0m?[36m => => transferring context: 2B
    0.0s
?[0m?[36m => [internal] load metadata for docker.io/library/python:3.8.2-alpine
    0.0s
?[0m?[36m => [1/7] FROM docker.io/library/python:3.8.2-alpine
    0.0s
...
```

the solution is to start __Docker Toolbox__ from the __Git Bash__:
```sh
/c/Program\ Files/Docker\ Toolbox/start.sh
```

```sh
docker-compose up --build
```
it will show the normal console output
```txt
#1 [internal] load build definition from Dockerfile
#1 transferring dockerfile: 281B 0.0s done
#1 DONE 0.0s

#2 [internal] load .dockerignore
#2 transferring context: 2B 0.0s done
#2 DONE 0.1s

#3 [internal] load metadata for docker.io/library/python:3.8.2-alpine
#3 DONE 0.0s

#4 [internal] load build context
#4 transferring context: 457B 0.0s done
#4 DONE 0.0s

#5 [1/7] FROM docker.io/library/python:3.8.2-alpine
#5 resolve docker.io/library/python:3.8.2-alpine done
#5 ...

#6 [2/7] WORKDIR /app
#6 DONE 0.0s

#7 [3/7] RUN apk add curl
#7 0.513 fetch http://dl-cdn.alpinelinux.org/alpine/v3.11/main/x86_64/APKINDEX.tar.gz
#7 1.222 fetch http://dl-cdn.alpinelinux.org/alpine/v3.11/community/x86_64/APKINDEX.tar.gz
#7 1.814 (1/3) Installing nghttp2-libs (1.40.0-r1)
#7 1.845 (2/3) Installing libcurl (7.79.1-r0)
#7 1.901 (3/3) Installing curl (7.79.1-r0)
#7 1.942 Executing busybox-1.31.1-r9.trigger
#7 1.963 OK: 12 MiB in 37 packages
#7 DONE 2.1s

#8 [4/7] RUN pip install --upgrade pip
#8 7.156 Collecting pip
#8 7.385   Downloading pip-23.1.2-py3-none-any.whl (2.1 MB)
#8 8.685 Installing collected packages: pip
#8 8.687   Attempting uninstall: pip
#8 8.691     Found existing installation: pip 20.1
#8 9.355     Uninstalling pip-20.1:
#8 10.22       Successfully uninstalled pip-20.1
#8 16.97 Successfully installed pip-23.1.2
#8 DONE 18.0s

#9 [5/7] COPY ./requirements.txt ./
#9 DONE 0.0s

#5 [1/7] FROM docker.io/library/python:3.8.2-alpine
#5 ...

#10 [6/7] RUN pip install -r requirements.txt
#10 2.915 Collecting Flask==2.0.3 (from -r requirements.txt (line 1))
#10 3.129   Downloading Flask-2.0.3-py3-none-any.whl (95 kB)
#10 3.223      ???????????????????????????????????????? 95.6/95.6 kB 1.3 MB/s eta 0:00:00
#10 3.475 Collecting Werkzeug>=2.0 (from Flask==2.0.3->-r requirements.txt (line 1))
#10 3.522   Downloading Werkzeug-2.3.3-py3-none-any.whl (242 kB)
#10 3.574      ??????????????????????????????????????? 242.3/242.3 kB 7.3 MB/s eta 0:00:00
#10 3.706 Collecting Jinja2>=3.0 (from Flask==2.0.3->-r requirements.txt (line 1))
#10 3.765   Downloading Jinja2-3.1.2-py3-none-any.whl (133 kB)
#10 3.800      ??????????????????????????????????????? 133.1/133.1 kB 8.1 MB/s eta 0:00:00
#10 3.891 Collecting itsdangerous>=2.0 (from Flask==2.0.3->-r requirements.txt (line 1))
#10 3.934   Downloading itsdangerous-2.1.2-py3-none-any.whl (15 kB)
#10 4.089 Collecting click>=7.1.2 (from Flask==2.0.3->-r requirements.txt (line 1))
#10 4.136   Downloading click-8.1.3-py3-none-any.whl (96 kB)
#10 4.162      ???????????????????????????????????????? 96.6/96.6 kB 7.8 MB/s eta 0:00:00
#10 4.492 Collecting MarkupSafe>=2.0 (from Jinja2>=3.0->Flask==2.0.3->-r requirements.txt (line 1))
#10 4.534   Downloading MarkupSafe-2.1.2-cp38-cp38-musllinux_1_1_x86_64.whl (29 kB)
#10 4.808 Installing collected packages: MarkupSafe, itsdangerous, click, Werkzeug, Jinja2, Flask
#10 6.603 Successfully installed Flask-2.0.3 Jinja2-3.1.2 MarkupSafe-2.1.2 Werkzeug-2.3.3 click-8.1.3 itsdangerous-2.1.2
#10 6.606 WARNING: Running pip as the 'root' user can result in broken permissions and conflicting behaviour with the system package manager. It is recommended to use a virtual environment instead: https://pip.pypa.io/warnings/venv
#10 DONE 7.0s

#5 [1/7] FROM docker.io/library/python:3.8.2-alpine
#5 ...

#11 [7/7] COPY app.py ./
#11 DONE 0.1s

#12 exporting to image
#12 exporting layers
#12 exporting layers 1.8s done
#12 writing image sha256:0410b0d21901344292779c0c9e06f518628867287e10a6e769ae4a8ab14fb4ed done
#12 naming to docker.io/library/basic-docker-toolbox-app done
#12 DONE 1.8s

#5 [1/7] FROM docker.io/library/python:3.8.2-alpine
Network basic-docker-toolbox_default  Creating
Network basic-docker-toolbox_default  Created
Container app  Creating
Container app  Created
Attaching to app
app  |  * Serving Flask app 'app' (lazy loading)
app  |  * Environment: production
app  |    WARNING: This is a development server. Do not use it in a production deployment.
app  |    Use a production WSGI server instead.
app  |  * Debug mode: off
app  | WARNING: This is a development server. Do not use it in a production deployment. Use a production WSGI server instead.
app  |  * Running on all addresses (0.0.0.0)
app  |  * Running on http://127.0.0.1:5000
app  |  * Running on http://172.20.0.2:5000
app  | Press CTRL+C to quit

```

in seperate console, run
```sh
docker-compose ps
```

it will show sucess
```text
NAME                COMMAND                SERVICE             STATUS              PORTS
app                 "python /app/app.py"   app                 running (healthy)   0.0.0.0:5000->5000/tcp
```

* verify
```sh
curl -s $(docker-machine ip):5000/hello/docker-toolbox
```
```text
Hello docker-toolbox!
```
and the docker console will show flask log
```sh
docker-compose logs app
```
```text
app  | 127.0.0.1 - - [23/Apr/2023 22:04:42] "GET / HTTP/1.1" 200 -
app    | 192.168.99.1 - - [20/Apr/2023 22:34:02] "GET /hello/docker-toolbox HTTP/1.1" 200 -
```

the connection to docker machine will takes place over __Virtual Box__ __Host-Only Ethernet Adapter__ with IP Address `192.168.99.1` and net mask `255.255.255.0`



NOTE: certain versions of Docker-Compose, e.g.  
```
Docker Compose version v2.17.3
```
will throw exception in runtime:

```sh
docker-compose up --build
```

```text
panic: interface conversion: *windowsconsole.ansiWriter is not console.File: missing method Close

goroutine 1 [running]:
github.com/docker/compose/v2/pkg/progress.NewWriter({0x22f8180, 0xc0002b40a0}, {
0x22dbca0?, 0xc000453020})
        github.com/docker/compose/v2/pkg/progress/writer.go:115 +0xc9 github.com/docker/compose/v2/pkg/progress.RunWithStatus({0x22f8180, 0xc0002b40a0 }, 0xc00060e460, {0x22dbca0, 0xc000453020})
	        github.com/docker/compose/v2/pkg/progress/writer.go:71 +0xa5 github.com/docker/compose/v2/pkg/progress.Run({0x22f8180, 0xc0002b40a0}, 0xc00042ff00, {0x22dbca0, 0xc000453020})
		        github.com/docker/compose/v2/pkg/progress/writer.go:62 +0x85
			github.com/docker/compose/v2/pkg/compose.(*composeService).Up(0xc000069160, {0x22f8180, 0xc0002b40a0}, 0xc0003b7760, {{{0xc00007ceb0, 0x0, 0x1}, 0x0, 0x0, {0x1fd57a8, ...}, ...}, ...})
			        github.com/docker/compose/v2/pkg/compose/up.go:34 +0x172
				github.com/docker/compose/v2/pkg/api.(*ServiceProxy).Up(0xc00038e1e0, {0x22f8180, 0xc0002b40a0}, 0x0?, {{{0xc00007ceb0, 0x0, 0x1}, 0x0, 0x0, {0x1fd57a8, ...}, .
				..}, ...})
				        github.com/docker/compose/v2/pkg/api/proxy.go:181 +0x14a
					github.com/docker/compose/v2/cmd/compose.runUp({0x22f8180?, 0xc0002b40a0}, {0x2bb13a98, 0xc0000f2500}, {0x230f310, _}, {0x1, 0x0, {0x1fd4070, 0x7}, ...}, ...) github.com/docker/compose/v2/cmd/compose/up.go:196 +0x89d github.com/docker/compose/v2/cmd/compose.upCommand.func2({0x22f8180, 0xc0002b40a0}, 0xc0003b7760, {0xc00007ceb0, 0x0, 0x1}) github.com/docker/compose/v2/cmd/compose/up.go:89 +0x1e5
							github.com/docker/compose/v2/cmd/compose.(*ProjectOptions).WithServices.func1({0x22f8180, 0xc0002b40a0}, {0xc00007ceb0, 0x0, 0x1}) github.com/docker/compose/v2/cmd/compose/compose.go:127 +0xd7 github.com/docker/compose/v2/cmd/compose.Adapt.func1({0x22f8180?, 0xc0002b40a0?}
								, 0x2?, {0xc00007ceb0?, 0x1?, 0x0?})
								        github.com/docker/compose/v2/cmd/compose/compose.go:92 +0x36 github.com/docker/compose/v2/cmd/compose.AdaptCmd.func1(0xc0001af800, {0xc00007ceb0, 0x0, 0x1})
									        github.com/docker/compose/v2/cmd/compose/compose.go:71 +0x21c
										github.com/spf13/cobra.(*Command).execute(0xc0001af800, {0xc000398a40, 0x1, 0x1}
) github.com/spf13/cobra@v1.7.0/command.go:940 +0x862
```

one will have to restore the original version of `docker-compose.exe`


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
