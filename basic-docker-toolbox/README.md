### Info

### Notes
if seeing
```text
```


try the [resolution from forums.docker.com](https://forums.docker.com/t/docker-on-windows-fails-with-pipe-docker-engine-the-system-cannot-find-the-file-specified/28479/4)

* run

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
set DOCKER_CERT_PATH=C:\Users\Serguei\.docker\machine\machines\default
set DOCKER_MACHINE_NAME=default
set COMPOSE_CONVERT_WINDOWS_PATHS=true
```
run in the clean command shell
```cmd
@FOR /f “tokens=*” %i IN (‘echo commands.txt’) DO @%i
```


followed by the command from the real `Docker Quickstart Terminal` desktop shortcut

```cmd
"C:\Program Files\Git\bin\bash.exe" --login -i "C:\Program Files\Docker Toolbox\start.sh"
```
Alternatively make these settings permanent via __Control Panel__. The __Docker Toolbox__ installer apparently does not do that.

#### Upgrade Docker compose

Note: From the end of June 2023 Compose V1, is about to not be supported, and the last archive build of Docker ToolBox comes with Docker-compose version `1.24.1`

To upgrade via curl, need to use Windows account with write permission to `Program Files` directories.

First in git bash that is launched by __Docker Toolbox__

```sh
cd /c/Program Files/Docker Toolbox
cd $TEMP
VERSION=2.17.3
curl -sL https://github.com/docker/compose/releases/download/v$VERSION/docker-compose-windows-x86_64.exe -o docker-compose.exe
```
verify
```sh
./docker-compose.exe  --version
```
```text
Docker Compose version v2.17.3
```
second, in elevated prompt
```cmd
copy /y %TEMP%\docker-compose.exe "c:\Program Files\Docker Toolbox"
```
NOTE the directory path

unfortunately it will lead to a problem in runtime:

```sh
docker-compose up --build
```

```text
panic: interface conversion: *windowsconsole.ansiWriter is not console.File: mis
sing method Close

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


*verify
```sh

curl $(docker-machine ip):5000/hello/docker-toolbox
```
```text
Hello docker-toolbox!
```
and the docker console will show

```text
app    | 192.168.99.1 - - [20/Apr/2023 22:34:02] "GET /hello/docker-toolbox HTTP
/1.1" 200 -
```

the connection to docker machine will take place over __Virtual Box__ __Host-Only Ethernet Adapter__ with IP Address `192.168.99.1` and net mask `255.255.255.0`
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
