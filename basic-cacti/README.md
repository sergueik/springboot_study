### Info 

Replica of the [chestersgarage/cactii](https://github.com/ChestersGarage/cacti-aio)
- all in one alpine based Cacti (https://www.cacti.net) legacy network monitoring and graphing appliance
 with some compoent versions updated to prevent errors like
```text
```
### Note

The software versons appear to be off-sync with base image (`alpine:3.12`) and need to be adjusted

### Usage
*  download the original container image
```
IMAGE=chestersgarage/cacti:latest
docker pull $IMAGE
```
* run as suggested in [README.md](https://github.com/ChestersGarage/cacti-aio/blob/master/README.md)

```sh
mkdir -p logs/apache2 mysql mysql-conf cacti-data
docker run -d --net='bridge' -p 1984:80/tcp  -v $(pwd)/mysql:/var/lib/mysql:rw -v $(pwd)/mysql-conf:/etc/mysql:rw -v $(pwd)/cacti-data:/var/lib/cacti/rra:rw -v $(pwd)/logs:/var/log:rw -e TZ='America/New_York' -e MYSQL='password' --name Cacti $IMAGE
```

if the build was not successful will need to clean up:
```sh
sudo rm -fr logs mysql mysql-conf cacti-data
```

* run container. Needs a lot of configuration fed the container with -  see he original [README.md](https://github.com/ChestersGarage/cacti-aio/blob/master/README.md)

* build the image
```sh
IMAGE=basic-cacti
docker build -f Dockerfile -t $IMAGE .
```
* run as suggested in [README.md](https://github.com/ChestersGarage/cacti-aio/blob/master/README.md)
```sh
NAME=cacti
mkdir -p logs/apache2 mysql mysql-conf cacti-data
docker run -d --net='bridge' -p 1984:80/tcp  -v $(pwd)/mysql:/var/lib/mysql:rw -v $(pwd)/mysql-conf:/etc/mysql:rw -v $(pwd)/cacti-data:/var/lib/cacti/rra:rw -v $(pwd)/logs:/var/log:rw -e TZ='America/New_York' -e MYSQL='password' --name $NAME $IMAGE
```

* inspect processes
```sh
docker exec $NAME ps
```
```text
PID   USER     TIME  COMMAND
    1 root      0:00 {init} /bin/bash -x /init
  704 root      0:00 {mysqld_safe} /bin/sh /usr/bin/mysqld_safe --datadir=/var/lib/mysql
  861 mysql     0:01 /usr/bin/mysqld --basedir=/usr --datadir=/var/lib/mysql --plugin-dir=/usr/lib/mariadb/plugin --user=mysql --log-error=/var/lib/mysql/5517348ac445.err --pid-file=5517348ac445.pid
  940 root      0:00 crond -L /var/log/cron
  942 root      0:00 /usr/sbin/httpd -k start -f /etc/apache2/httpd.conf
  943 root      0:00 tail -F /var/log/apache2/error.log /var/log/apache2/access.log /var/log/cron /var/log/cacti/cacti.log /var/log/cacti/cacti_stderr.log /var/lib/mysql/5517348ac445.err
  944 apache    0:00 /usr/sbin/httpd -k start -f /etc/apache2/httpd.conf
  945 apache    0:00 /usr/sbin/httpd -k start -f /etc/apache2/httpd.conf
  946 apache    0:00 /usr/sbin/httpd -k start -f /etc/apache2/httpd.conf
  947 apache    0:00 /usr/sbin/httpd -k start -f /etc/apache2/httpd.conf
  948 apache    0:00 /usr/sbin/httpd -k start -f /etc/apache2/httpd.conf
  949 apache    0:00 php /opt/cacti/poller.php
  954 apache    0:00 [sh]
  956 root      0:00 ps
```
*  connect via browser to  `http://localhost:1984/cacti/`
change the  password to e.g. `P@ss1234` to meet the requirements
### Cleanup
```sh
docker container rm -f $NAME
docker image prune -f 
docker image rm -f $IMAGE
```
### See Also

  * [Система мониторинга Cacti documentation](https://www.tux.in.ua/articles/4) (in Russian)
  * [Cacti setup on Ubuntu 20.04](https://infoit.com.ua/linux/ustanovka-i-nastrojka-cacti-v-ubuntu-20-04-18-04/) (in Russian)

  * [Cacti basic instal](http://system-administrators.info/?p=2619) (in Russian)
  * [Cacti plugins](http://system-administrators.info/?p=2662) (in Russian)
  * [Cacti plugins development](http://system-administrators.info/?p=2666)(in Russian)
  * https://en.wikipedia.org/wiki/Simple_Network_Management_Protocol
  * https://en.wikipedia.org/wiki/Cacti_(software)
  * Youtube videos
    + [Cacti - The RRDTool-based graphing solution](https://www.youtube.com/watch?v=HFm0Lb-A5DI)

    + [Cacti RRDtool installation on Ubuntu 20.04 LTS - no voice](https://www.youtube.com/watch?v=gaYH4rEjQYs)
    + [Introduction to Cacti | The primary features of Cacti](https://www.youtube.com/watch?v=Xww5y9V1ikI) - no audio (music)

    + [install & Configure MRTG (Multi Router Traffic Grapher) on Windows 10]( 
https://www.youtube.com/watch?v=0MhNu0WxOy0)

    +  [Installation, Configuration & Demonstration of MRTG on Windows using GNS3](https://www.youtube.com/watch?v=XKoUp5zY8pU)

    + [Simple Network Management Protocol Intro (from CCNI)](https://www.youtube.com/watch?v=Lq7j-QipNrI)

    + [Cacti SNMP Graphing Infroduction Basics](https://www.youtube.com/watch?v=aDF3ylH7S90)


    + [Introduction to SNMP - Simple Network Management Protocol - domain specific](https://www.youtube.com/watch?v=ZX-XGQoISHQ)
    + [how snmp works (nagios) a quick guide](https://www.youtube.com/watch?v=2IXP0TkwNJU)
    + [Walk through of Cacti Automation - a lot of typical tasks, very cryptic](https://www.youtube.com/watch?v=IwT2VI4e_4I)
    + [Cacti - Creating Automation Tree Rules](https://www.youtube.com/watch?v=yxO-CgaeFNc)
    + [Cacti device defaults walk-through](https://www.youtube.com/watch?v=ZoNQdL-MkT4)
    + [Cacti Syslog Plugin Installation & Configuration](https://www.youtube.com/watch?v=Ut2b9Jq0Vls)
    + [Cacti - Adjusting Poller Processes](https://www.youtube.com/watch?v=PiRMdb4Q8uI)
    + [installing the Cactid Systemd Daemon](https://www.youtube.com/watch?v=Ggpwvd2GV1E)
    + [Installing Cacti On Ubuntu/Debian](https://www.youtube.com/watch?v=-ihZe5cA4Ps)
    + [Javascript Flot renderer of RRDTool datafile](https://www.youtube.com/watch?v=yY9rbOHxwyg) and __JavascriptRRD__ [project page](https://sourceforge.net/projects/javascriptrrd/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
