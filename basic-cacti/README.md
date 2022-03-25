### Info 

Replica of the [chestersgarage/cactii](https://github.com/ChestersGarage/cacti-aio) all in one alpine based Cacti (https://www.cacti.net) legacy network monitoring and graphing appliance

### Note

The software versons appear to be off-sync with base image (`alpine:3.12`) and need to be adjusted

### Usage
*  download the original container image
```
IMAGE=chestersgarage/cacti:latest
docker pull $IMAGE
```
* run container. Needs a lot of configuration fed the container with -  see he original [README.md](https://github.com/ChestersGarage/cacti-aio/blob/master/README.md)

* build the image
```sh
`IMAGE=basic-cacti
docker build -f Dockerfile -t $IMAGE .
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
