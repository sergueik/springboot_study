### Info

*  basic Spring Framework TaskExecutor and TaskScheduler code extracted from tutorial [project](https://github.com/eugenp/tutorials/tree/master/spring-scheduling) with the `Thread PoolTaskScheduler` bean to run the task on fixed size thread  pool instance of `Runnable`.

### Usage
* test locally
```sh
mvn spring-boot:run
```

* launch healt check monitoring
```sh
while true; do curl -v http://localhost:8085/task-scheduler/healthcheck  ; sleep  60; done
```
* will log to console two tasks, round-robin, and health check messages
```text
2021-11-07 18:30:53  [http-nio-8085-exec-8] - INFO  respond to healthcheck
2021-11-07 18:31:02  [ThreadPoolTaskScheduler1] - INFO  Fixed rate task performed at 11/07/2021 18:31:02 on thread ThreadPoolTaskScheduler1
2021-11-07 18:31:53  [http-nio-8085-exec-10] - INFO  respond to healthcheck
2021-11-07 18:32:00  [ThreadPoolTaskScheduler3] - INFO  Cron task performed at 11/07/2021 18:32:00 on thread ThreadPoolTaskScheduler3
```
* create volume to share data across Docker containers

```sh
DATA=data-volume
docker volume create --name $DATA
```
* build the monitoring app
```sh
mvn clean package
```
* build image with monitoring app
```sh
RPM_IMAGE=rpm-agent
docker build -t $RPM_IMAGE -f Dockerfile.$RPM_IMAGE .
docker run -it --name $RPM_IMAGE --rm -v $DATA:/data $RPM_IMAGE
```
* run app and rpm containers
```sh
APP_IMAGE=basic
APP_DATA='app-data'
docker build -t $APP_IMAGE -f Dockerfile.$APP_IMAGE .
docker run -it --name $APP_IMAGE --rm -v $APP_DATA:/data $APP_IMAGE

RPM_IMAGE=rpm-agent
docker build -t $RPM_IMAGE -f Dockerfile.$RPM_IMAGE .
docker run -it --name=$RPM_IMAGE --volumes-from $APP_IMAGE $RPM_IMAGE
```

### See Also

  * [guide](https://www.baeldung.com/spring-task-scheduler) to the Spring Task Scheduler
  * another [demo project](https://github.com/JavaInUse/springboot-taskscheduler) 
  * Russian [translation](https://ru.wikibooks.org/wiki/RRDtool) of the originalRRDTool [WIKI](https://oss.oetiker.ch/rrdtool/doc/index.en.html)
  * [rrd based metric monitoring and predictions](https://habr.com/ru/post/134599/) (in Russian0
  * [practical example of rrd feed](https://www.codeproject.com/Articles/867463/Monitoring-Lync-with-MRTG)
  * [practical example of rrd feed (part 2?)](https://www.codeproject.com/Articles/1016359/Monitoring-Lync-with-MRTG-2)
  * [.net native rrd produced](https://www.codeproject.com/Articles/28763/C-Hooks-For-RRDtool)
