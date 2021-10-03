### Info

*  basic Spring Framework TaskExecutor and TaskScheduler code extracted from tutorial [project](https://github.com/eugenp/tutorials/tree/master/spring-scheduling) with the `Thread PoolTaskScheduler` bean to run the task on fixed size thread  pool instance of `Runnable`.

### Usage
* test locally
```sh
mvn spring-boot:run
```
* will log to console two tasks, round-robin:
```text
2021-10-03 14:00:00  [ThreadPoolTaskScheduler2] - INFO  Cron task performed at 10/03/2021 14:00:00 on thread ThreadPoolTaskScheduler2
2021-10-03 14:01:58  [ThreadPoolTaskScheduler4] - INFO  Fixed rate task performed at 10/03/2021 14:01:58 on thread ThreadPoolTaskScheduler4
2021-10-03 14:02:00  [ThreadPoolTaskScheduler1] - INFO  Cron task performed at 10/03/2021 14:02:00 on thread ThreadPoolTaskScheduler1
2021-10-03 14:03:58  [ThreadPoolTaskScheduler5] - INFO  Fixed rate task performed at 10/03/2021 14:03:58 on thread ThreadPoolTaskScheduler5
2021-10-03 14:04:00  [ThreadPoolTaskScheduler3] - INFO  Cron task performed at 10/03/2021 14:04:00 on thread ThreadPoolTaskScheduler3
2021-10-03 14:05:58  [ThreadPoolTaskScheduler2] - INFO  Fixed rate task performed at 10/03/2021 14:05:58 on thread ThreadPoolTaskScheduler2
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
