## Info

Supervisor wrapper of Python 'server' script example

### USage
* build and launch
```sh
IMAGE_NAME='basic-supervisor'
docker build -t $IMAGE_NAME -f Dockerfile .
```
```sh
CONTAINER_NAME='example_supervisor'
docker rm $CONTAINER_NAME
docker run -it --name $IMAGE_NAME $CONTAINER_NAME
```
this will log to console
```
2020-12-19 18:00:54,578 CRIT Supervisor is running as root.  Privileges were not dropped because no user is specified in the config file.  If you intend to run as root, you can set user=root in the config file to avoid this message.
2020-12-19 18:00:54,578 INFO Included extra file "/etc/supervisor.d/agent.ini" during parsing
2020-12-19 18:00:54,588 INFO RPC interface 'supervisor' initialized
2020-12-19 18:00:54,588 CRIT Server 'unix_http_server' running without any HTTP authentication checking
2020-12-19 18:00:54,589 INFO supervisord started with pid 6
2020-12-19 18:00:55,594 INFO spawned: 'server' with pid 8
2020-12-19 18:00:56,597 INFO success: server entered RUNNING state, process has stayed up for > than 1 seconds (startsecs)
```
 * confirm
```sh
docker exec -it $CONTAINER_NAME supervisorctl status server
```
this will respond with
```sh
server  RUNNING   pid 8, uptime 0:00:14
```
 * check
```sh
docker exec -it example_container s
```

in the container run
```
ls /var/log/
server.err       server.out       supervisord.log
```
```sh
 ps ax |  grep pytho[n]
6 root      0:00 {supervisord} /usr/bin/python3 /usr/bin/supervisord -c /etc/supervisord.conf
8 root      0:00 python server.py
```


### See Also

 * https://csjourney.com/managing-processes-with-supervisor-in-depth-tutorial/
	
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
