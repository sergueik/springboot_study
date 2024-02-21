### Info 

replica of [harness-cd-community](https://github.com/harness/harness-cd-community)

### Usage

```sh
pushd docker-compose/harnes
docker-compose up --build
```
NOTE: expect two digit load average on  a 16 GB 4 core laptop and expect networking changes while Harness cluster is running (inbound connections may be blocked)

```sh
ping 192.168.0.25
```
```text
PING 192.168.0.25 (192.168.0.25) 56(84) bytes of data.
From 192.168.0.1 icmp_seq=1 Destination Host Unreachable

```
presumably because the subnet `192.168.0.0/24` is used by `docker-compose.yml`:
```yml
networks:
  harness-network:
    ipam:
      config:
        - subnet: 192.168.0.0/24

```
      
```sh
docker network list
```
```text
NETWORK ID     NAME                             DRIVER    SCOPE
3b7541afd02d   basic-fullcycle-java17_default   bridge    local
1d6d95b8dccf   basic-perl-cgi_app               bridge    local
7af7a322f065   bridge                           bridge    local
1bb8675c1585   harness_harness-network          bridge    local
83ddadb0b1e0   host                             host      local
7369691904d9   none                             null      local
```
```sh
docker network prune -f
```
```text
Deleted Networks:
basic-fullcycle-java17_default
harness_harness-network
basic-perl-cgi_app
```

```sh
docker-compose ps
```
```text
docker-compose ps 
NAME                         IMAGE                                       COMMAND                  SERVICE             CREATED             STATUS                      PORTS
harness-delegate-proxy-1     harness/delegate-proxy-signed:79310         "/bin/sh -c 'nginx -…"   delegate-proxy      47 minutes ago      Up 39 minutes (healthy)     8080/tcp
harness-log-service-1        harness/log-service-signed:release-70-ubi   "/usr/local/bin/log-…"   log-service         47 minutes ago      Up 39 minutes (healthy)     8079/tcp
harness-manager-1            harness/manager-signed:79421                "./run.sh"               manager             47 minutes ago      Up 39 minutes (healthy)     9090/tcp, 9879/tcp
harness-mongo-1              mongo:4.4.22                                "docker-entrypoint.s…"   mongo               47 minutes ago      Up 40 minutes (healthy)     27017/tcp
harness-ng-auth-ui-1         harness/ng-auth-ui-signed:1.7.0             "/bin/sh -c 'sed -i …"   ng-auth-ui          47 minutes ago      Up 39 minutes (healthy)     8080/tcp
harness-ng-manager-1         harness/ng-manager-signed:79421             "./run.sh"               ng-manager          47 minutes ago      Up 39 minutes (healthy)     7090/tcp
harness-ng-ui-1              harness/nextgenui-signed:0.353.10           "sh /opt/entrypoint.…"   ng-ui               47 minutes ago      Up 39 minutes (healthy)     8080/tcp
harness-pipeline-service-1   harness/pipeline-service-signed:1.33.8      "/opt/harness/run.sh"    pipeline-service    47 minutes ago      Up 40 minutes (healthy)     12001/tcp, 12011/tcp, 14002/tcp
harness-platform-service-1   harness/platform-service-signed:79202       "/opt/harness/run.sh"    platform-service    47 minutes ago      Exited (1) 38 minutes ago   
harness-proxy-1              harness/nginx:1.21.4                        "/docker-entrypoint.…"   proxy               40 minutes ago      Up 39 minutes (healthy)     0.0.0.0:80->80/tcp, :::80->80/tcp, 0.0.0.0:9879->9879/tcp, :::9879->9879/tcp
harness-redis-1              harness/redis:6.2.7-alpine                  "docker-entrypoint.s…"   redis               47 minutes ago      Up 39 minutes (healthy)     6379/tcp
```

![Harness Signup](https://github.com/sergueik/springboot_study/blob/master/basic-elk-cluster/screenshots/capture-harness-signup.png)

![Harness Login](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-harness-login.png)

### Cleanup
```sh
docker-compose stop
docker network prune -f
```
### See Also

  * [deploy to physical data center](https://developer.harness.io/docs/continuous-delivery/get-started/cd-tutorials/pdc) using a `harness/delegate` Docker instance
  * [deploy to Azure VM](https://developer.harness.io/docs/continuous-delivery/get-started/cd-tutorials/azure)
  * [Harness Community Edition](https://developer.harness.io/docs/continuous-delivery/deploy-srv-diff-platforms/community-ed/harness-community-edition-quickstart) [repo](https://github.com/harness/harness-cd-community/tree/main) - retired in favour of [Gitness](https://gitness.com/)
  * [Harness Community Edition deployments](https://developer.harness.io/docs/continuous-delivery/deploy-srv-diff-platforms/community-ed/harness-community-edition-quickstart)
  * [Harness Delegate overview](https://developer.harness.io/docs/platform/delegates/delegate-concepts/delegate-overview) - NOTE, Harness recommends that you create a custom image.
  * running  in Docker compose [repo](https://github.com/harness/harness-cd-community) and [docker-compose.yml](https://github.com/harness/harness-cd-community/blob/main/docker-compose/harness/docker-compose.yml)  using `harness/nextgenui-signed` ,`harness/ng-auth-ui-signed` ,`harness/ng-manager-signed` ,`harness/pipeline-service-signed` ,`harness/platform-service-signed` , `harness/log-service-signed:release-70-ubi`, `harness/ci-scm-signed`, `harness/delegate-proxy-signed`, `harness/nginx`,`harness/redis`, and `mongo`
  * [Harness Shell Script step example](https://developer.harness.io/docs/continuous-delivery/x-platform-cd-features/cd-steps/utilities/shell-script-step)
  * https://hostadvice.com/how-to/web-hosting/ubuntu/how-to-configure-firewall-with-ufw-on-ubuntu-18/

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
