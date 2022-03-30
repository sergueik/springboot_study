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
NOTE: it takes significant time (5 minute) to build the image

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
in the first launch it will redirect to `http://127.0.0.1:1984/cacti/install/install.php`
Login as `admin`/`admin` and wheh prompted change the  password to e.g. `P@ss1234` to meet the requirements
Accept GPL License Agreement, click __Begin__ - this will take you to __Cacti Install__

### Cacti Install
On pre-install configuration screen do not bother about `innodb_doublewrite`

Keep the `Local_Linux_Machine.xml.gz` selected, unselect the rest
Confirm installation
NOTE - the application is unable to survive `stop` / `start` of a container:

```text
FATAL: Connection to Cacti database failed. Please ensure:

the PHP MySQL module is installed and enabled.
the database is running.
the credentials in config.php are valid.
```
```sh
docker exec $NAME ls /opt/cacti/rra
```
```text
example_load_1min_2.rrd
example_proc_1.rrd
```

NOTE: there will likely be an SNMP error, to generatet *some data* need to use __Load Average__ or __Processes__
Also disable the __Downed Device Detection__ in `http://127.0.0.1:1984/cacti/settings.php` and `http://127.0.0.1:1984/cacti/host.php?action=edit&id=1`
and set __SNMP Version__ to `Not in Use` in `http://127.0.0.1:1984/cacti/host.php?action=edit&id=1`

![Processes Graph](https://github.com/sergueik/springboot_study/blob/master/basic-cacti/screenshots/capture_processes_graph.png)


```ssh
docker exec $NAME vi /opt/cacti/scripts/unix_processes.pl
```
- NOTE : not interactively

```text
#!/usr/bin/env perl

delete @ENV{qw(PATH)};
$ENV{PATH} = '/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin';

open(PROCESS, "ps ax | grep -c : |");
$output = <PROCESS>;
close(PROCESS);
chomp($output);
print $output;
```
```sh
docker exec $NAME perl perl /opt/cacti/scripts/unix_processes.pl
```
```text
24
```
![Processes Table](https://github.com/sergueik/springboot_study/blob/master/basic-cacti/screenshots/capture_processes_table.png)

data processing take place via feeding it to `rrdtool`
      
```sh
ps -o pid,ppid,comm,args |grep 2815
```
```text
2815   949 php              php /opt/cacti/poller.php
2822  2815 rrdtool          /usr/bin/rrdtool -
```
and inserting to MySQL Database (TBD what tables, but `processes` is empty)

```sh
docker exec -it $NAME mysql
```
```text
Welcome to the MariaDB monitor.  Commands end with ; or \g.
Your MariaDB connection id is 600
Server version: 10.4.24-MariaDB MariaDB Server

Copyright (c) 2000, 2018, Oracle, MariaDB Corporation Ab and others.

Type 'help;' or '\h' for help. Type '\c' to clear the current input statement.
```
```text
MariaDB [(none)]> use cacti;
Reading table information for completion of table and column names
You can turn off this feature to get a quicker startup with -A

Database changed
MariaDB [cacti]> show tables;
```
```text
+-------------------------------------+
| Tables_in_cacti                     |
+-------------------------------------+
| aggregate_graph_templates           |
| aggregate_graph_templates_graph     |
| aggregate_graph_templates_item      |
| aggregate_graphs                    |
| aggregate_graphs_graph_item         |
| aggregate_graphs_items              |
| automation_devices                  |
| automation_graph_rule_items         |
| automation_graph_rules              |
| automation_ips                      |
| automation_match_rule_items         |
| automation_networks                 |
| automation_processes                |
| automation_snmp                     |
| automation_snmp_items               |
| automation_templates                |
| automation_tree_rule_items          |
| automation_tree_rules               |
| cdef                                |
| cdef_items                          |
| color_template_items                |
| color_templates                     |
| colors                              |
| data_debug                          |
| data_input                          |
| data_input_data                     |
| data_input_fields                   |
| data_local                          |
| data_source_profiles                |
| data_source_profiles_cf             |
| data_source_profiles_rra            |
| data_source_purge_action            |
| data_source_purge_temp              |
| data_source_stats_daily             |
| data_source_stats_hourly            |
| data_source_stats_hourly_cache      |
| data_source_stats_hourly_last       |
| data_source_stats_monthly           |
| data_source_stats_weekly            |
| data_source_stats_yearly            |
| data_template                       |
| data_template_data                  |
| data_template_rrd                   |
| external_links                      |
| graph_local                         |
| graph_template_input                |
| graph_template_input_defs           |
| graph_templates                     |
| graph_templates_gprint              |
| graph_templates_graph               |
| graph_templates_item                |
| graph_tree                          |
| graph_tree_items                    |
| host                                |
| host_graph                          |
| host_snmp_cache                     |
| host_snmp_query                     |
| host_template                       |
| host_template_graph                 |
| host_template_snmp_query            |
| plugin_config                       |
| plugin_db_changes                   |
| plugin_hooks                        |
| plugin_realms                       |
| poller                              |
| poller_command                      |
| poller_data_template_field_mappings |
| poller_item                         |
| poller_output                       |
| poller_output_boost                 |
| poller_output_boost_arch_1648591980 |
| poller_output_boost_processes       |
| poller_output_realtime              |
| poller_reindex                      |
| poller_resource_cache               |
| poller_time                         |
| processes                           |
| reports                             |
| reports_items                       |
| sessions                            |
| settings                            |
| settings_tree                       |
| settings_user                       |
| settings_user_group                 |
| sites                               |
| snmp_query                          |
| snmp_query_graph                    |
| snmp_query_graph_rrd                |
| snmp_query_graph_rrd_sv             |
| snmp_query_graph_sv                 |
| snmpagent_cache                     |
| snmpagent_cache_notifications       |
| snmpagent_cache_textual_conventions |
| snmpagent_managers                  |
| snmpagent_managers_notifications    |
| snmpagent_mibs                      |
| snmpagent_notifications_log         |
| user_auth                           |
| user_auth_cache                     |
| user_auth_group                     |
| user_auth_group_members             |
| user_auth_group_perms               |
| user_auth_group_realm               |
| user_auth_perms                     |
| user_auth_realm                     |
| user_domains                        |
| user_domains_ldap                   |
| user_log                            |
| vdef                                |
| vdef_items                          |
| version                             |
+-------------------------------------+
111 rows in set (0.003 sec)
```
```text
MariaDB [cacti]> select  * from processes;
Empty set (0.000 sec)
```
```sh
docker exec -it $NAME vi /opt/cacti/lib/poller.php
```
```text
process_poller_output - grabs data from the 'poller_output' table and feeds t
     results to RRDtool for processing                                          
  @arg $rrdtool_pipe - the array of pipes containing the file descriptor for rrdtool
  @arg $remainder - don't use LIMIT if true */                                  
  
function process_poller_output(&$rrdtool_pipe, $remainder = false) {            
        global $config, $debug; 
```
and
```text
function poller_push_table($db_cnn, $records, $table, $ignore = false, $dupes = 
        $prefix = 'INSERT ' . ($ignore ? 'IGNORE':'') . ' INTO ' . $table . ' ';
```
![Processes Table](https://github.com/sergueik/springboot_study/blob/master/basic-cacti/screenshots/capture_processes_configuration.png)

```sh
docker cp $NAME:/opt/cacti/rra/example_proc_1.rrd .
```
### Cleanup
```sh
docker container rm -f $NAME
docker image prune -f 
docker image rm -f $IMAGE
```
### See Also

  * Cacti documentation [github repo](https://github.com/Cacti/documentation)
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
