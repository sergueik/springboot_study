### Info
this directory contains an `jq` example extracted from  __Controlling Service Startup Order in Docker Compose__ [codeptoject article](https://www.codeproject.com/Articles/1260230/Controlling-Service-Startup-Order-in-Docker-Compos)
### Usage

#### Unhealthy

use the configuration files in the `unhealthy` directory

Run with Alpine MySQL image  using TCP port check as in `Dockerfile` and then performing `docker` API  check to ensure the dependent service state is `running` and status is `healthy`. Due to some misconfiguration the __Alpine__ __MySQL__ never  switches to listening to TCP socket and thus never becomes healthyr:

```sh
docker-compose up --build
```
```text
basic-docker-compose-dependon-custom-app-1           | Start checking whether MySQL is up & running
basic-docker-compose-dependon-custom-app-1           | (able to process incoming connections) each 30 for a total amount of 20 times
basic-docker-compose-dependon-custom-app-1           | "Up 32 seconds (health: starting)"
basic-docker-compose-dependon-custom-app-1           | WARN: [1/20] MySQL database still NOT up & running ...
basic-docker-compose-dependon-custom-app-1           | "Up About a minute (health: starting)"
basic-docker-compose-dependon-custom-app-1           | WARN: [2/20] MySQL database still NOT up & running ...
basic-docker-compose-dependon-custom-app-1           | "Up About a minute (health: starting)"
basic-docker-compose-dependon-custom-app-1           | WARN: [3/20] MySQL database still NOT up & running ...
basic-docker-compose-dependon-custom-app-1           | "Up 2 minutes (unhealthy)"
basic-docker-compose-dependon-custom-app-1           | WARN: [4/20] MySQL database still NOT up & running ...
...
basic-docker-compose-dependon-custom-app-1           | WARN: [19/20] MySQL database still NOT up & running ...
basic-docker-compose-dependon-custom-app-1           | "Up 10 minutes (unhealthy)"
basic-docker-compose-dependon-custom-app-1           | WARN: [20/20] MySQL database still NOT up & running ...
basic-docker-compose-dependon-custom-app-1           | ERROR: Could not connect to MySQL in due time.
basic-docker-compose-dependon-custom-app-1 exited with code 1

```

#### Healthy
the `mysql-alpine` container lacks the user credentals initializqation and needs ot be tested via mysql client connecting via socket without credentials.

To see the `docker` API opetarion in successful case, change to `healthy` directory 

```sh
docker-compose up --build
```
```text
Attaching to healthy-app-1, healthy-mysql-alpine-1
healthy-mysql-alpine-1  | [i] MySQL data directory not found, creating initial DBs
healthy-app-1           | Start checking whether MySQL is up & running
healthy-app-1           | (able to process incoming connections) each 3 for a total amount of 10 times
healthy-mysql-alpine-1  | 2023-05-08 22:35:59 6 [Warning] Failed to load slave replication state from table mysql.gtid_slave_pos: 1017: Can't find file: './mysql/' (errno: 2 "No such file or directory")
healthy-app-1           | "Up 4 seconds (health: starting)"
healthy-app-1           | WARN: [1/10] MySQL database still NOT up & running ...
healthy-mysql-alpine-1  | [i] Creating database: test
healthy-mysql-alpine-1  | [i] Creating user: java with password password
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] /usr/bin/mysqld (mysqld 10.3.25-MariaDB-log) starting as process 69 ...
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Using Linux native AIO
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Mutexes and rw_locks use GCC atomic builtins
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Uses event mutexes
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Compressed tables use zlib 1.2.11
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Number of pools: 1
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Using SSE2 crc32 instructions
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Initializing buffer pool, total size = 128M, instances = 1, chunk size = 128M
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Completed initialization of buffer pool
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: If the mysqld execution user is authorized, page cleaner thread priority can be changed. See the man page of setpriority().
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: 128 out of 128 rollback segments are active.
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Creating shared tablespace for temporary tables
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Setting file './ibtmp1' size to 12 MB. Physically writing the file full; Please wait ...
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: File './ibtmp1' size is now 12 MB.
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: 10.3.25 started; log sequence number 1625452; transaction id 21
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Loading buffer pool(s) from /app/mysql/ib_buffer_pool
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] InnoDB: Buffer pool(s) load completed at 230508 22:36:04
healthy-mysql-alpine-1  | 2023-05-08 22:36:04 0 [Note] Plugin 'FEEDBACK' is disabled.
healthy-app-1           | "Up 7 seconds (health: starting)"
healthy-app-1           | WARN: [2/10] MySQL database still NOT up & running ...
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] /usr/bin/mysqld (mysqld 10.3.25-MariaDB-log) starting as process 1 ...
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Using Linux native AIO
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Mutexes and rw_locks use GCC atomic builtins
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Uses event mutexes
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Compressed tables use zlib 1.2.11
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Number of pools: 1
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Using SSE2 crc32 instructions
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Initializing buffer pool, total size = 128M, instances = 1, chunk size = 128M
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Completed initialization of buffer pool
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: If the mysqld execution user is authorized, page cleaner thread priority can be changed. See the man page of setpriority().
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: 128 out of 128 rollback segments are active.
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Creating shared tablespace for temporary tables
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Setting file './ibtmp1' size to 12 MB. Physically writing the file full; Please wait ...
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: File './ibtmp1' size is now 12 MB.
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: 10.3.25 started; log sequence number 1625461; transaction id 21
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Loading buffer pool(s) from /app/mysql/ib_buffer_pool
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] InnoDB: Buffer pool(s) load completed at 230508 22:36:06
healthy-mysql-alpine-1  | 2023-05-08 22:36:06 0 [Note] Plugin 'FEEDBACK' is disabled.
healthy-mysql-alpine-1  | 2023-05-08 22:36:07 0 [Note] Reading of all Master_info entries succeeded
healthy-mysql-alpine-1  | 2023-05-08 22:36:07 0 [Note] Added new Master_info '' to hash table
healthy-mysql-alpine-1  | 2023-05-08 22:36:07 0 [Note] /usr/bin/mysqld: ready for connections.
healthy-mysql-alpine-1  | Version: '10.3.25-MariaDB-log'  socket: '/tmp/mysqld.sock'  port: 0  MariaDB Server
healthy-app-1           | "Up 10 seconds (healthy)"
healthy-app-1           | OK: [3/10] MySQL is up & running.
healthy-app-1 exited with code 0
```

### TODO

the `mysql:8.0.18` image used in the [original configuraton](https://github.com/satrapu/jdbc-with-docker/blob/master/docker-compose-using-api.yml)
does not have health check passed presumably to some missing configuration

### See Also
  * https://github.com/satrapu/jdbc-with-docker/blob/master/docker-compose-using-api.yml
  * https://github.com/byrnedo/docker-alpine-curl
  * https://superuser.com/questions/834307/can-curl-send-requests-to-sockets

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
