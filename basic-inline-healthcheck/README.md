### Info

### Usage
* pull
```sh
docker pull mysql:5.7-debian
```
```
Status: Downloaded newer image for mysql:5.7-debian
docker.io/library/mysql:5.7-debian
```
mysql:5.7
```sh
docker-compose up --build
```

```text
[+] Running 3/3
 ⠿ Network basic-inline-healthcheck_example  Created                       0.8s
 => [internal] load metadata for docker.io/library/mysql:5.7-debian        0.0s
 => [1/2] FROM docker.io/library/mysql:5.7-debian                          6.1s
 => => resolve docker.io/library/mysql:5.7-debian                          0.0s
 => [2/2] RUN apt-get update && apt-get install -q -y netcat              94.5s
 => => # 1.gz because associated file /usr/share/man/man1/nc.traditional.1.gz (
 => => # of link group nc) doesn't exist
 => => # update-alternatives: warning: skip creation of /usr/share/man/man1/net
 => => # cat.1.gz because associated file /usr/share/man/man1/nc.traditional.1.
 => => # gz (of link group nc) doesn't exist
 => => # Setting up netcat (1.10-41.1) ...

 ⠿ Container mysql-server                    C...                          1.6s
 ⠿ Container app                             Created                       0.2s
Attaching to app, mysql-server
mysql-server  | 2023-06-10 21:01:54+00:00 [Note] [Entrypoint]: Entrypoint script for MySQL Server 5.7.37-1debian10 started.
mysql-server  | 2023-06-10 21:01:55+00:00 [Note] [Entrypoint]: Switching to dedicated user 'mysql'
mysql-server  | 2023-06-10 21:01:55+00:00 [Note] [Entrypoint]: Entrypoint script for MySQL Server 5.7.37-1debian10 started.
mysql-server  | 2023-06-10 21:01:55+00:00 [Note] [Entrypoint]: Initializing database files
mysql-server  | 2023-06-10T21:01:55.268720Z 0 [Warning] TIMESTAMP with implicit DEFAULT value is deprecated. Please use --explicit_defaults_for_timestamp server option (see documentation for more details).
mysql-server  | 2023-06-10T21:03:08.889339Z 0 [Warning] InnoDB: New log files created, LSN=45790
mysql-server  | 2023-06-10T21:03:20.524873Z 0 [Warning] InnoDB: Creating foreign key constraint system tables.
mysql-server  | 2023-06-10T21:03:20.943724Z 0 [Warning] No existing UUID has been found, so we assume that this is the first time that this server has been started. Generating a new UUID: 3affd9d3-07d2-11ee-bf8a-0242ac170002.
mysql-server  | 2023-06-10T21:03:20.969868Z 0 [Warning] Gtid table is not ready to be used. Table 'mysql.gtid_executed' cannot be opened.mysql-server  | 2023-06-10T21:03:24.406014Z 0 [Warning] A deprecated TLS version TLSv1 is enabled. Please use TLSv1.2 or higher.
mysql-server  | 2023-06-10T21:03:24.406049Z 0 [Warning] A deprecated TLS version TLSv1.1 is enabled. Please use TLSv1.2 or higher.
mysql-server  | 2023-06-10T21:03:26.194504Z 0 [Warning] CA certificate ca.pem is self signed.
mysql-server  | 2023-06-10T21:03:27.401926Z 1 [Warning] root@localhost is created with an empty password ! Please consider switching off the --initialize-insecure option.
mysql-server  | 2023-06-10 21:04:27+00:00 [Note] [Entrypoint]: Database files initialized
mysql-server  | 2023-06-10 21:04:27+00:00 [Note] [Entrypoint]: Starting temporary server
mysql-server  | 2023-06-10 21:04:27+00:00 [Note] [Entrypoint]: Waiting for server startup
mysql-server  | 2023-06-10T21:04:27.788135Z 0 [Warning] TIMESTAMP with implicit DEFAULT value is deprecated. Please use --explicit_defaults_for_timestamp server option (see documentation for more details).
mysql-server  | 2023-06-10T21:04:27.790569Z 0 [Note] mysqld (mysqld 5.7.37) starting as process 96 ...
mysql-server  | 2023-06-10T21:04:27.795729Z 0 [Note] InnoDB: PUNCH HOLE support available
mysql-server  | 2023-06-10T21:04:27.795768Z 0 [Note] InnoDB: Mutexes and rw_locks use GCC atomic builtins
mysql-server  | 2023-06-10T21:04:27.795775Z 0 [Note] InnoDB: Uses event mutexes mysql-server  | 2023-06-10T21:04:27.795782Z 0 [Note] InnoDB: GCC builtin __atomic_thread_fence() is used for memory barrier
mysql-server  | 2023-06-10T21:04:27.795788Z 0 [Note] InnoDB: Compressed tables use zlib 1.2.11
mysql-server  | 2023-06-10T21:04:27.795793Z 0 [Note] InnoDB: Using Linux native AIO
mysql-server  | 2023-06-10T21:04:27.796214Z 0 [Note] InnoDB: Number of pools: 1
mysql-server  | 2023-06-10T21:04:27.796406Z 0 [Note] InnoDB: Using CPU crc32 instructions
mysql-server  | 2023-06-10T21:04:27.799383Z 0 [Note] InnoDB: Initializing buffer  pool, total size = 128M, instances = 1, chunk size = 128M
mysql-server  | 2023-06-10T21:04:27.813342Z 0 [Note] InnoDB: Completed initialization of buffer pool
mysql-server  | 2023-06-10T21:04:27.816733Z 0 [Note] InnoDB: If the mysqld execution user is authorized, page cleaner thread priority can be changed. See the man page of setpriority().
mysql-server  | 2023-06-10T21:04:27.831490Z 0 [Note] InnoDB: Highest supported file format is Barracuda.
mysql-server  | 2023-06-10T21:04:27.857676Z 0 [Note] InnoDB: Creating shared tablespace for temporary tables
mysql-server  | 2023-06-10T21:04:27.858322Z 0 [Note] InnoDB: Setting file './ibtmp1' size to 12 MB. Physically writing the file full; Please wait ...
mysql-server  | 2023-06-10T21:04:29.779871Z 0 [Note] InnoDB: File './ibtmp1' size is now 12 MB.
mysql-server  | 2023-06-10T21:04:29.782324Z 0 [Note] InnoDB: 96 redo rollback segment(s) found. 96 redo rollback segment(s) are active.
mysql-server  | 2023-06-10T21:04:29.782376Z 0 [Note] InnoDB: 32 non-redo rollback segment(s) are active.
mysql-server  | 2023-06-10T21:04:29.784326Z 0 [Note] InnoDB: 5.7.37 started; log sequence number 2751715
mysql-server  | 2023-06-10T21:04:29.785147Z 0 [Note] InnoDB: Loading buffer pool(s) from /var/lib/mysql/ib_buffer_pool
mysql-server  | 2023-06-10T21:04:29.785986Z 0 [Note] Plugin 'FEDERATED' is disabled.
mysql-server  | 2023-06-10T21:04:29.797640Z 0 [Note] InnoDB: Buffer pool(s) load completed at 230610 21:04:29
mysql-server  | 2023-06-10T21:04:29.801788Z 0 [Note] Found ca.pem, server-cert.pem and server-key.pem in data directory. Trying to enable SSL support using them.
mysql-server  | 2023-06-10T21:04:29.802464Z 0 [Note] Skipping generation of SSL certificates as certificate files are present in data directory.
mysql-server  | 2023-06-10T21:04:29.802964Z 0 [Warning] A deprecated TLS version TLSv1 is enabled. Please use TLSv1.2 or higher.
mysql-server  | 2023-06-10T21:04:29.803464Z 0 [Warning] A deprecated TLS version TLSv1.1 is enabled. Please use TLSv1.2 or higher.
mysql-server  | 2023-06-10T21:04:29.805898Z 0 [Warning] CA certificate ca.pem is self signed.
mysql-server  | 2023-06-10T21:04:29.806020Z 0 [Note] Skipping generation of RSA key pair as key files are present in data directory.
mysql-server  | 2023-06-10T21:04:29.873966Z 0 [Warning] Insecure configuration for --pid-file: Location '/var/run/mysqld' in the path is accessible to all OS users. Consider choosing a different directory.
mysql-server  | 2023-06-10T21:04:29.889786Z 0 [Note] Event Scheduler: Loaded 0 events
mysql-server  | 2023-06-10T21:04:29.891823Z 0 [Note] mysqld: ready for connections.
mysql-server  | Version: '5.7.37'  socket: '/var/run/mysqld/mysqld.sock'  port: 0  MySQL Community Server (GPL)
mysql-server  | 2023-06-10 21:04:30+00:00 [Note] [Entrypoint]: Temporary serverstarted.
mysql-server  | Warning: Unable to load '/usr/share/zoneinfo/iso3166.tab' as time zone. Skipping it.
mysql-server  | Warning: Unable to load '/usr/share/zoneinfo/leap-seconds.list'as time zone. Skipping it.
mysql-server  | 2023-06-10T21:05:39.112257Z 0 [Note] InnoDB: page_cleaner: 1000ms intended loop took 62328ms. The settings might not be optimal. (flushed=200 and evicted=0, during the time.)
mysql-server  | Warning: Unable to load '/usr/share/zoneinfo/zone.tab' as time zone. Skipping it.
mysql-server  | Warning: Unable to load '/usr/share/zoneinfo/zone1970.tab' as time zone. Skipping it.
mysql-server  | 2023-06-10 21:05:41+00:00 [Note] [Entrypoint]: Creating database test
mysql-server  | 2023-06-10 21:05:41+00:00 [Note] [Entrypoint]: Creating user java
mysql-server  | 2023-06-10 21:05:41+00:00 [Note] [Entrypoint]: Giving user java access to schema test
mysql-server  |
mysql-server  | 2023-06-10 21:05:41+00:00 [Note] [Entrypoint]: Stopping temporary server
mysql-server  | 2023-06-10T21:05:42.124052Z 0 [Note] Giving 0 client threads a chance to die gracefully
mysql-server  | 2023-06-10T21:05:42.124112Z 0 [Note] Shutting down slave threads
mysql-server  | 2023-06-10T21:05:42.124123Z 0 [Note] Forcefully disconnecting 0 remaining clients
mysql-server  | 2023-06-10T21:05:42.124136Z 0 [Note] Event Scheduler: Purging the queue. 0 events
mysql-server  | 2023-06-10T21:05:42.127217Z 0 [Note] Binlog end
mysql-server  | 2023-06-10T21:05:42.128798Z 0 [Note] Shutting down plugin 'ngram'
mysql-server  | 2023-06-10T21:05:42.128846Z 0 [Note] Shutting down plugin 'partition'
mysql-server  | 2023-06-10T21:05:42.128856Z 0 [Note] Shutting down plugin 'BLACKHOLE'
mysql-server  | 2023-06-10T21:05:42.128865Z 0 [Note] Shutting down plugin 'ARCHIVE'
mysql-server  | 2023-06-10T21:05:42.128872Z 0 [Note] Shutting down plugin 'PERFORMANCE_SCHEMA'
mysql-server  | 2023-06-10T21:05:42.128934Z 0 [Note] Shutting down plugin 'MRG_MYISAM'
mysql-server  | 2023-06-10T21:05:42.128995Z 0 [Note] Shutting down plugin 'MyISAM'
mysql-server  | 2023-06-10T21:05:42.129019Z 0 [Note] Shutting down plugin 'INNODB_SYS_VIRTUAL'
mysql-server  | 2023-06-10T21:05:42.129027Z 0 [Note] Shutting down plugin 'INNODB_SYS_DATAFILES'
mysql-server  | 2023-06-10T21:05:42.129033Z 0 [Note] Shutting down plugin 'INNODB_SYS_TABLESPACES'
mysql-server  | 2023-06-10T21:05:42.129040Z 0 [Note] Shutting down plugin 'INNODB_SYS_FOREIGN_COLS'
mysql-server  | 2023-06-10T21:05:42.129046Z 0 [Note] Shutting down plugin 'INNODB_SYS_FOREIGN'
mysql-server  | 2023-06-10T21:05:42.129051Z 0 [Note] Shutting down plugin 'INNODB_SYS_FIELDS'
mysql-server  | 2023-06-10T21:05:42.129057Z 0 [Note] Shutting down plugin 'INNODB_SYS_COLUMNS'
mysql-server  | 2023-06-10T21:05:42.129063Z 0 [Note] Shutting down plugin 'INNODB_SYS_INDEXES'
mysql-server  | 2023-06-10T21:05:42.129068Z 0 [Note] Shutting down plugin 'INNODB_SYS_TABLESTATS'
mysql-server  | 2023-06-10T21:05:42.129074Z 0 [Note] Shutting down plugin 'INNODB_SYS_TABLES'
mysql-server  | 2023-06-10T21:05:42.129079Z 0 [Note] Shutting down plugin 'INNODB_FT_INDEX_TABLE'
mysql-server  | 2023-06-10T21:05:42.129084Z 0 [Note] Shutting down plugin 'INNODB_FT_INDEX_CACHE'
mysql-server  | 2023-06-10T21:05:42.129090Z 0 [Note] Shutting down plugin 'INNODB_FT_CONFIG'
mysql-server  | 2023-06-10T21:05:42.129095Z 0 [Note] Shutting down plugin 'INNODB_FT_BEING_DELETED'
mysql-server  | 2023-06-10T21:05:42.129100Z 0 [Note] Shutting down plugin 'INNODB_FT_DELETED'
mysql-server  | 2023-06-10T21:05:42.129106Z 0 [Note] Shutting down plugin 'INNODB_FT_DEFAULT_STOPWORD'
mysql-server  | 2023-06-10T21:05:42.129112Z 0 [Note] Shutting down plugin 'INNODB_METRICS'
mysql-server  | 2023-06-10T21:05:42.129117Z 0 [Note] Shutting down plugin 'INNODB_TEMP_TABLE_INFO'
mysql-server  | 2023-06-10T21:05:42.129123Z 0 [Note] Shutting down plugin 'INNODB_BUFFER_POOL_STATS'
mysql-server  | 2023-06-10T21:05:42.129128Z 0 [Note] Shutting down plugin 'INNODB_BUFFER_PAGE_LRU'
mysql-server  | 2023-06-10T21:05:42.129134Z 0 [Note] Shutting down plugin 'INNODB_BUFFER_PAGE'
mysql-server  | 2023-06-10T21:05:42.129139Z 0 [Note] Shutting down plugin 'INNODB_CMP_PER_INDEX_RESET'
mysql-server  | 2023-06-10T21:05:42.129145Z 0 [Note] Shutting down plugin 'INNODB_CMP_PER_INDEX'
mysql-server  | 2023-06-10T21:05:42.129150Z 0 [Note] Shutting down plugin 'INNODB_CMPMEM_RESET'
mysql-server  | 2023-06-10T21:05:42.129155Z 0 [Note] Shutting down plugin 'INNODB_CMPMEM'
mysql-server  | 2023-06-10T21:05:42.129161Z 0 [Note] Shutting down plugin 'INNODB_CMP_RESET'
mysql-server  | 2023-06-10T21:05:42.129166Z 0 [Note] Shutting down plugin 'INNODB_CMP'
mysql-server  | 2023-06-10T21:05:42.129172Z 0 [Note] Shutting down plugin 'INNODB_LOCK_WAITS'
mysql-server  | 2023-06-10T21:05:42.129178Z 0 [Note] Shutting down plugin 'INNODB_LOCKS'
mysql-server  | 2023-06-10T21:05:42.129183Z 0 [Note] Shutting down plugin 'INNODB_TRX'
mysql-server  | 2023-06-10T21:05:42.129189Z 0 [Note] Shutting down plugin 'InnoDB'
mysql-server  | 2023-06-10T21:05:42.130228Z 0 [Note] InnoDB: FTS optimize thread exiting.
mysql-server  | 2023-06-10T21:05:42.130842Z 0 [Note] InnoDB: Starting shutdown...
mysql-server  | 2023-06-10T21:05:42.231696Z 0 [Note] InnoDB: Dumping buffer pool(s) to /var/lib/mysql/ib_buffer_pool
mysql-server  | 2023-06-10T21:05:42.232807Z 0 [Note] InnoDB: Buffer pool(s) dump completed at 230610 21:05:42
mysql-server  | 2023-06-10T21:05:57.964880Z 0 [Note] InnoDB: Shutdown completed; log sequence number 12662109
mysql-server  | 2023-06-10T21:05:57.968148Z 0 [Note] InnoDB: Removed temporary tablespace data file: "ibtmp1"
mysql-server  | 2023-06-10T21:05:57.968201Z 0 [Note] Shutting down plugin 'MEMORY'
mysql-server  | 2023-06-10T21:05:57.968231Z 0 [Note] Shutting down plugin 'CSV'
mysql-server  | 2023-06-10T21:05:57.968247Z 0 [Note] Shutting down plugin 'sha256_password'
mysql-server  | 2023-06-10T21:05:57.968252Z 0 [Note] Shutting down plugin 'mysql_native_password'
mysql-server  | 2023-06-10T21:05:57.968464Z 0 [Note] Shutting down plugin 'binlog'
mysql-server  | 2023-06-10T21:05:57.970825Z 0 [Note] mysqld: Shutdown complete
mysql-server  |
mysql-server  | 2023-06-10 21:05:58+00:00 [Note] [Entrypoint]: Temporary servers topped
mysql-server  |
mysql-server  | 2023-06-10 21:05:58+00:00 [Note] [Entrypoint]: MySQL init process done. Ready for start up.
mysql-server  |
mysql-server  | 2023-06-10T21:05:58.386833Z 0 [Warning] TIMESTAMP with implicit DEFAULT value is deprecated. Please use --explicit_defaults_for_timestamp server  option (see documentation for more details).
mysql-server  | 2023-06-10T21:05:58.389649Z 0 [Note] mysqld (mysqld 5.7.37) starting as process 1 ...
mysql-server  | 2023-06-10T21:05:58.396280Z 0 [Note] InnoDB: PUNCH HOLE support available
mysql-server  | 2023-06-10T21:05:58.396322Z 0 [Note] InnoDB: Mutexes and rw_locks use GCC atomic builtins
mysql-server  | 2023-06-10T21:05:58.396329Z 0 [Note] InnoDB: Uses event mutexes
mysql-server  | 2023-06-10T21:05:58.396335Z 0 [Note] InnoDB: GCC builtin __atomic_thread_fence() is used for memory barrier
mysql-server  | 2023-06-10T21:05:58.396341Z 0 [Note] InnoDB: Compressed tables use zlib 1.2.11
mysql-server  | 2023-06-10T21:05:58.396347Z 0 [Note] InnoDB: Using Linux native AIO
mysql-server  | 2023-06-10T21:05:58.396763Z 0 [Note] InnoDB: Number of pools: 1
mysql-server  | 2023-06-10T21:05:58.396956Z 0 [Note] InnoDB: Using CPU crc32 instructions
mysql-server  | 2023-06-10T21:05:58.399598Z 0 [Note] InnoDB: Initializing buffer pool, total size = 128M, instances = 1, chunk size = 128M
mysql-server  | 2023-06-10T21:05:58.413539Z 0 [Note] InnoDB: Completed initialization of buffer pool
mysql-server  | 2023-06-10T21:05:58.416987Z 0 [Note] InnoDB: If the mysqld execution user is authorized, page cleaner thread priority can be changed. See the man page of setpriority().
mysql-server  | 2023-06-10T21:05:58.432509Z 0 [Note] InnoDB: Highest supported file format is Barracuda.
mysql-server  | 2023-06-10T21:05:58.635407Z 0 [Note] InnoDB: Creating shared tablespace for temporary tables
mysql-server  | 2023-06-10T21:05:58.636188Z 0 [Note] InnoDB: Setting file './ibtmp1' size to 12 MB. Physically writing the file full; Please wait ...
mysql-server  | 2023-06-10T21:06:03.294926Z 0 [Note] InnoDB: File './ibtmp1' size is now 12 MB.
mysql-server  | 2023-06-10T21:06:03.296900Z 0 [Note] InnoDB: 96 redo rollback segment(s) found. 96 redo rollback segment(s) are active.
mysql-server  | 2023-06-10T21:06:03.296931Z 0 [Note] InnoDB: 32 non-redo rollback segment(s) are active.
mysql-server  | 2023-06-10T21:06:03.297821Z 0 [Note] InnoDB: page_cleaner: 1000ms intended loop took 4881ms. The settings might not be optimal. (flushed=0 and evicted=0, during the time.)
mysql-server  | 2023-06-10T21:06:03.297907Z 0 [Note] InnoDB: 5.7.37 started; log sequence number 12662109
mysql-server  | 2023-06-10T21:06:03.298149Z 0 [Note] InnoDB: Loading buffer pool(s) from /var/lib/mysql/ib_buffer_pool
mysql-server  | 2023-06-10T21:06:03.298552Z 0 [Note] Plugin 'FEDERATED' is disabled.
mysql-server  | 2023-06-10T21:06:03.312075Z 0 [Note] Found ca.pem, server-cert.pem and server-key.pem in data directory. Trying to enable SSL support using them.
mysql-server  | 2023-06-10T21:06:03.312126Z 0 [Note] Skipping generation of SSL certificates as certificate files are present in data directory.
mysql-server  | 2023-06-10T21:06:03.312135Z 0 [Warning] A deprecated TLS version TLSv1 is enabled. Please use TLSv1.2 or higher.
mysql-server  | 2023-06-10T21:06:03.314094Z 0 [Warning] A deprecated TLS version TLSv1.1 is enabled. Please use TLSv1.2 or higher.
mysql-server  | 2023-06-10T21:06:03.315364Z 0 [Warning] CA certificate ca.pem is self signed.
mysql-server  | 2023-06-10T21:06:03.315464Z 0 [Note] Skipping generation of RSA key pair as key files are present in data directory.
mysql-server  | 2023-06-10T21:06:03.315990Z 0 [Note] InnoDB: Buffer pool(s) load completed at 230610 21:06:03
mysql-server  | 2023-06-10T21:06:03.316640Z 0 [Note] Server hostname (bind-address): '*'; port: 3306
mysql-server  | 2023-06-10T21:06:03.316880Z 0 [Note] IPv6 is available.
mysql-server  | 2023-06-10T21:06:03.316936Z 0 [Note]   - '::' resolves to '::';
mysql-server  | 2023-06-10T21:06:03.316987Z 0 [Note] Server socket created on IP: '::'.
mysql-server  | 2023-06-10T21:06:03.329006Z 0 [Warning] Insecure configuration for --pid-file: Location '/var/run/mysqld' in the path is accessible to all OS users. Consider choosing a different directory.
mysql-server  | 2023-06-10T21:06:03.352784Z 0 [Note] Event Scheduler: Loaded 0 events
mysql-server  | 2023-06-10T21:06:03.353452Z 0 [Note] mysqld: ready for connections.
mysql-server  | Version: '5.7.37'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server (GPL)
```
* verify
```sh
docker container ls
```
```text
CONTAINER ID   IMAGE       COMMAND                  CREATED              STATUS                        PORTS                 NAMES
2248021eb022   mysql:5.7   "docker-entrypoint.s…"   About a minute ago   Up About a minute (healthy)   3306/tcp, 33060/tcp   mysql-server
```
* verify
```
docker-compose ps
```
```text
NAME                IMAGE               COMMAND                  SERVICE             CREATED              STATUS                        PORTS
mysql-server        mysql:5.7           "docker-entrypoint.s…"   mysql-server        About a minute ago   Up About a minute (healthy)   3306/tcp, 33060/tcp
```
### Cleanup
```sh
docke-compose stop
docker-compose rm -f 
docker image prune -f
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
 
