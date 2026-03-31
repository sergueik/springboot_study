### Info
  Ilustrate the https://spring.io/blog/2024/08/23/structured-logging-in-spring-boot-3-4

### Usage
 * prior to __3.4.x__ was not logging in __JSON__
```sh
mvn -f pom.old.xml clean package
```
```sh
java -jar target\example.mdc.jar |tee app.log > NUL
```
```sh
grep Hello app.log | tail -1
```
```text
2026-03-30T20:31:06.337-04:00  INFO 26408 --- [           main] example.Application                      : Hello structured logging
```
* With __3.5.0__
```sh
mvn package
```
```sh
java -jar target\example.mdc.jar |tee app.log > NUL
```

```sh
grep Hello app.log | tail -1 | jq "."
```
```json
{
  "@timestamp": "2026-03-31T00:27:19.469402200Z",
  "log": {
    "level": "INFO",
    "logger": "example.Application"
  },
  "process": {
    "pid": 18600,
    "thread": {
      "name": "main"
    }
  },
  "service": {
    "version": "0.1.0-SNAPSHOT",
    "node": {}
  },
  "message": "Hello structured logging",
  "ecs": {
    "version": "8.11"
  }
}
```
* auto extend with __APM__ [java agent]():

```sh
mvn clean package
```
```sh
ELASTIC_APM_AGENT_VERSION=1.49.0
curl -skLo ~/Downloads/elastic-apm-agent.jar "https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar"
cp  ~/Downloads/elastic-apm-agent.jar .
```

```sh
java -javaagent:elastic-apm-agent.jar -Delastic.apm.service_name=app1 -Delastic.apm.application_packages=example  -Djava.security.egd=file:/dev/./urandom -jar target/example.mdc.jar

```
> NOTE: dropped the usuall option `-Delastic.apm.server_url`
```
java -javaagent:elastic-apm-agent.jar -Delastic.apm.service_name=app1 -Delastic.apm.application_packages=example -Delastic.apm.server_url=http://apm-server:8200 -Djava.security.egd=file:/dev/./urandom -jar target/example.mdc.jar
```

and ignoring the errors

```text
2026-03-30 20:48:36,522 [elastic-apm-server-reporter] ERROR co.elastic.apm.agent.report.IntakeV2ReportingEventHandler - Failed to handle event of type METRICSET_JSON_WRITER with this error: Connection refused: no further information
2026-03-30 20:48:36,522 [elastic-apm-server-reporter] INFO  co.elastic.apm.agent.report.AbstractIntakeApiHandler - Backing off for 0 seconds (+/-10%)
2026-03-30 20:48:36,524 [elastic-apm-server-reporter] ERROR co.elastic.apm.agent.report.AbstractIntakeApiHandler - Error trying to connect to APM Server at http://127.0.0.1:8200/intake/v2/events. Although not necessarily related to SSL, some related SSL configurations corresponding the current connection are logged at INFO level.
2026-03-30 20:48:36,525 [elastic-apm-server-reporter] ERROR co.elastic.apm.agent.report.IntakeV2ReportingEventHandler - Failed to handle event of type METRICSET_JSON_WRITER with this error: Connection refused: no further information
2026-03-30 20:48:36,525 [elastic-apm-server-reporter] INFO  co.elastic.apm.agent.report.AbstractIntakeApiHandler - Backing off for 1 seconds (+/-10%)
```

```sh
java -javaagent:elastic-apm-agent.jar -Delastic.apm.service_name=app1 -Delastic.apm.application_packages=example  -Djava.security.egd=file:/dev/./urandom -jar target/example.mdc.jar | tee a.log > NUL
```
```sh
grep Hello a.log | tail -1 | jq "."
```
```json
{
  "@timestamp": "2026-03-31T00:50:07.013340800Z",
  "log": {
    "level": "INFO",
    "logger": "example.Application"
  },
  "process": {
    "pid": 26032,
    "thread": {
      "name": "main"
    }
  },
  "service": {
    "version": "0.1.0-SNAPSHOT",
    "node": {}
  },
  "message": "Hello structured logging",
  "ecs": {
    "version": "8.11"
  }
}
```

```sh
test -d app/target ||mkdir app/target
cp target/example.mdc.jar app/target
```
```sh
docker-compose up --build -d
```

```sh
docker-compose logs app
```


```sh
docker-compose logs app | grep --color=never "Hello" | tail -1 |cut -f 2 -d '|' |strings | sed "s|\[[0-9]m||" | jq "."
```

```json
{
  "@timestamp": "2026-03-31T01:25:43.332152153Z",
  "log": {
    "level": "INFO",
    "logger": "example.Application"
  },
  "process": {
    "pid": 1,
    "thread": {
      "name": "main"
    }
  },
  "service": {
    "version": "0.1.0-SNAPSHOT",
    "node": {}
  },
  "message": "Hello structured logging",
  "ecs": {
    "version": "8.11"
  }
}

```
add explicit transaction:
```java
    @Override
    public void run(String... args) {
        Transaction transaction = ElasticApm.startTransaction();
        try {
            transaction.setName("hello-lab");
            transaction.setType("lab");

            transaction.activate();
            log.info("Hello structured logging");
        } finally {
            transaction.end();
            System.exit(0);
        }
    }
```
repeat the build,run with java agent 
```json
{
  "@timestamp": "2026-03-31T01:46:28.416467600Z",
  "log": {
    "level": "INFO",
    "logger": "example.Application"
  },
  "process": {
    "pid": 24388,
    "thread": {
      "name": "main"
    }
  },
  "service": {
    "version": "0.1.0-SNAPSHOT",
    "node": {}
  },
  "message": "Hello structured logging",
  "transaction": {
    "id": "4fa7247e8da2ff6d"
  },
  "trace": {
    "id": "71c4a9c44925fb8990e04a3bdd38a154"
  },
  "ecs": {
    "version": "8.11"
  }
}

```
the closest Spring Batch equivalent is
```java
public class BatchMdcJobListener implements JobExecutionListener {

    @Override
    public void beforeJob(JobExecution jobExecution) {
        MDC.put("jobName", jobExecution.getJobInstance().getJobName());
        MDC.put("jobExecutionId", String.valueOf(jobExecution.getId()));
    }

    @Override
    public void afterJob(JobExecution jobExecution) {
        MDC.clear();
    }
}
```
### Troubleshooting
```text
docker-compose up --build -d
Creating network "basic-logs-mdc_elastic" with the default driver
Building apm-server
DEPRECATED: The legacy builder is deprecated and will be removed in a future release.
            Install the buildx component to build images with BuildKit:
            https://docs.docker.com/go/buildx/

Sending build context to Docker daemon  506.4kB
Step 1/11 : FROM docker.elastic.co/apm/apm-server:8.17.8
8.17.8: Pulling from apm/apm-server
72fbfa3bf5fc: Pulling fs layer
445678d54ca4: Pulling fs layer
9d46aed32d04: Pulling fs layer
d28f4f4da841: Pulling fs layer
e71c00bb31a2: Pulling fs layer
a70b20993b5a: Pulling fs layer
516a23e5622b: Pulling fs layer
eae9d771ec7a: Pulling fs layer
3eb005cc3716: Pulling fs layer
3ffe579908f6: Pulling fs layer
86da36d8b11a: Pulling fs layer
31d357487211: Pulling fs layer
70f53b2a9b58: Pulling fs layer
49e52acdf3c9: Pulling fs layer
eae9d771ec7a: Waiting
3eb005cc3716: Waiting
3ffe579908f6: Waiting
e71c00bb31a2: Waiting
a70b20993b5a: Waiting
d28f4f4da841: Waiting
86da36d8b11a: Waiting
70f53b2a9b58: Waiting
31d357487211: Waiting
49e52acdf3c9: Waiting
445678d54ca4: Verifying Checksum
9d46aed32d04: Verifying Checksum
9d46aed32d04: Download complete
d28f4f4da841: Verifying Checksum
d28f4f4da841: Download complete
e71c00bb31a2: Verifying Checksum
e71c00bb31a2: Download complete
516a23e5622b: Download complete
72fbfa3bf5fc: Verifying Checksum
72fbfa3bf5fc: Download complete
a70b20993b5a: Verifying Checksum
a70b20993b5a: Download complete
3eb005cc3716: Verifying Checksum
3eb005cc3716: Download complete
3ffe579908f6: Verifying Checksum
3ffe579908f6: Download complete
86da36d8b11a: Verifying Checksum
86da36d8b11a: Download complete
eae9d771ec7a: Verifying Checksum
eae9d771ec7a: Download complete
31d357487211: Verifying Checksum
31d357487211: Download complete
72fbfa3bf5fc: Pull complete
445678d54ca4: Pull complete
9d46aed32d04: Pull complete
49e52acdf3c9: Verifying Checksum
49e52acdf3c9: Download complete
d28f4f4da841: Pull complete
e71c00bb31a2: Pull complete
a70b20993b5a: Pull complete
516a23e5622b: Pull complete
70f53b2a9b58: Download complete
eae9d771ec7a: Pull complete
3eb005cc3716: Pull complete
3ffe579908f6: Pull complete
86da36d8b11a: Pull complete
31d357487211: Pull complete
70f53b2a9b58: Pull complete
49e52acdf3c9: Pull complete
Digest: sha256:ce2ea30fb0d2b382ce00388bf9379958d7e451e8998045eedefdbcbb210db873
Status: Downloaded newer image for docker.elastic.co/apm/apm-server:8.17.8
 ---> b07d7f5ccfe4
Step 2/11 : USER root
 ---> Running in 7811da143f51
 ---> Removed intermediate container 7811da143f51
 ---> eb482585b252
Step 3/11 : RUN apt-get update && apt -y --fix-broken install
 ---> Running in 8c3708630f76
Get:1 http://archive.ubuntu.com/ubuntu jammy InRelease [270 kB]
Get:2 http://security.ubuntu.com/ubuntu jammy-security InRelease [129 kB]
Get:3 http://security.ubuntu.com/ubuntu jammy-security/main amd64 Packages [3842 kB]
Get:4 http://archive.ubuntu.com/ubuntu jammy-updates InRelease [128 kB]
Get:5 http://archive.ubuntu.com/ubuntu jammy-backports InRelease [127 kB]
Get:6 http://archive.ubuntu.com/ubuntu jammy/restricted amd64 Packages [164 kB]
Get:7 http://archive.ubuntu.com/ubuntu jammy/multiverse amd64 Packages [266 kB]
Get:8 http://archive.ubuntu.com/ubuntu jammy/universe amd64 Packages [17.5 MB]
Get:9 http://security.ubuntu.com/ubuntu jammy-security/multiverse amd64 Packages [62.6 kB]
Get:10 http://security.ubuntu.com/ubuntu jammy-security/universe amd64 Packages [1309 kB]
Get:11 http://security.ubuntu.com/ubuntu jammy-security/restricted amd64 Packages [6803 kB]
Get:12 http://archive.ubuntu.com/ubuntu jammy/main amd64 Packages [1792 kB]
Get:13 http://archive.ubuntu.com/ubuntu jammy-updates/multiverse amd64 Packages [70.9 kB]
Get:14 http://archive.ubuntu.com/ubuntu jammy-updates/universe amd64 Packages [1620 kB]
Get:15 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 Packages [4173 kB]
Get:16 http://archive.ubuntu.com/ubuntu jammy-updates/restricted amd64 Packages [7011 kB]
Get:17 http://archive.ubuntu.com/ubuntu jammy-backports/universe amd64 Packages [35.6 kB]
Get:18 http://archive.ubuntu.com/ubuntu jammy-backports/main amd64 Packages [84.0 kB]
Fetched 45.4 MB in 6s (7423 kB/s)
Reading package lists...

WARNING: apt does not have a stable CLI interface. Use with caution in scripts.

Reading package lists...
Building dependency tree...
Reading state information...
Correcting dependencies... Done
The following additional packages will be installed:
  krb5-locales libgssapi-krb5-2 libk5crypto3 libkrb5-3 libkrb5support0 libnsl2
  libpam-modules libpam-modules-bin libtirpc3
Suggested packages:
  krb5-doc krb5-user
The following NEW packages will be installed:
  krb5-locales libgssapi-krb5-2 libk5crypto3 libkrb5-3 libkrb5support0 libnsl2
  libtirpc3
The following packages will be upgraded:
  libpam-modules libpam-modules-bin
2 upgraded, 7 newly installed, 0 to remove and 23 not upgraded.
Need to get 1075 kB of archives.
After this operation, 2505 kB of additional disk space will be used.
Get:1 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libkrb5support0 amd64 1.19.2-2ubuntu0.7 [32.7 kB]
Get:2 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libk5crypto3 amd64 1.19.2-2ubuntu0.7 [86.5 kB]
Get:3 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libkrb5-3 amd64 1.19.2-2ubuntu0.7 [356 kB]
Get:4 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libgssapi-krb5-2 amd64 1.19.2-2ubuntu0.7 [144 kB]
Get:5 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libtirpc3 amd64 1.3.2-2ubuntu0.1 [82.3 kB]
Get:6 http://archive.ubuntu.com/ubuntu jammy/main amd64 libnsl2 amd64 1.3.0-2build2 [42.3 kB]
Get:7 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libpam-modules-bin amd64 1.4.0-11ubuntu2.6 [37.4 kB]
Get:8 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libpam-modules amd64 1.4.0-11ubuntu2.6 [282 kB]
Get:9 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 krb5-locales all 1.19.2-2ubuntu0.7 [12.2 kB]
debconf: delaying package configuration, since apt-utils is not installed
Fetched 1075 kB in 2s (709 kB/s)
Selecting previously unselected package libkrb5support0:amd64.
(Reading database ... 4351 files and directories currently installed.)
Preparing to unpack .../libkrb5support0_1.19.2-2ubuntu0.7_amd64.deb ...
Unpacking libkrb5support0:amd64 (1.19.2-2ubuntu0.7) ...
Setting up libkrb5support0:amd64 (1.19.2-2ubuntu0.7) ...
Selecting previously unselected package libk5crypto3:amd64.
(Reading database ... 4356 files and directories currently installed.)
Preparing to unpack .../libk5crypto3_1.19.2-2ubuntu0.7_amd64.deb ...
Unpacking libk5crypto3:amd64 (1.19.2-2ubuntu0.7) ...
Setting up libk5crypto3:amd64 (1.19.2-2ubuntu0.7) ...
Selecting previously unselected package libkrb5-3:amd64.
(Reading database ... 4361 files and directories currently installed.)
Preparing to unpack .../libkrb5-3_1.19.2-2ubuntu0.7_amd64.deb ...
Unpacking libkrb5-3:amd64 (1.19.2-2ubuntu0.7) ...
Setting up libkrb5-3:amd64 (1.19.2-2ubuntu0.7) ...
Selecting previously unselected package libgssapi-krb5-2:amd64.
(Reading database ... 4374 files and directories currently installed.)
Preparing to unpack .../libgssapi-krb5-2_1.19.2-2ubuntu0.7_amd64.deb ...
Unpacking libgssapi-krb5-2:amd64 (1.19.2-2ubuntu0.7) ...
Setting up libgssapi-krb5-2:amd64 (1.19.2-2ubuntu0.7) ...
Selecting previously unselected package libtirpc3:amd64.
(Reading database ... 4382 files and directories currently installed.)
Preparing to unpack .../libtirpc3_1.3.2-2ubuntu0.1_amd64.deb ...
Unpacking libtirpc3:amd64 (1.3.2-2ubuntu0.1) ...
Setting up libtirpc3:amd64 (1.3.2-2ubuntu0.1) ...
Selecting previously unselected package libnsl2:amd64.
(Reading database ... 4388 files and directories currently installed.)
Preparing to unpack .../libnsl2_1.3.0-2build2_amd64.deb ...
Unpacking libnsl2:amd64 (1.3.0-2build2) ...
Setting up libnsl2:amd64 (1.3.0-2build2) ...
(Reading database ... 4393 files and directories currently installed.)
Preparing to unpack .../libpam-modules-bin_1.4.0-11ubuntu2.6_amd64.deb ...
Unpacking libpam-modules-bin (1.4.0-11ubuntu2.6) over (1.4.0-11ubuntu2.5) ...
Setting up libpam-modules-bin (1.4.0-11ubuntu2.6) ...
(Reading database ... 4393 files and directories currently installed.)
Preparing to unpack .../libpam-modules_1.4.0-11ubuntu2.6_amd64.deb ...
debconf: unable to initialize frontend: Dialog
debconf: (TERM is not set, so the dialog frontend is not usable.)
debconf: falling back to frontend: Readline
debconf: unable to initialize frontend: Readline
debconf: (Can't locate Term/ReadLine.pm in @INC (you may need to install the Term::ReadLine module) (@INC contains: /etc/perl /usr/local/lib/x86_64-linux-gnu/perl/5.34.0 /usr/local/share/perl/5.34.0 /usr/lib/x86_64-linux-gnu/perl5/5.34 /usr/share/perl5 /usr/lib/x86_64-linux-gnu/perl-base /usr/lib/x86_64-linux-gnu/perl/5.34 /usr/share/perl/5.34 /usr/local/lib/site_perl) at /usr/share/perl5/Debconf/FrontEnd/Readline.pm line 7.)
debconf: falling back to frontend: Teletype
Unpacking libpam-modules:amd64 (1.4.0-11ubuntu2.6) over (1.4.0-11ubuntu2.5) ...
Setting up libpam-modules:amd64 (1.4.0-11ubuntu2.6) ...
Installing new version of config file /etc/security/namespace.init ...
Selecting previously unselected package krb5-locales.
(Reading database ... 4393 files and directories currently installed.)
Preparing to unpack .../krb5-locales_1.19.2-2ubuntu0.7_all.deb ...
Unpacking krb5-locales (1.19.2-2ubuntu0.7) ...
Setting up krb5-locales (1.19.2-2ubuntu0.7) ...
Processing triggers for libc-bin (2.35-0ubuntu3.10) ...
 ---> Removed intermediate container 8c3708630f76
 ---> 4c249174e533
Step 4/11 : RUN apt-get install -q -y curl
 ---> Running in 2c084ae0bb79
Reading package lists...
Building dependency tree...
Reading state information...
The following additional packages will be installed:
  ca-certificates libbrotli1 libcurl4 libldap-2.5-0 libldap-common
  libnghttp2-14 libpsl5 librtmp1 libsasl2-2 libsasl2-modules
  libsasl2-modules-db libssh-4 openssl publicsuffix
Suggested packages:
  libsasl2-modules-gssapi-mit | libsasl2-modules-gssapi-heimdal
  libsasl2-modules-ldap libsasl2-modules-otp libsasl2-modules-sql
The following NEW packages will be installed:
  ca-certificates curl libbrotli1 libcurl4 libldap-2.5-0 libldap-common
  libnghttp2-14 libpsl5 librtmp1 libsasl2-2 libsasl2-modules
  libsasl2-modules-db libssh-4 openssl publicsuffix
0 upgraded, 15 newly installed, 0 to remove and 23 not upgraded.
Need to get 2999 kB of archives.
After this operation, 7149 kB of additional disk space will be used.
Get:1 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 openssl amd64 3.0.2-0ubuntu1.21 [1184 kB]
Get:2 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 ca-certificates all 20240203~22.04.1 [162 kB]
Get:3 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libnghttp2-14 amd64 1.43.0-1ubuntu0.2 [76.9 kB]
Get:4 http://archive.ubuntu.com/ubuntu jammy/main amd64 libpsl5 amd64 0.21.0-1.2build2 [58.4 kB]
Get:5 http://archive.ubuntu.com/ubuntu jammy/main amd64 publicsuffix all 20211207.1025-1 [129 kB]
Get:6 http://archive.ubuntu.com/ubuntu jammy/main amd64 libbrotli1 amd64 1.0.9-2build6 [315 kB]
Get:7 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libsasl2-modules-db amd64 2.1.27+dfsg2-3ubuntu1.2 [20.5 kB]
Get:8 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libsasl2-2 amd64 2.1.27+dfsg2-3ubuntu1.2 [53.8 kB]
Get:9 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libldap-2.5-0 amd64 2.5.20+dfsg-0ubuntu0.22.04.1 [184 kB]
Get:10 http://archive.ubuntu.com/ubuntu jammy/main amd64 librtmp1 amd64 2.4+20151223.gitfa8646d.1-2build4 [58.2 kB]
Get:11 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libssh-4 amd64 0.9.6-2ubuntu0.22.04.7 [187 kB]
Get:12 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libcurl4 amd64 7.81.0-1ubuntu1.23 [290 kB]
Get:13 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 curl amd64 7.81.0-1ubuntu1.23 [194 kB]
Get:14 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libldap-common all 2.5.20+dfsg-0ubuntu0.22.04.1 [16.4 kB]
Get:15 http://archive.ubuntu.com/ubuntu jammy-updates/main amd64 libsasl2-modules amd64 2.1.27+dfsg2-3ubuntu1.2 [68.8 kB]
debconf: delaying package configuration, since apt-utils is not installed
Fetched 2999 kB in 1s (3139 kB/s)
Selecting previously unselected package openssl.
(Reading database ... 4396 files and directories currently installed.)
Preparing to unpack .../00-openssl_3.0.2-0ubuntu1.21_amd64.deb ...
Unpacking openssl (3.0.2-0ubuntu1.21) ...
Selecting previously unselected package ca-certificates.
Preparing to unpack .../01-ca-certificates_20240203~22.04.1_all.deb ...
Unpacking ca-certificates (20240203~22.04.1) ...
Selecting previously unselected package libnghttp2-14:amd64.
Preparing to unpack .../02-libnghttp2-14_1.43.0-1ubuntu0.2_amd64.deb ...
Unpacking libnghttp2-14:amd64 (1.43.0-1ubuntu0.2) ...
Selecting previously unselected package libpsl5:amd64.
Preparing to unpack .../03-libpsl5_0.21.0-1.2build2_amd64.deb ...
Unpacking libpsl5:amd64 (0.21.0-1.2build2) ...
Selecting previously unselected package publicsuffix.
Preparing to unpack .../04-publicsuffix_20211207.1025-1_all.deb ...
Unpacking publicsuffix (20211207.1025-1) ...
Selecting previously unselected package libbrotli1:amd64.
Preparing to unpack .../05-libbrotli1_1.0.9-2build6_amd64.deb ...
Unpacking libbrotli1:amd64 (1.0.9-2build6) ...
Selecting previously unselected package libsasl2-modules-db:amd64.
Preparing to unpack .../06-libsasl2-modules-db_2.1.27+dfsg2-3ubuntu1.2_amd64.deb ...
Unpacking libsasl2-modules-db:amd64 (2.1.27+dfsg2-3ubuntu1.2) ...
Selecting previously unselected package libsasl2-2:amd64.
Preparing to unpack .../07-libsasl2-2_2.1.27+dfsg2-3ubuntu1.2_amd64.deb ...
Unpacking libsasl2-2:amd64 (2.1.27+dfsg2-3ubuntu1.2) ...
Selecting previously unselected package libldap-2.5-0:amd64.
Preparing to unpack .../08-libldap-2.5-0_2.5.20+dfsg-0ubuntu0.22.04.1_amd64.deb ...
Unpacking libldap-2.5-0:amd64 (2.5.20+dfsg-0ubuntu0.22.04.1) ...
Selecting previously unselected package librtmp1:amd64.
Preparing to unpack .../09-librtmp1_2.4+20151223.gitfa8646d.1-2build4_amd64.deb ...
Unpacking librtmp1:amd64 (2.4+20151223.gitfa8646d.1-2build4) ...
Selecting previously unselected package libssh-4:amd64.
Preparing to unpack .../10-libssh-4_0.9.6-2ubuntu0.22.04.7_amd64.deb ...
Unpacking libssh-4:amd64 (0.9.6-2ubuntu0.22.04.7) ...
Selecting previously unselected package libcurl4:amd64.
Preparing to unpack .../11-libcurl4_7.81.0-1ubuntu1.23_amd64.deb ...
Unpacking libcurl4:amd64 (7.81.0-1ubuntu1.23) ...
Selecting previously unselected package curl.
Preparing to unpack .../12-curl_7.81.0-1ubuntu1.23_amd64.deb ...
Unpacking curl (7.81.0-1ubuntu1.23) ...
Selecting previously unselected package libldap-common.
Preparing to unpack .../13-libldap-common_2.5.20+dfsg-0ubuntu0.22.04.1_all.deb ...
Unpacking libldap-common (2.5.20+dfsg-0ubuntu0.22.04.1) ...
Selecting previously unselected package libsasl2-modules:amd64.
Preparing to unpack .../14-libsasl2-modules_2.1.27+dfsg2-3ubuntu1.2_amd64.deb ...
Unpacking libsasl2-modules:amd64 (2.1.27+dfsg2-3ubuntu1.2) ...
Setting up libpsl5:amd64 (0.21.0-1.2build2) ...
Setting up libbrotli1:amd64 (1.0.9-2build6) ...
Setting up libsasl2-modules:amd64 (2.1.27+dfsg2-3ubuntu1.2) ...
Setting up libnghttp2-14:amd64 (1.43.0-1ubuntu0.2) ...
Setting up libldap-common (2.5.20+dfsg-0ubuntu0.22.04.1) ...
Setting up libsasl2-modules-db:amd64 (2.1.27+dfsg2-3ubuntu1.2) ...
Setting up librtmp1:amd64 (2.4+20151223.gitfa8646d.1-2build4) ...
Setting up libsasl2-2:amd64 (2.1.27+dfsg2-3ubuntu1.2) ...
Setting up libssh-4:amd64 (0.9.6-2ubuntu0.22.04.7) ...
Setting up openssl (3.0.2-0ubuntu1.21) ...
Setting up publicsuffix (20211207.1025-1) ...
Setting up libldap-2.5-0:amd64 (2.5.20+dfsg-0ubuntu0.22.04.1) ...
Setting up ca-certificates (20240203~22.04.1) ...
debconf: unable to initialize frontend: Dialog
debconf: (TERM is not set, so the dialog frontend is not usable.)
debconf: falling back to frontend: Readline
debconf: unable to initialize frontend: Readline
debconf: (Can't locate Term/ReadLine.pm in @INC (you may need to install the Term::ReadLine module) (@INC contains: /etc/perl /usr/local/lib/x86_64-linux-gnu/perl/5.34.0 /usr/local/share/perl/5.34.0 /usr/lib/x86_64-linux-gnu/perl5/5.34 /usr/share/perl5 /usr/lib/x86_64-linux-gnu/perl-base /usr/lib/x86_64-linux-gnu/perl/5.34 /usr/share/perl/5.34 /usr/local/lib/site_perl) at /usr/share/perl5/Debconf/FrontEnd/Readline.pm line 7.)
debconf: falling back to frontend: Teletype
Updating certificates in /etc/ssl/certs...
146 added, 0 removed; done.
Setting up libcurl4:amd64 (7.81.0-1ubuntu1.23) ...
Setting up curl (7.81.0-1ubuntu1.23) ...
Processing triggers for libc-bin (2.35-0ubuntu3.10) ...
Processing triggers for ca-certificates (20240203~22.04.1) ...
Updating certificates in /etc/ssl/certs...
0 added, 0 removed; done.
Running hooks in /etc/ca-certificates/update.d...
done.
 ---> Removed intermediate container 2c084ae0bb79
 ---> d769f36cf1c4
Step 5/11 : ADD config/apm-server.yml /usr/share/apm-server/apm-server.yml
 ---> 297e47dd436b
Step 6/11 : RUN chown apm-server  /usr/share/apm-server/apm-server.yml
 ---> Running in 8bac0c304e94
 ---> Removed intermediate container 8bac0c304e94
 ---> 166e8a6277f9
Step 7/11 : RUN chmod 644 /usr/share/apm-server/apm-server.yml
 ---> Running in 1718e01cb9b8
 ---> Removed intermediate container 1718e01cb9b8
 ---> a0239854f4dd
Step 8/11 : ADD config/fields.yml /usr/share/apm-server/fields.yml
 ---> 86aa2c6e8764
Step 9/11 : RUN chown apm-server  /usr/share/apm-server/fields.yml
 ---> Running in 2634a680b981
 ---> Removed intermediate container 2634a680b981
 ---> 5d6c96bf67ae
Step 10/11 : RUN chmod 644 /usr/share/apm-server/fields.yml
 ---> Running in 0f5f24e6f707
 ---> Removed intermediate container 0f5f24e6f707
 ---> 167f83ffa524
Step 11/11 : USER apm-server
 ---> Running in f0f3b292d5ef
 ---> Removed intermediate container f0f3b292d5ef
 ---> ad5780070ebf
Successfully built ad5780070ebf
Successfully tagged basic-logs-mdc_apm-server:latest
Building app
DEPRECATED: The legacy builder is deprecated and will be removed in a future release.
            Install the buildx component to build images with BuildKit:
            https://docs.docker.com/go/buildx/

Sending build context to Docker daemon  26.84MB
Step 1/12 : FROM eclipse-temurin:17-jre-alpine
 ---> 10a02d7c48ad
Step 2/12 : ARG HOME=/home
 ---> Running in b616645b92b5
 ---> Removed intermediate container b616645b92b5
 ---> a720dd0c0929
Step 3/12 : WORKDIR ${HOME}
 ---> Running in 77cacf0777ce
 ---> Removed intermediate container 77cacf0777ce
 ---> a25c01d808e2
Step 4/12 : RUN apk add --no-cache curl wget
 ---> Running in 0220b8fbafaf
(1/8) Installing c-ares (1.34.6-r0)
(2/8) Installing nghttp2-libs (1.68.0-r0)
(3/8) Installing nghttp3 (1.13.1-r0)
(4/8) Installing libpsl (0.21.5-r3)
(5/8) Installing libcurl (8.17.0-r1)
(6/8) Installing curl (8.17.0-r1)
(7/8) Installing pcre2 (10.47-r0)
(8/8) Installing wget (1.25.0-r2)
Executing busybox-1.37.0-r30.trigger
OK: 43.1 MiB in 81 packages
 ---> Removed intermediate container 0220b8fbafaf
 ---> 750b5fa8edf0
Step 5/12 : ARG ELASTIC_APM_AGENT_VERSION=1.49.0
 ---> Running in 63221eed6a10
 ---> Removed intermediate container 63221eed6a10
 ---> c20e9bfcddc2
Step 6/12 : ARG APM_SERVER="apm_server"
 ---> Running in d7a8ef4f000a
 ---> Removed intermediate container d7a8ef4f000a
 ---> 5572c0100b4a
Step 7/12 : RUN wget -O ${HOME}/elastic-apm-agent.jar https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar
 ---> Running in ba5f0584cc82
--2026-03-31 01:15:39--  https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/1.49.0/elastic-apm-agent-1.49.0.jar
Resolving search.maven.org (search.maven.org)... 3.166.181.65, 3.166.181.46, 3.166.181.64, ...
Connecting to search.maven.org (search.maven.org)|3.166.181.65|:443... connected.
HTTP request sent, awaiting response... 301 Moved Permanently
Location: https://repo1.maven.org/maven2/co/elastic/apm/elastic-apm-agent/1.49.0/elastic-apm-agent-1.49.0.jar [following]
--2026-03-31 01:15:42--  https://repo1.maven.org/maven2/co/elastic/apm/elastic-apm-agent/1.49.0/elastic-apm-agent-1.49.0.jar
Resolving repo1.maven.org (repo1.maven.org)... 104.18.18.12, 104.18.19.12, 2606:4700::6812:130c, ...
Connecting to repo1.maven.org (repo1.maven.org)|104.18.18.12|:443... connected.
HTTP request sent, awaiting response... 200 OK
Length: 11308600 (11M) [application/java-archive]
Saving to: '/home/elastic-apm-agent.jar'

     0K .......... .......... .......... .......... ..........  0% 5.23M 2s
    50K .......... .......... .......... .......... ..........  0% 1.23M 5s
   100K .......... .......... .......... .......... ..........  1% 1.62M 6s
   150K .......... .......... .......... .......... ..........  1% 4.69M 5s
   200K .......... .......... .......... .......... ..........  2% 3.51M 4s
   250K .......... .......... .......... .......... ..........  2% 5.20M 4s
   300K .......... .......... .......... .......... ..........  3% 5.80M 4s
   350K .......... .......... .......... .......... ..........  3% 4.75M 3s
   400K .......... .......... .......... .......... ..........  4% 3.18M 3s
   450K .......... .......... .......... .......... ..........  4% 6.85M 3s
   500K .......... .......... .......... .......... ..........  4% 5.89M 3s
   550K .......... .......... .......... .......... ..........  5% 43.8M 3s
   600K .......... .......... .......... .......... ..........  5% 5.48M 3s
   650K .......... .......... .......... .......... ..........  6% 24.4M 3s
   700K .......... .......... .......... .......... ..........  6% 23.7M 2s
   750K .......... .......... .......... .......... ..........  7% 6.85M 2s
   800K .......... .......... .......... .......... ..........  7% 68.9M 2s
   850K .......... .......... .......... .......... ..........  8% 2.86M 2s
   900K .......... .......... .......... .......... ..........  8% 28.2M 2s
   950K .......... .......... .......... .......... ..........  9% 12.3M 2s
  1000K .......... .......... .......... .......... ..........  9% 21.5M 2s
  1050K .......... .......... .......... .......... ..........  9% 23.7M 2s
  1100K .......... .......... .......... .......... .......... 10% 6.11M 2s
  1150K .......... .......... .......... .......... .......... 10% 34.8M 2s
  1200K .......... .......... .......... .......... .......... 11% 76.7M 2s
  1250K .......... .......... .......... .......... .......... 11%  342M 2s
  1300K .......... .......... .......... .......... .......... 12% 2.99M 2s
  1350K .......... .......... .......... .......... .......... 12% 12.3M 2s
  1400K .......... .......... .......... .......... .......... 13% 34.1M 2s
  1450K .......... .......... .......... .......... .......... 13% 7.81M 2s
  1500K .......... .......... .......... .......... .......... 14% 12.3M 2s
  1550K .......... .......... .......... .......... .......... 14%  120M 1s
  1600K .......... .......... .......... .......... .......... 14%  460M 1s
  1650K .......... .......... .......... .......... .......... 15%  471M 1s
  1700K .......... .......... .......... .......... .......... 15% 6.51M 1s
  1750K .......... .......... .......... .......... .......... 16%  283M 1s
  1800K .......... .......... .......... .......... .......... 16%  319M 1s
  1850K .......... .......... .......... .......... .......... 17%  326M 1s
  1900K .......... .......... .......... .......... .......... 17% 9.65M 1s
  1950K .......... .......... .......... .......... .......... 18% 99.1M 1s
  2000K .......... .......... .......... .......... .......... 18% 73.2M 1s
  2050K .......... .......... .......... .......... .......... 19% 8.25M 1s
  2100K .......... .......... .......... .......... .......... 19% 22.8M 1s
  2150K .......... .......... .......... .......... .......... 19%  205M 1s
  2200K .......... .......... .......... .......... .......... 20% 89.0M 1s
  2250K .......... .......... .......... .......... .......... 20%  322M 1s
  2300K .......... .......... .......... .......... .......... 21% 19.2M 1s
  2350K .......... .......... .......... .......... .......... 21% 10.9M 1s
  2400K .......... .......... .......... .......... .......... 22% 14.3M 1s
  2450K .......... .......... .......... .......... .......... 22% 27.8M 1s
  2500K .......... .......... .......... .......... .......... 23% 87.5M 1s
  2550K .......... .......... .......... .......... .......... 23% 38.2M 1s
  2600K .......... .......... .......... .......... .......... 23% 19.2M 1s
  2650K .......... .......... .......... .......... .......... 24%  388M 1s
  2700K .......... .......... .......... .......... .......... 24% 7.55M 1s
  2750K .......... .......... .......... .......... .......... 25% 36.9M 1s
  2800K .......... .......... .......... .......... .......... 25% 53.4M 1s
  2850K .......... .......... .......... .......... .......... 26%  294M 1s
  2900K .......... .......... .......... .......... .......... 26% 50.9M 1s
  2950K .......... .......... .......... .......... .......... 27% 4.68M 1s
  3000K .......... .......... .......... .......... .......... 27% 74.9M 1s
  3050K .......... .......... .......... .......... .......... 28% 37.9M 1s
  3100K .......... .......... .......... .......... .......... 28%  320M 1s
  3150K .......... .......... .......... .......... .......... 28% 13.4M 1s
  3200K .......... .......... .......... .......... .......... 29% 34.6M 1s
  3250K .......... .......... .......... .......... .......... 29%  353M 1s
  3300K .......... .......... .......... .......... .......... 30%  286M 1s
  3350K .......... .......... .......... .......... .......... 30% 56.5M 1s
  3400K .......... .......... .......... .......... .......... 31% 4.21M 1s
  3450K .......... .......... .......... .......... .......... 31%  179M 1s
  3500K .......... .......... .......... .......... .......... 32%  238M 1s
  3550K .......... .......... .......... .......... .......... 32%  325M 1s
  3600K .......... .......... .......... .......... .......... 33% 11.3M 1s
  3650K .......... .......... .......... .......... .......... 33% 19.3M 1s
  3700K .......... .......... .......... .......... .......... 33%  197M 1s
  3750K .......... .......... .......... .......... .......... 34%  342M 1s
  3800K .......... .......... .......... .......... .......... 34% 20.7M 1s
  3850K .......... .......... .......... .......... .......... 35%  107M 1s
  3900K .......... .......... .......... .......... .......... 35% 44.8M 1s
  3950K .......... .......... .......... .......... .......... 36% 14.5M 1s
  4000K .......... .......... .......... .......... .......... 36% 34.2M 1s
  4050K .......... .......... .......... .......... .......... 37% 13.2M 1s
  4100K .......... .......... .......... .......... .......... 37% 13.2M 1s
  4150K .......... .......... .......... .......... .......... 38%  307M 1s
  4200K .......... .......... .......... .......... .......... 38%  202M 1s
  4250K .......... .......... .......... .......... .......... 38% 15.8M 1s
  4300K .......... .......... .......... .......... .......... 39%  318M 1s
  4350K .......... .......... .......... .......... .......... 39%  343M 1s
  4400K .......... .......... .......... .......... .......... 40%  296M 1s
  4450K .......... .......... .......... .......... .......... 40%  247M 1s
  4500K .......... .......... .......... .......... .......... 41%  314M 1s
  4550K .......... .......... .......... .......... .......... 41% 17.6M 1s
  4600K .......... .......... .......... .......... .......... 42% 11.5M 1s
  4650K .......... .......... .......... .......... .......... 42%  156M 0s
  4700K .......... .......... .......... .......... .......... 43% 43.4M 0s
  4750K .......... .......... .......... .......... .......... 43%  314M 0s
  4800K .......... .......... .......... .......... .......... 43% 15.4M 0s
  4850K .......... .......... .......... .......... .......... 44%  392M 0s
  4900K .......... .......... .......... .......... .......... 44%  428M 0s
  4950K .......... .......... .......... .......... .......... 45% 11.2M 0s
  5000K .......... .......... .......... .......... .......... 45% 99.8M 0s
  5050K .......... .......... .......... .......... .......... 46% 13.3M 0s
  5100K .......... .......... .......... .......... .......... 46%  248M 0s
  5150K .......... .......... .......... .......... .......... 47%  130M 0s
  5200K .......... .......... .......... .......... .......... 47%  265M 0s
  5250K .......... .......... .......... .......... .......... 47% 54.0M 0s
  5300K .......... .......... .......... .......... .......... 48% 35.8M 0s
  5350K .......... .......... .......... .......... .......... 48% 70.4M 0s
  5400K .......... .......... .......... .......... .......... 49% 19.6M 0s
  5450K .......... .......... .......... .......... .......... 49% 23.4M 0s
  5500K .......... .......... .......... .......... .......... 50% 7.12M 0s
  5550K .......... .......... .......... .......... .......... 50% 28.0M 0s
  5600K .......... .......... .......... .......... .......... 51% 30.1M 0s
  5650K .......... .......... .......... .......... .......... 51% 68.2M 0s
  5700K .......... .......... .......... .......... .......... 52% 33.6M 0s
  5750K .......... .......... .......... .......... .......... 52% 36.9M 0s
  5800K .......... .......... .......... .......... .......... 52%  180M 0s
  5850K .......... .......... .......... .......... .......... 53%  202M 0s
  5900K .......... .......... .......... .......... .......... 53% 10.3M 0s
  5950K .......... .......... .......... .......... .......... 54% 36.7M 0s
  6000K .......... .......... .......... .......... .......... 54%  211M 0s
  6050K .......... .......... .......... .......... .......... 55%  276M 0s
  6100K .......... .......... .......... .......... .......... 55% 11.9M 0s
  6150K .......... .......... .......... .......... .......... 56%  181M 0s
  6200K .......... .......... .......... .......... .......... 56%  280M 0s
  6250K .......... .......... .......... .......... .......... 57% 14.8M 0s
  6300K .......... .......... .......... .......... .......... 57%  163M 0s
  6350K .......... .......... .......... .......... .......... 57%  232M 0s
  6400K .......... .......... .......... .......... .......... 58% 3.96M 0s
  6450K .......... .......... .......... .......... .......... 58% 85.0M 0s
  6500K .......... .......... .......... .......... .......... 59%  207M 0s
  6550K .......... .......... .......... .......... .......... 59%  294M 0s
  6600K .......... .......... .......... .......... .......... 60%  131M 0s
  6650K .......... .......... .......... .......... .......... 60% 77.2M 0s
  6700K .......... .......... .......... .......... .......... 61%  119M 0s
  6750K .......... .......... .......... .......... .......... 61%  232M 0s
  6800K .......... .......... .......... .......... .......... 62%  280M 0s
  6850K .......... .......... .......... .......... .......... 62% 82.1M 0s
  6900K .......... .......... .......... .......... .......... 62%  309M 0s
  6950K .......... .......... .......... .......... .......... 63% 41.0M 0s
  7000K .......... .......... .......... .......... .......... 63% 25.8M 0s
  7050K .......... .......... .......... .......... .......... 64% 11.8M 0s
  7100K .......... .......... .......... .......... .......... 64% 27.8M 0s
  7150K .......... .......... .......... .......... .......... 65% 14.6M 0s
  7200K .......... .......... .......... .......... .......... 65% 38.0M 0s
  7250K .......... .......... .......... .......... .......... 66% 40.8M 0s
  7300K .......... .......... .......... .......... .......... 66%  108M 0s
  7350K .......... .......... .......... .......... .......... 67%  213M 0s
  7400K .......... .......... .......... .......... .......... 67% 6.70M 0s
  7450K .......... .......... .......... .......... .......... 67% 25.4M 0s
  7500K .......... .......... .......... .......... .......... 68%  115M 0s
  7550K .......... .......... .......... .......... .......... 68% 15.2M 0s
  7600K .......... .......... .......... .......... .......... 69% 20.1M 0s
  7650K .......... .......... .......... .......... .......... 69%  235M 0s
  7700K .......... .......... .......... .......... .......... 70% 97.2M 0s
  7750K .......... .......... .......... .......... .......... 70% 31.3M 0s
  7800K .......... .......... .......... .......... .......... 71% 24.7M 0s
  7850K .......... .......... .......... .......... .......... 71% 26.6M 0s
  7900K .......... .......... .......... .......... .......... 71% 25.3M 0s
  7950K .......... .......... .......... .......... .......... 72% 3.39M 0s
  8000K .......... .......... .......... .......... .......... 72% 98.2M 0s
  8050K .......... .......... .......... .......... .......... 73%  144M 0s
  8100K .......... .......... .......... .......... .......... 73%  264M 0s
  8150K .......... .......... .......... .......... .......... 74%  349M 0s
  8200K .......... .......... .......... .......... .......... 74%  236M 0s
  8250K .......... .......... .......... .......... .......... 75%  243M 0s
  8300K .......... .......... .......... .......... .......... 75%  138M 0s
  8350K .......... .......... .......... .......... .......... 76%  239M 0s
  8400K .......... .......... .......... .......... .......... 76% 29.5M 0s
  8450K .......... .......... .......... .......... .......... 76% 15.8M 0s
  8500K .......... .......... .......... .......... .......... 77%  169M 0s
  8550K .......... .......... .......... .......... .......... 77% 50.8M 0s
  8600K .......... .......... .......... .......... .......... 78% 47.8M 0s
  8650K .......... .......... .......... .......... .......... 78% 9.50M 0s
  8700K .......... .......... .......... .......... .......... 79%  173M 0s
  8750K .......... .......... .......... .......... .......... 79% 23.3M 0s
  8800K .......... .......... .......... .......... .......... 80%  299M 0s
  8850K .......... .......... .......... .......... .......... 80% 22.3M 0s
  8900K .......... .......... .......... .......... .......... 81% 18.3M 0s
  8950K .......... .......... .......... .......... .......... 81% 15.2M 0s
  9000K .......... .......... .......... .......... .......... 81% 31.7M 0s
  9050K .......... .......... .......... .......... .......... 82% 33.9M 0s
  9100K .......... .......... .......... .......... .......... 82%  230M 0s
  9150K .......... .......... .......... .......... .......... 83% 72.9M 0s
  9200K .......... .......... .......... .......... .......... 83%  220M 0s
  9250K .......... .......... .......... .......... .......... 84% 95.8M 0s
  9300K .......... .......... .......... .......... .......... 84% 11.4M 0s
  9350K .......... .......... .......... .......... .......... 85% 34.8M 0s
  9400K .......... .......... .......... .......... .......... 85% 29.1M 0s
  9450K .......... .......... .......... .......... .......... 86% 31.8M 0s
  9500K .......... .......... .......... .......... .......... 86% 51.0M 0s
  9550K .......... .......... .......... .......... .......... 86% 4.76M 0s
  9600K .......... .......... .......... .......... .......... 87%  313M 0s
  9650K .......... .......... .......... .......... .......... 87%  117M 0s
  9700K .......... .......... .......... .......... .......... 88% 16.3M 0s
  9750K .......... .......... .......... .......... .......... 88%  156M 0s
  9800K .......... .......... .......... .......... .......... 89%  191M 0s
  9850K .......... .......... .......... .......... .......... 89% 51.7M 0s
  9900K .......... .......... .......... .......... .......... 90%  238M 0s
  9950K .......... .......... .......... .......... .......... 90%  172M 0s
 10000K .......... .......... .......... .......... .......... 91% 10.2M 0s
 10050K .......... .......... .......... .......... .......... 91% 17.5M 0s
 10100K .......... .......... .......... .......... .......... 91%  123M 0s
 10150K .......... .......... .......... .......... .......... 92%  235M 0s
 10200K .......... .......... .......... .......... .......... 92% 39.3M 0s
 10250K .......... .......... .......... .......... .......... 93% 5.51M 0s
 10300K .......... .......... .......... .......... .......... 93%  122M 0s
 10350K .......... .......... .......... .......... .......... 94%  321M 0s
 10400K .......... .......... .......... .......... .......... 94%  272M 0s
 10450K .......... .......... .......... .......... .......... 95% 10.9M 0s
 10500K .......... .......... .......... .......... .......... 95% 33.5M 0s
 10550K .......... .......... .......... .......... .......... 95% 41.5M 0s
 10600K .......... .......... .......... .......... .......... 96% 48.1M 0s
 10650K .......... .......... .......... .......... .......... 96% 43.0M 0s
 10700K .......... .......... .......... .......... .......... 97% 37.2M 0s
 10750K .......... .......... .......... .......... .......... 97% 38.9M 0s
 10800K .......... .......... .......... .......... .......... 98% 15.6M 0s
 10850K .......... .......... .......... .......... .......... 98% 1.11M 0s
 10900K .......... .......... .......... .......... .......... 99% 16.6M 0s
 10950K .......... .......... .......... .......... .......... 99% 8.08M 0s
 11000K .......... .......... .......... .......... ...       100% 33.4M=0.6s

2026-03-31 01:15:45 (17.1 MB/s) - '/home/elastic-apm-agent.jar' saved [11308600/11308600]

 ---> Removed intermediate container ba5f0584cc82
 ---> 72c0bfc79705
Step 8/12 : ARG app_jar="example.mdc.jar"
 ---> Running in 22d7b48eaf75
 ---> Removed intermediate container 22d7b48eaf75
 ---> 71ed02fa3fe7
Step 9/12 : ADD "target/${app_jar}" ${HOME}/app.jar
 ---> cb32e932ce85
Step 10/12 : EXPOSE 8080
 ---> Running in 12e7bec0971b
 ---> Removed intermediate container 12e7bec0971b
 ---> 7afc6e9161ce
Step 11/12 : ENV APM_SERVER=${APM_SERVER}
 ---> Running in ad2e70448fd5
 ---> Removed intermediate container ad2e70448fd5
 ---> 9b2b28dfe898
Step 12/12 : ENTRYPOINT [ "java", "-javaagent:/home/elastic-apm-agent.jar", "-Delastic.apm.service_name=app", "-Delastic.apm.application_packages=example", "-Delastic.apm.server_url=http://apm-server:8200", "-Djava.security.egd=file:/dev/./urandom", "-jar", "/home/app.jar" ]
 ---> Running in 4d590031f543
 ---> Removed intermediate container 4d590031f543
 ---> a60c3981af14
Successfully built a60c3981af14
Successfully tagged basic-logs-mdc_app:latest
Creating apm-server ... done
Creating app        ... done
```

On windows Docker Toolbox

```
docker-compose up --build -d > a.log
```

```
 docker-compose up --build -d > a.log
failed to solve: failed to solve with frontend dockerfile.v0: failed to build LLB: executor failed running [/bin/sh -c apt-get update && apt -y --fix-broken install]: runc did not terminate sucessfully

```
`a.log`:

```text
failed to get console mode for stdout: The handle is invalid.
#1 [basic-logs-mdc-apm-server internal] load build definition from Dockerfile
#1 transferring dockerfile: 30B 0.0s
#1 transferring dockerfile: 32B 0.0s done
#1 DONE 0.1s

#2 [basic-logs-mdc-app internal] load build definition from Dockerfile
#2 transferring dockerfile: 32B 0.0s done
#2 DONE 0.0s

#3 [basic-logs-mdc-app internal] load .dockerignore
#3 transferring context: 2B 0.0s done
#3 DONE 0.0s

#4 [basic-logs-mdc-apm-server internal] load .dockerignore
#4 transferring context: 2B 0.0s done
#4 DONE 0.0s

#5 [basic-logs-mdc-apm-server internal] load metadata for docker.elastic.co/apm/apm-server:8.17.8
#5 ...

#6 [basic-logs-mdc-app internal] load metadata for docker.io/library/eclipse-temurin:17-jre-alpine
#6 DONE 0.7s

#7 [basic-logs-mdc-app 1/5] FROM docker.io/library/eclipse-temurin:17-jre-alpine@sha256:7aa804a1824d18d06c68598fe1c2953b5b203823731be7b9298bb3e0f1920b0d
#7 DONE 0.0s

#8 [basic-logs-mdc-app internal] load build context
#8 transferring context: 72B done
#8 DONE 0.0s

#9 [basic-logs-mdc-app 2/5] WORKDIR /home
#9 CACHED

#10 [basic-logs-mdc-app 3/5] RUN apk add --no-cache curl wget
#10 CACHED

#11 [basic-logs-mdc-app 4/5] RUN wget -O /home/elastic-apm-agent.jar https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/1.49.0/elastic-apm-agent-1.49.0.jar
#11 CACHED

#12 [basic-logs-mdc-app 5/5] ADD target/example.mdc.jar /home/app.jar
#12 CACHED

#13 [basic-logs-mdc-app] exporting to image
#13 exporting layers done
#13 writing image sha256:0c0bf713c75e772e1b390aa9d8fc0d2dd39b29601774072af8962fdc05c355cc done
#13 naming to docker.io/library/basic-logs-mdc-app done
#13 DONE 0.0s

#5 [basic-logs-mdc-apm-server internal] load metadata for docker.elastic.co/apm/apm-server:8.17.8
#5 DONE 1.1s

#14 [basic-logs-mdc-apm-server 1/9] FROM docker.elastic.co/apm/apm-server:8.17.8@sha256:ce2ea30fb0d2b382ce00388bf9379958d7e451e8998045eedefdbcbb210db873
#14 CACHED

#15 [basic-logs-mdc-apm-server internal] load build context
#15 transferring context: 107B 0.0s done
#15 DONE 0.0s

#16 [basic-logs-mdc-apm-server 2/9] RUN apt-get update && apt -y --fix-broken install
#16 1.025 Get:1 http://security.ubuntu.com/ubuntu jammy-security InRelease [129 kB]
#16 1.029 Get:2 http://archive.ubuntu.com/ubuntu jammy InRelease [270 kB]
#16 1.943 Get:3 http://archive.ubuntu.com/ubuntu jammy-updates InRelease [128 kB]
#16 2.148 Get:4 http://archive.ubuntu.com/ubuntu jammy-backports InRelease [127 kB]
#16 2.299 Err:1 http://security.ubuntu.com/ubuntu jammy-security InRelease
#16 2.300   The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 2.942 Err:2 http://archive.ubuntu.com/ubuntu jammy InRelease
#16 2.943   The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 3.491 Err:3 http://archive.ubuntu.com/ubuntu jammy-updates InRelease
#16 3.493   The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 4.045 Err:4 http://archive.ubuntu.com/ubuntu jammy-backports InRelease
#16 4.047   The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 4.056 Reading package lists...
#16 4.118 W: http://security.ubuntu.com/ubuntu/dists/jammy-security/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2012-cdimage.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.122 W: http://security.ubuntu.com/ubuntu/dists/jammy-security/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2018-archive.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.125 W: GPG error: http://security.ubuntu.com/ubuntu jammy-security InRelease: The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 4.125 E: The repository 'http://security.ubuntu.com/ubuntu jammy-security InRelease' is not signed.
#16 4.125 W: http://archive.ubuntu.com/ubuntu/dists/jammy/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2012-cdimage.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.125 W: http://archive.ubuntu.com/ubuntu/dists/jammy/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2018-archive.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.125 W: GPG error: http://archive.ubuntu.com/ubuntu jammy InRelease: The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 4.127 E: The repository 'http://archive.ubuntu.com/ubuntu jammy InRelease' is not signed.
#16 4.127 W: http://archive.ubuntu.com/ubuntu/dists/jammy-updates/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2012-cdimage.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.127 W: http://archive.ubuntu.com/ubuntu/dists/jammy-updates/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2018-archive.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.127 W: GPG error: http://archive.ubuntu.com/ubuntu jammy-updates InRelease: The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 4.129 E: The repository 'http://archive.ubuntu.com/ubuntu jammy-updates InRelease' is not signed.
#16 4.130 W: http://archive.ubuntu.com/ubuntu/dists/jammy-backports/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2012-cdimage.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.130 W: http://archive.ubuntu.com/ubuntu/dists/jammy-backports/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2018-archive.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.132 W: GPG error: http://archive.ubuntu.com/ubuntu jammy-backports InRelease: The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 4.133 E: The repository 'http://archive.ubuntu.com/ubuntu jammy-backports InRelease' is not signed.
#16 4.134 E: Problem executing scripts APT::Update::Post-Invoke 'rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true'
#16 4.135 E: Sub-process returned an error code
#16 ERROR: executor failed running [/bin/sh -c apt-get update && apt -y --fix-broken install]: runc did not terminate sucessfully
------
 > [basic-logs-mdc-apm-server 2/9] RUN apt-get update && apt -y --fix-broken install:
#16 4.127 W: http://archive.ubuntu.com/ubuntu/dists/jammy-updates/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2012-cdimage.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.127 W: http://archive.ubuntu.com/ubuntu/dists/jammy-updates/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2018-archive.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.127 W: GPG error: http://archive.ubuntu.com/ubuntu jammy-updates InRelease: The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 4.129 E: The repository 'http://archive.ubuntu.com/ubuntu jammy-updates InRelease' is not signed.
#16 4.130 W: http://archive.ubuntu.com/ubuntu/dists/jammy-backports/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2012-cdimage.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.130 W: http://archive.ubuntu.com/ubuntu/dists/jammy-backports/InRelease: The key(s) in the keyring /etc/apt/trusted.gpg.d/ubuntu-keyring-2018-archive.gpg are ignored as the file is not readable by user '_apt' executing apt-key.
#16 4.132 W: GPG error: http://archive.ubuntu.com/ubuntu jammy-backports InRelease: The following signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C
#16 4.133 E: The repository 'http://archive.ubuntu.com/ubuntu jammy-backports InRelease' is not signed.
#16 4.134 E: Problem executing scripts APT::Update::Post-Invoke 'rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true'
#16 4.135 E: Sub-process returned an error code
------

```
