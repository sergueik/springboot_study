### Info

basic Jenkins setup to run Karate tests and report

#### Setup Container
```sh
VERSION=2.322
docker pull jenkins/jenkins:$VERSION-alpine-jdk8
```
```sh
docker run --name jenkins -d -p 8080:8080 jenkins/jenkins:$VERSION-alpine-jdk8
```
```sh
docker logs jenkins
```
this will show
```text
2023-02-17 14:46:02.126+0000 [id=27]    INFO    jenkins.install.SetupWizard#init:

*************************************************************

Jenkins initial setup is required. An admin user has been created and a password generated.
Please use the following password to proceed to installation:

b94cca68f35540ef90e096fdd8d02e4a

This may also be found at: /var/jenkins_home/secrets/initialAdminPassword

*************************************************************
```
Navigate to `http://localhost:8080` which will redirect to `http://192.168.0.64:8080/login?from=%2F` for *Unlock Jenkins* page

After configuring the admin user (usually `admin`/`password`)  and selecting and instaling the minimal number of plugins to install may stop the container

need to keep the plugins
  * Workspace Cleanup
  * Gradle
  * Pipeline
  * Git

NOTE: if starting with Docker image hosting an older Jenkins war release e.g. __2.308__ Jenkins will fail to download some (most) of plugins and the error will be:
```text
 - Jenkins (2.319.1) or higher required
```
with the version vary. The earlier supported version is __2.321__

#### Install Software
To add software and configurations connect to container as root user
```sh
docker exec -u root -it jenkins sh
 ```
```sh
apk add maven gradle git
```

* add mvn and gradle configuration and tool locations to Jenkins Tool through `http://192.168.0.92:8080/manage`

configuration details:

  * `JDK_HOME`: `opt/java/openjdk``
  * `GRADLE_HOME`: `/usr/share/java/gradle`
  * `MAVEN_HOME`: `/usr/share/java/maven-3`
  * Clear "install automatically" check boxes 
and set Default settings provider  to `/usr/share/jenkins/settings.xml`


![configuration](https://github.com/sergueik/springboot_study/blob/master/basic-jenkins/screenshots/capture-tool-configuration.png)

and optionally create an empty project directly on container to avoiddeealing with git at this step

```sh
mkdir ~jenkins/karate-collector
mkdir -p ~jenkins/karate-collector/src/test/java/example
chown -R jenkins:jenkins  ~jenkins/karate-collector
exit
```
  
#### Copy Project

```sh
docker cp ../basic-karate-collector/build.gradle jenkins:/var/jenkins_home/karate-collector
docker cp ../basic-karate-collector/pom.xml jenkins:/var/jenkins_home/karate-collector
docker cp ../basic-karate-collector/src/test/java/example/ jenkins:/var/jenkins_home/karate-collector/src/test/java
```
#### Create and Run a Basic Project

```sh
mvn -verison
gradle -version
cp -R ~/karate-collector/* .
mvn clean test
gradle clean test
```
after the build measure repository cache directories
```sh
find ~/.m2 -type f |wc
```
```text
1339    1339  133615
```
```sh
find ~/.gradle/ -type f |wc
```
```text
468     468   66530
```
```sh
du -sh ~/.gradle
```
```text
220M    /var/jenkins_home/.gradle
```
```sh
/ $ du -sh ~/.m2
```
```text
118M    /var/jenkins_home/.m2
```
#### Artifacts
in the minimal configuration add post-build action to archive Karate summary JSON artifacts and delete the workspace.

![build](https://github.com/sergueik/springboot_study/blob/master/basic-jenkins/screenshots/capture-build.png)

can also find them in the workspace in the container (handy when additional artifacts are intended to be collected)
```sh
find / -iname 'karate*txt'
```
```text
/var/jenkins_home/workspace/test/build/karate-reports/karate-summary-json.txt
/var/jenkins_home/workspace/test/target/karate-reports/karate-summary-json.txt
```

#### Timing


 * On a fast host, clean builds take a little over 1 minute. On a network and disk poor host the clean builds take over 6 minute each. To force clean build, simply move the cache repositories
```sh
mv ~/.m2 ~/.m2.MOVED
mv ~/.gradle/ ~/.gradle.MOVED
```

 * incremental builds take around 10 sec on a fast machine and around 30 sec on slow one.

The test suite contains Just one feature and one test within.
This clean to incremental speed ratio

#### Connect Two Containers 

Connect Two Containers  which were not launched with `--link` option through ip addreses

* start the container with non conflicting port
```sh
IMAGE=basic-perl-apache
NAME=basic-perl-cgi
docker run -d -p 9090:80 -p 9443:443 --name $NAME $IMAGE
docker logs -f $NAME
```
```sh
docker exec -it $NAME sh
```
```sh
ifconfig eth0
```
```text
eth0  Link encap:Ethernet  HWaddr 02:42:AC:11:00:02
      inet addr:172.17.0.2  Bcast:172.17.255.255  Mask:255.255.0.0
      UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1
      RX packets:25 errors:0 dropped:0 overruns:0 frame:0
      TX packets:0 errors:0 dropped:0 overruns:0 carrier:0
      collisions:0 txqueuelen:0
      RX bytes:3054 (2.9 KiB)  TX bytes:0 (0.0 B
```          

```sh
NAME=jenkins
```
```sh
docker start $NAME
docker exec -u root -it $NAME sh
```

```sh
ping 172.17.0.2
```

```text
PING 172.17.0.2 (172.17.0.2): 56 data bytes
64 bytes from 172.17.0.2: seq=0 ttl=64 time=0.247 ms
64 bytes from 172.17.0.2: seq=1 ttl=64 time=0.143 ms
```
NOTE user argument - will fail without since the default user on Jenkins is `jenkins`.

* run the curl command from `basic-perl-cgi` repo (use the actual IP address of the receiving container): 
```sh

echo Mem:        1863756      665300      157580> data.txt 
echo Swap:       2720764       24832     2695932>> data.txt 
echo load_average: 0.16 0.08 0.08 1/460 32100>> data.txt 
echo rpm: 104>> data.txt 
echo date: Sun Jun 26 22:08:31 EDT 2022>> data.txt 
echo computer: lenovo120S.private.org>> data.txt 
echo uptime: 18:56:03 up 1 day,  3:44,  3 users,  load average: 0.07, 0.10, 0.09>> data.txt 
echo disk: /dev/sda1 27G 22G 3.6G 86% />> data.txt 
echo foo: 1>> data.txt
curl -F "data=@$(pwd)/data.txt" -X POST "http://172.17.0.2:80/cgi-bin/upload.cgi?type=send&new=1"
```
this will produce a happy response

```html
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style type="text/css">
img {border: none;}
</style>
</head>
<body>
<p>Thanks for uploading data</p>
<p><img src="/upload/data.txt" alt="data" /></p>
</body>
</html>

```
Update the build job, using same code in shell script..
create data.txt in `karate-collector` in `jenkins` user home directory  in the container: 
there is some challenge with using output shell redirection in the shell step in Jenkins.

```sh
docker exec -it jenkins sh
```

save sample data:
```text
Mem: 1863756 665300 157580
Swap: 2720764 24832 2695932
load_average: 0.16 0.08 0.08 1/460 32100
rpm: 104
date: Sun Jun 26 22:08:31 EDT 2022
computer: lenovo120S.private.org
uptime: 18:56:03 up 1 day, 3:44, 3 users, load average: 0.07, 0.10, 0.09
disk: /dev/sda1 27G 22G 3.6G 86% /
foo: 0

```
NOTE: shell command sensitive to space  in front of `>`: 
```sh
cd ~/karate-collector
echo Mem:        1863756      665300      157580 > data.txt
echo Swap:       2720764       24832     2695932 >> data.txt
echo load_average: 0.16 0.08 0.08 1/460 32100 >> data.txt
echo rpm: 104 >> data.txt
echo date: Sun Jun 26 22:08:31 EDT 2022 >> data.txt
echo computer: lenovo120S.private.org >> data.txt
echo uptime: 18:56:03 up 1 day,  3:44,  3 users,  load average: 0.07, 0.10, 0.09 >> data.txt
echo disk: /dev/sda1 27G 22G 3.6G 86% />> data.txt
echo foo: 0 >> data.txt
```


in Jenkins shell step, replace `foo: 0` with `foo: 42` and send file:
```sh
ls data.txt
sed -i 's|foo: .*|foo: 42|' data.txt
curl -F "data=@./data.txt" -X POST "http://172.17.0.2:80/cgi-bin/upload.cgi?type=send&new=1"
```
after the job completes find the expected value in the STDERR output of the Perl script 

```Perl
print STDERR Dumper(read_data($query->upload('data')->{content}), $/;
```			

in `/var/log/apache2/error.log`:
```text
$VAR1 = {
 'rpm' => '104',
 'swap' => '2720764 24832 2695932',
 'uptime' => '18:56:03 up 1 day, 3:44, 3 users, load a'
 'foo' => '42',
 'load_average' => '0.16 0.08 0.08 1/460 32100',
 'computer' => 'lenovo120S.private.org',
 'date' => 'Sun Jun 26 22:08:31 EDT 2022',
 'mem' => '1863756 665300 157580',
 'disk' => '/dev/sda1 27G 22G 3.6G 86% /'
};
```


### See Also
   * [configure the remote Maven repository in Jenkins project jobs](https://docs.cloudbees.com/docs/cloudbees-ci-kb/latest/client-and-managed-masters/configure-the-remote-maven-repository-in-jenkins-project-jobs)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
