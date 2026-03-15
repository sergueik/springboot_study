### Info
Tiny HTTP server in pure Java for [layered Dockerfile](https://www.baeldung.com/docker-layers-spring-boot) unpublished snapshot dependency.  

### Usage

* put snapshot dependency in custom location

```sh
pushd commandline-parser
mkdir %TEMP%/repo
mvn clean deploy -DaltDeploymentRepository=local::default::file:%TEMP%/repo
popd
```
* observe a number of messages like below

```text
Uploaded to local: file:C:\Users\kouzm\AppData\Local\Temp/repo/example/commandline-parser/0.12.1-SNAPSHOT/commandline-parser-0.12.1-20260315.204541-1.jar (26 kB at 980 kB/s)
Uploading to local: file:C:\Users\kouzm\AppData\Local\Temp/repo/example/commandline-parser/0.12.1-SNAPSHOT/commandline-parser-0.12.1-20260315.204541-1.pom
Uploaded to local: file:C:\Users\kouzm\AppData\Local\Temp/repo/example/commandline-parser/0.12.1-SNAPSHOT/commandline-parser-0.12.1-20260315.204541-1.pom (6.5 kB at 1.3 MB/s)
Downloading from local: file:C:\Users\kouzm\AppData\Local\Temp/repo/example/commandline-parser/maven-metadata.xml
Uploading to local: file:C:\Users\kouzm\AppData\Local\Temp/repo/example/commandline-parser/0.12.1-SNAPSHOT/maven-metadata.xml
Uploaded to local: file:C:\Users\kouzm\AppData\Local\Temp/repo/example/commandline-parser/0.12.1-SNAPSHOT/maven-metadata.xml (778 B at 18 kB/s)
Uploading to local: file:C:\Users\kouzm\AppData\Local\Temp/repo/example/commandline-parser/maven-metadata.xml
Uploaded to local: file:C:\Users\kouzm\AppData\Local\Temp/repo/example/commandline-parser/maven-metadata.xml (286 B at 9.9 kB/s)
```

 * spin http server serving the repository



```sh
push basic-httpserver
mvn package
java -cp  target\example.httpserver.jar;target\lib\* example.App -port 8081 -root %temp%\repo
popd
```

 * add the repository to subject project `pom.xml` `<repositories>`:

```xml
    <repository>
      <id>local-repository</id>
      <name>not really a repo</name>
      <url>https://localhost:8081</url>
    </repository>
```

 * pin and add the dependency to subject project `pom.xml` `<dependencies>`:

```xml
    <commandline-parser.version>0.12.1-SNAPSHOT</commandline-parser.version>
```

```xml
     <groupId>example</groupId>
      <artifactId>commandline-parser</artifactId>
      <version>${commandline-parser.version}</version>
      <exclusions>
        <exclusion>
          <groupId>com.google.guava</groupId>
          <artifactId>guava</artifactId>
        </exclusion>
        <exclusion>
          <groupId>org.json</groupId>
          <artifactId>json</artifactId>
        </exclusion>
        <exclusion>
          <groupId>com.google.code.gson</groupId>
          <artifactId>gson</artifactId>
        </exclusion>
      </exclusions>
    </dependency>
```

 > NOTE: do not really need to use the API from dependency 

 * make sure the `commandline-parser` of the desired version is not cached in `.m2`

```xml
pushd \~/.m2/repository/example/
rm -fr commandline-parser/0.12.1-SNAPSHOT
popd
```

`````
INFO] --------------------------< example:layered >---------------------------
[INFO] Building example:layered 0.2.0-SNAPSHOT
[INFO]   from pom.xml
[INFO] --------------------------------\[ jar ]---------------------------------
Downloading from local-repository: https://localhost:8081/example/commandline-parser/0.12.0-SNAPSHOT/maven-metadata.xm
...
Downloaded from local-repository: http://localhost:8081/example/commandline-parser/0.12.1-SNAPSHOT/commandline-parser-0.12.1-20260315.204541-1.jar (26 kB at 1.6 MB/s)
```

### Docker Toolbox

Try to add to hostonly-repository with hostname: `192.168.99.1` in '<repository>`

```xml
  <repository>
      <id>hostonly-repository</id>
      <name>not really a repo</name>
      <!-- NOTE: not https -->
      <url>http://192.168.99.1:8081</url>
        <releases>
        <enabled>true</enabled>
      </releases>
      <snapshots>
        <enabled>true</enabled>
      </snapshots>
      <layout>default</layout>
    </repository>
```
package the app with external dependency
```sh
mvn -Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true -U clean package
```

but if it errors:

```text
[ERROR] Failed to execute goal on project layered: Could not resolve dependencies for project example:layered:jar:0.3.0-SNAPSHOT: 
Failed to collect dependencies at example:commandline-parser:jar:0.12.1-SNAPSHOT: 
Failed to read artifact descriptor for example:commandline-parser:jar:0.12.1-SNAPSHOT: 
Could not transfer artifact example:commandline-parser:pom:0.12.1-SNAPSHOT from/to maven-default-http-blocker (http://0.0.0.0/): 
Blocked mirror for repositories: [hostonly-repository (http://192.168.99.1:8081, default, releases+snapshots)]
```



the only canonical option is through tweaking `~/.m2/settings.xml` in certain way: 

```xml

<settings>
  <mirrors>
    <mirror>
      <id>allow-http</id>
      <mirrorOf>*</mirrorOf>
      <url>http://192.168.99.1:8081</url>
      <blocked>false</blocked>
    </mirror>
  </mirrors>
 <profiles>
    <profile>
      <id>allow-insecure-http</id>
      <repositories>
        <repository>
          <id>hostonly-repository</id>
          <url>http://192.168.99.1:8081</url>
          <releases><enabled>true</enabled></releases>
          <snapshots><enabled>true</enabled></snapshots>
        </repository>
      </repositories>
    </profile>
  </profiles>
  <activeProfiles>
    <activeProfile>allow-insecure-http</activeProfile>
  </activeProfiles>
</settings>
```

### Background Information

During the experimentation with a tiny local HTTP server to serve an unpublished SNAPSHOT dependency for a layered Spring Boot Docker build, several key points were observed:

1. **Local SNAPSHOT deployment**
   - The `commandline-parser` project was deployed to a temporary file-based Maven repository using:
   ```sh
   mvn clean deploy -DaltDeploymentRepository=local::default::file:%TEMP%/repo
   ```
   - This created a repository layout with `.jar`, `.pom`, and `maven-metadata.xml` files.

2. **Tiny HTTP server in Java**
   - A minimal Java HTTP server (`com.sun.net.httpserver.HttpServer`) was used to serve the repository on the host at port 8081.
   - From the host machine, accessing the repository via `http://localhost:8081` worked as expected, with successful artifact downloads.

3. **Maven HTTP blocker behavior (3.8+)**
   - Maven version 3.8 and later enforces the **`maven-default-http-blocker`**, which blocks plain HTTP repositories by default to encourage HTTPS usage.
   - Attempting to access the repository using any hostname other than `localhost` (e.g., `http://192.168.99.1:8081`) results in a blocked mirror error:
   ```
   Blocked mirror for repositories: [hostonly-repository (http://192.168.99.1:8081, default, releases+snapshots)]
   ```
   - This is a policy issue in Maven, not a network connectivity problem. The host-only gateway IP (`192.168.99.1`) is reachable from Docker Machine containers, as confirmed with `nc` tests.

4. **Workarounds for local experimentation**
   - For host-side builds, using `http://localhost:8081` is the simplest approach.
   - To allow HTTP on a non-localhost repository, Maven can be configured via `~/.m2/settings.xml`:
     - Create a mirror with `<blocked>false</blocked>`
     - Define a profile that enables releases and snapshots from the repository
     - Activate the profile globally
   - Quick command-line flags are also possible for temporary bypass:
     ```sh
     mvn -Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true -U clean package
     ```

5. **Docker Machine / host-only networking**
   - In a Docker Toolbox / Docker Machine setup, `host.docker.internal` is **not automatically available**.
   - Containers can access services on the Windows host via the host-only gateway IP (usually `192.168.99.1`), but Maven may block HTTP requests to that IP unless configured explicitly.

6. **Key learning points**
   - Maven 3.8+ enforces HTTPS and will block otherwise reachable HTTP repositories.
   - Host-only networking via Docker Machine allows containers to reach host services, but special handling is required to avoid Maven HTTP-blocker errors.
   - Using `localhost` on the host is the most straightforward approach for local SNAPSHOT experiments, avoiding the need for advanced Maven settings tweaks.