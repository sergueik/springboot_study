### Info

Also  possible to put there:
```xml
<proxies>
  <proxy>
    <host>proxy.company.com</host>
    <port>8080</port>
    <nonProxyHosts>localhost|127.0.0.1|*.internal</nonProxyHosts>
  </proxy>
</proxies>
```
or 
```xml
<mirror>
  <id>internal-nexus</id>
  <mirrorOf>*</mirrorOf>
  <url>https://nexus.company.local/repository/maven-public</url>
</mirror>
```
### Background

The `.mvn/settings.xml` is project-local Maven settings file, used instead of (or in addition to):

  * `~/.m2/settings.xml` (user global)
  * `$MAVEN_HOME/conf/settings.xml` (machine global / CI image)

It can define:

  * mirrors
  * proxies
  * authentication
  * pluginRepositories / repositories
  * offline mode
  * profiles

The `.mvn/settings.xml` is stronger
than `repositories`/`pluginRepositories` in the `pom.xml`
because it is part of the build contract, not the environment
and  because the legacy POM repositories are not enough because:
they don’t 

  * fully control plugin resolution
  * control mirrors/proxy behavior
  * control authentication
  * control global repository rewrite rules

So they are incomplete for enterprise builds

That’s why debugging feels like archaeology

* Maven config layers:

|Layer | Scope |
|------|-------|
|`pom.xml`| project dependencies|
|`.mvn/maven.config` | enforced CLI behavior|
|`.mvn/settings.xml`| project build environment|
|`~/.m2/settings.xml`| developer local overrides|
|`$MAVEN_HOME/conf` | machine/CI defaults|


### Revealing

```sh
mvn dependency:resolve -Dverbose
```
```sh
mvn dependency:get -Dartifact=com.example:jrecord-fake:1.0
```

```sh
mvn help:effective-pom
```
