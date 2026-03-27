### Synpthom
* instance comes with 3.5 and has 36 and 3.8 available 
* after upgrading to newer maven can only return version check *presumably done without running java*: 
```sh
mvn -version
```
```text
3.8.5 ... full message
```

```sh
test -f pom.xml ||cat /dev/null > pom.xml
mvn compile
```
expect
```
no target
```

actually see

```text
java.lang.NoClassDefFoundError:  com/google/common/collect/ImmutableList
```
which indicates that a class needed at runtime was present during compilation but cannot be located by the Java Virtual Machine (JVM) class path during execution which in this case points to missing `guava.jar`

#### Tracing (Optional, Complex)

prove the jar in question was linked to the app. NOTE
```
strace -f -e openat mvn -version 2>&1 | grep guava
```
```text
ENOENT /usr/share/java/guava.jar
```

### Technical Steps
```
 which mvn
  readlink /usr/bin/mvn
  readlink /etc/alternatives/mvn
 file /usr/share/maven/bin/mvn
 grep java /usr/share/maven/bin/mvn:
 grep -A 3 JAVACMD /usr/share/maven/bin/mvn


exec "$JAVACMD" \
  $MAVEN_OPTS \
  $MAVEN_DEBUG_OPTS \
  -classpath "${CLASSWORLDS_JAR}" \
  "-Dclassworlds.conf=${MAVEN_HOME}/bin/m2.conf" \
  "-Dmaven.home=${MAVEN_HOME}" \
  "-Dlibrary.jansi.path=${MAVEN_HOME}/lib/jansi-native" \
  "-Dmaven.multiModuleProjectDirectory=${MAVEN_PROJECTBASEDIR}" \
  ${CLASSWORLDS_LAUNCHER} "$@"

CLASSWORLDS_JAR=`echo "${MAVEN_HOME}"/boot/plexus-classworlds-*.jar`
CLASSWORLDS_LAUNCHER=org.codehaus.plexus.classworlds.launcher.Launcher


```
one technically can have one's own `mvn` with a *better* classpath including `guava.jar` and demonstrate it to be runnable
#### Background 
why is needed, when it was added what it solves

* Inventory check
```sh
rpm -ql maven | grep jar
```
or
```
ls -l /usr/share/maven/lib | grep -E 'guava.*\.jar'
```
> NOTE to not be too specific in grep not to miss plain `guava.jar`
In a working stream (3.5), this command output should nnnot be empry

*  side-by-side comparison:
+ Switch to 3.5 (working)
```
ls /usr/share/maven/lib | sort > /tmp/lib-good.txt
```
+ Switch to 3.8 (broken)
```
ls /usr/share/maven/lib | sort > /tmp/lib-bad.txt
```
```
diff /tmp/lib-good.txt /tmp/lib-bad.txt
```

### Rebuilding Maven From Source


Enterprise internet usage strongly discourages anyone from downloading unveted software packages even from trusted sources like Apache.
Building maven from older maven is prone to late errors due to dependncy potentially enforcing newer maven than `3.5.4` (very old, more than two LTS JDK releases old)  


###  Why *Faster Softer Scooter* can work

Maven RPMs on RHEL often package only Maven core jars, and many libraries (like Guava) used to be expected as system dependencies (or brought in via modules).
When you yum/dnf module enable maven:3.8 and install, the RPM may not include Guava because it’s assumed you have a “base system” with certain Java libraries.
Your Windows developer’s .m2/repository already has the exact Guava jar versions that Maven 3.8 expects.
If you copy them into /usr/share/maven/lib (or safer /usr/share/maven/lib/ext), Maven’s classpath sees them immediately.

✅ That’s exactly why your idea works in practice

### Usage

* Enable the 3.8 module and install Maven as usual

* Bulk Add all required jars overwriting the rpm installed directory contents:
```sh
sudo cp /tmp/windows_maven_jars/*.jar /usr/share/maven/lib/
sudo chmod 644 /usr/share/maven/lib/*.jar
```
This Immediately solves the `ClassDefNotFoundError`.

Caveat: __RPM__ doesn’t track these manually added jars; future `dnf upgrade` may overwrite or remove them.

* Safer lternative is to pub  missing guava and all other jars from a Windows `.m2` transferred though `/tmp` into  `lib/ext` which will  and make Maven __3.8__ fully functional, no `ClassDefNotFoundError`:
```sh
sudo mkdir -p /usr/share/maven/lib/ext
sudo cp /mnt/c/Users/Dev/.m2/repository/com/google/guava/guava-*.jar /usr/share/maven/lib/ext/
sudo chmod 644 /usr/share/maven/lib/ext/*.jar
```
