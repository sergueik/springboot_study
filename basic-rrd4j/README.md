### Info

This contains replica of [rrd4j/rrd4j](https://github.com/rrd4j/rrd4j)

### Usage

* to avoid being stopped by failing test and debigging build clean errors,

```sh
biz.aQute.bnd:bnd-maven-plugin:5.2.0:bnd-process (bnd-process) on project rrd4j: Classes found in the wrong directory: {META-INF/target/classes/org/rrd4j/core/Util.class=org.rrd4j.core.Util... followed by long list of clases
```

build via
```sh
mvn -Dmaven.test.skip=true clean package
```
* invoke
```cmd
java -Xmx4096m -jar target/rrd4j-3.9-SNAPSHOT-inspector.jar
```
Note the heap setting to avoid out of memory erorr in runtime if tested on am emory-constrained VM (adjust as available RAM permits)

NOTE: on Windows need to first
```cmd
mkdir \tmp
```
* there is a lot of sample `rrd` files in the project.


### Suppport RRDTool `.rrd` files

As suggested on [stackoverflow](https://community.openhab.org/t/rrd4j-and-tools-to-read-rrd-files/4655/4)  one can downgrade to version __2.1.1__ to gain support of regular RRDtool files.

*fetch
```sh
mkdir upstream
cd upstream/
git clone https://github.com/rrd4j/rrd4j
cd rrd4j/
git fetch https://github.com/rrd4j/rrd4j
git checkout -B 2.2.1 origin/2.2.1
```
modify the `pom.xml` to remove javadoc plugins.
* build
```sh
mvn -Dmaven.test.skip=true clean package
```
* invoke
```cmd
java -Xmx4096m -jar target/rrd4j-2.2.1-inspector.jar
```

Does not clear the exception
```text
java.io.IOException: Invalid file header. File [some.rrd] is not a RRD4J RRD file
```

- kept for later examination
### See Also

  * https://github.com/OpenNMS/jrrd - mixed Java / C wrapper
  * [RRDtool Tutorial - Part 1](https://www.youtube.com/watch?v=JaK-IctEyWs)
  * [RRDtool Tutorial - Part 2](https://www.youtube.com/watch?v=m_qeVVB2yzw)
  * https://github.com/sysmo-nms/rrdio
  * https://oznetnerd.com/2018/04/17/writing-a-grafana-backend-using-the-simple-json-datasource-flask/
  * probably moves data the other direction https://github.com/nitinka/JMetrics
