### Info
this builds https://github.com/nidi3/graphviz-java/blob/master/README.md#user-content-how-it-works pruned from native dependencies
### Usage
```sh
mvn package
java -cp target\example.graphviz-java.jar;target\lib\* example.Example -inputfile color.dot -outputfile example.png
```
> NOTE: can use Unix path separaror, but still have to use Windows slement separator (`;` instead of `:`)
```cmd
docker pull eclipse-temurin:11-jre-alpine

docker build -t example-graphviz-java -f Dockefile . 
docker pull eclipse-temurin:11-jre-alpine
```
```sh
docker build -t example-graphviz-java -f Dockefile .
```
```sh
docker run --rm example-graphviz-java -help
```
```text
Usage: jar -outputFile <filename> -inputfile <filename>
```
```sh
docker run --rm -w $(pwd):/app example-graphviz-java -inputfile example.dot -outputfile example.png
```
```text
03:21:33.782 INFO  example.Example - converting example.dot example.png
```
### See Also
  * https://github.com/omerio/graphviz-webapp
  * https://hub.docker.com/r/mejran/graphviz-server
  * https://github.com/blackears/svgSalamander SVG engine for Java
  * https://mvnrepository.com/artifact/guru.nidi/graphviz-java/0.18.1
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
