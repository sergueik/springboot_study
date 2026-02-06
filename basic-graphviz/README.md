### Info
this builds https://github.com/nidi3/graphviz-java/blob/master/README.md#user-content-how-it-works pruned from native dependencies
### Usage
```sh
mvn clean package
```
review the fat jar:
```sh
unzip -ql target/example.graphviz-java.jar 
```
test the app on the developer machine
```sh
java -jar target/example.graphviz-java.jar -inputfile color.dot -outputfile example.png
```
> NOTE: when testing on Windows can use Unix path separaror, but still have to use Windows slement separator (`;` instead of `:`)
```cmd
docker pull eclipse-temurin:11-jre-alpine
export NAME=example-graphviz-java
docker build -t $NAME -f Dockerfile . 
```
```sh
docker run --rm $NAME -help
```
```text
Usage: jar -outputFile <filename> -inputfile <filename>
```
```sh
docker run --rm -w $(pwd):/app $NAME -inputfile example.dot -outputfile example.png
```
```text
03:47:15.860 INFO  example.Example - converting example.dot example.png
```
### See Also
  * https://github.com/omerio/graphviz-webapp
  * https://hub.docker.com/r/mejran/graphviz-server
  * https://github.com/blackears/svgSalamander SVG engine for Java
  * https://mvnrepository.com/artifact/guru.nidi/graphviz-java/0.18.1
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
