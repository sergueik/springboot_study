### Info

Attempt to exercise old examples of signal handling in Java. Currently does not work.

### Usage

```sh
mvn package
java -Xrs -cp target/example-signal-handler-0.1.0-SNAPSHOT.jar   example.App
```
```sh
ps ax | grep example.Ap[p] | awk '{print $1}' |xargs -IX kill -12 X
```

### See Also

  * https://ringlord.com/dl/Signals-in-Java.pdf
  * https://kostenko.org/blog/2019/08/java-catch-os-signals.html
  * https://www.ibm.com/docs/en/ztpf/2019?topic=signals-used-by-jvm
  * https://m.cafe.daum.net/sepro/5CO5/100?listURI=%2Fsepro%2F_rece
