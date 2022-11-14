### Info

This directory includes simplified example code 
from __Improved Java Logging with Mapped Diagnostic Context (MDC)__ [article](https://www.baeldung.com/mdc-in-log4j-2-logback)

- removed the custom `com.baeldung.mdc.TransactionFactory` and left just slf4j logger dependency.
Currently the application is run 

### Usage

```sh
mvn integration-test
```

this will launch the and log the `MDC` mediated information to its console logs:
```text
[INFO] Running example.IntegrationTest
458  [pool-1-thread-3]  INFO e.LogTransferService - Preparing to transfer 1296$. - tx.id=3 tx.owner=Marc
458  [pool-1-thread-2]  INFO e.LogTransferService - Preparing to transfer 1591$. - tx.id=2 tx.owner=Samantha
458  [pool-1-thread-1]  INFO e.LogTransferService - Preparing to transfer 1772$. - tx.id=1 tx.owner=Marc
1056 [pool-1-thread-2]  INFO e.LogTransferService - Has transfer of 1591$ completed successfully ? true. - tx.id=2 tx.owner=Samantha
1056 [pool-1-thread-2]  INFO e.LogTransferService - Preparing to transfer 1362$. - tx.id=4 tx.owner=Marc
1187 [pool-1-thread-1]  INFO e.LogTransferService - Has transfer of 1772$ completed successfully ? true. - tx.id=1 tx.owner=Marc
1187 [pool-1-thread-1]  INFO e.LogTransferService - Preparing to transfer 1093$. - tx.id=5 tx.owner=Susan
1217 [pool-1-thread-3]  INFO e.LogTransferService - Has transfer of 1296$ completed successfully ? false. - tx.id=3 tx.owner=Marc
1217 [pool-1-thread-3]  INFO e.LogTransferService - Preparing to transfer 1925$. - tx.id=6 tx.owner=John
1614 [pool-1-thread-2]  INFO e.LogTransferService - Has transfer of 1362$ completed successfully ? true. - tx.id=4 tx.owner=Marc
1614 [pool-1-thread-2]  INFO e.LogTransferService - Preparing to transfer 1719$. - tx.id=7 tx.owner=John
1745 [pool-1-thread-3]  INFO e.LogTransferService - Has transfer of 1925$ completed successfully ? true. - tx.id=6 tx.owner=John
1745 [pool-1-thread-3]  INFO e.LogTransferService - Preparing to transfer 688$.- tx.id=8 tx.owner=John
2043 [pool-1-thread-1]  INFO e.LogTransferService - Has transfer of 1093$ completed successfully ? false. - tx.id=5 tx.owner=Susan
2043 [pool-1-thread-1]  INFO e.LogTransferService - Preparing to transfer 1866$. - tx.id=9 tx.owner=Susan
2304 [pool-1-thread-3]  INFO e.LogTransferService - Has transfer of 688$ completed successfully ? true. - tx.id=8 tx.owner=John
2304 [pool-1-thread-3]  INFO e.LogTransferService - Preparing to transfer 782$.- tx.id=10 tx.owner=Marc
2579 [pool-1-thread-2]  INFO e.LogTransferService - Has transfer of 1719$ completed successfully ? true. - tx.id=7 tx.owner=John
2772 [pool-1-thread-1]  INFO e.LogTransferService - Has transfer of 1866$ completed successfully ? true. - tx.id=9 tx.owner=Susan
2872 [pool-1-thread-3]  INFO e.LogTransferService - Has transfer of 782$ completed successfully ? true. - tx.id=10 tx.owner=Marc
```
### See Also

   * APM Log Correlation [documetation](https://github.com/elastic/apm-agent-java/blob/main/docs/log-correlation.asciidoc) - note, with APM Agent version 1.30.0 it is enabled by default. The implementation of the `traceid` is introducing `slf4j-api` [dependency](https://mvnrepository.com/artifact/org.slf4j/slf4j-api) 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

