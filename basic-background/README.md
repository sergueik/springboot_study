### Info

this directory contains code based on [java Concurrent Collection - ConcurrentHashMap Examples](https://www.codejava.net/java-core/concurrency/java-concurrent-collection-concurrenthashmap-examples) combined into a generic Springboot app to represent caching database proxy server. The reader service threads replaced with endpoint

### Usage

```sh
mvn spring-boot:run
```
this will start two backgrond worker threads randomly updating a hash with thread-specific values:
```text
1694205246221: Writer-2 has removed [4 => Writer-2]
1694205251222: Writer-2 has put [2 => Writer-2]
1694205256224: Writer-2 has put [3 => Writer-2]
1694205256226: Writer-2 has removed [2 => Writer-2]
1694205266213: Writer-1 has removed [6 => Writer-1]
1694205266228: Writer-2 has removed [0 => Writer-2]
1694205271214: Writer-1 has removed [5 => Writer-1]
1694205271229: Writer-2 has put [0 => Writer-2]
1694205281216: Writer-1 has put [5 => Writer-1]
...
```

the state of the can be queried via
```sh

curl -s http://localhost:8085/basic/
```

This will print the hash:
```text
1694205344283: 0=>Writer-1; 4=>Writer-2; 6=>Writer-1; 8=>Writer-2; 
```
```text
1694205372136: 0=>Writer-1; 1=>Writer-2; 2=>Writer-1; 4=>Writer-2; 5=>Writer-1; 9=>Writer-1;
```

### TODO


* after switching to repository, application fails in the runtime

