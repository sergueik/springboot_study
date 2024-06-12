### Info
this directory contains 
fork of [gatling Maven plugin](https://github.com/gatling/gatling-maven-plugin-demo-java) Java demo project trimmed to perform parallel data uploads for performance analysis

### Usage

* start caching server in `sqlite_hibernate2`

```sh
cd ..\sqlite_hibernate2
mkdir %USERPROFILE%\sqlite
copy src\main\resources\springboot.db %userprofile%\sqlite
mvn spring-boot:run
```

* execute the gatling test goal
```sh
mvn gatling:test
```

* observe the payload received on double end

```text
...
2024-06-11 19:45:54.391  INFO 4556 --- [           main] s.b.c.e.t.TomcatEmbeddedServletContainer : Tomcat initialized with port(s): 8085 (http)
.TomcatWebServer  : Tomcat started on port(s): 8085 (http) with context path ''
2023-05-09 19:41:42.368  INFO 1804 --- [           main] example.Application                      : Started Application in 5.204 seconds (JVM running for 6.062)
2023-05-09 19:48:56.691  INFO 1804 --- [nio-8085-exec-1] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring DispatcherServlet 'dispatcherServlet'
2023-05-09 19:48:56.697  INFO 1804 --- [nio-8085-exec-1] o.s.web.servlet.DispatcherServlet        : Initializing Servlet 'dispatcherServlet'
2023-05-09 19:48:56.720  INFO 1804 --- [nio-8085-exec-1] o.s.web.servlet.DispatcherServlet        : Completed initialization in 20 ms
Processing temp.txt
test data
Processing temp.txt
test data
...
```

* observe very detailed test run statistics on gatling end
```text
Simulation example.LoadSimulation started...

================================================================================

2023-05-09 19:50:11                                           5s elapsed
---- Requests ------------------------------------------------------------------

> Global                                                   (OK=50     KO=0     )
> Post                                                     (OK=50     KO=0     )
---- Admins --------------------------------------------------------------------
[#####################################-                                    ] 50%
          waiting: 49     / active: 1      / done: 50
================================================================================
===============================================================================
2023-05-09 19:50:16                                          10s elapsed
---- Requests ------------------------------------------------------------------
> Global                                                   (OK=100    KO=0     )
> Post                                                     (OK=100    KO=0     )
---- Admins --------------------------------------------------------------------
[##########################################################################]100%
          waiting: 0      / active: 0      / done: 100
================================================================================

Simulation example.LoadSimulation completed in 10 seconds
Parsing log file(s)...
Parsing log file(s) done
Generating reports...

================================================================================

---- Global Information --------------------------------------------------------

> request count                                        100 (OK=100    KO=0     )
> min response time                                     12 (OK=12     KO=-     )
> max response time                                   1070 (OK=1070   KO=-     )
> mean response time                                    98 (OK=98     KO=-     )
> std deviation                                        240 (OK=240    KO=-     )
> response time 50th percentile                         18 (OK=18     KO=-     )
> response time 75th percentile                         21 (OK=21     KO=-     )
> response time 95th percentile                        772 (OK=772    KO=-     )
> response time 99th percentile                       1039 (OK=1039   KO=-     )
> mean requests/sec                                     10 (OK=10     KO=-     )

---- Response Time Distribution ------------------------------------------------

> t < 800 ms                                            95 ( 95%)
> 800 ms <= t < 1200 ms                                  5 (  5%)
> t >= 1200 ms                                           0 (  0%)
> failed                                                 0 (  0%)
================================================================================


Reports generated in 0s.
Please open the following file: file:///...target/gatling/loadsimulation-20230509235003798/index.html
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  31.566 s
[INFO] Finished at: 2023-05-09T19:50:18-04:00
[INFO] ------------------------------------------------------------------------
```

### See Also

 * [Gatling quickstart tutorial](https://gatling.io/docs/gatling/tutorials/quickstart)
 * [Gatling advanced tutorial](https://gatling.io/docs/gatling/tutorials/advanced)
 * [Gatling Reference](https://gatling.io/docs/gatling/reference/current/http/request/#string-body)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
