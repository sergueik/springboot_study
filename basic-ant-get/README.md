### Info

use `ant-runner` Maven [plugin](https://maven.apache.org/plugins/maven-antrun-plugin/) to run Ant tasks for Maven in `` stage with Ant __Get__ [task])https://ant.apache.org/manual/Tasks/get.html) to download the specific ependency

Use Version __1.5__ of `jOpenDocument-1.5.jar` [download location](https://www.jopendocument.org/download/jOpenDocument-1.5.jar) to use the version which is not available in Maven Central
### Usage
```sh
mvn test
```

will print
```text
[WARNING] Parameter tasks is deprecated, use target instead
[INFO] Executing tasks

main:
      [echo] downloads.dir = ...\basic-ant-get\src\main\resources\downloads
      [get] Getting: https://www.jopendocument.org/download/jOpenDocument-1.5.jar
      [get] To: ...\basic-ant-get\src\main\resources\downloads\jOpenDocument-1.5.jar
[INFO] Executed tasks
[INFO]
[INFO] --- maven-resources-plugin:2.6:resources (default-resources) @ antrun-dependency-download ---
[INFO] Using 'UTF-8' encoding to copy filtered resources.
[INFO] Copying 1 resource
[INFO]
[INFO] --- maven-compiler-plugin:3.6.1:compile (default-compile) @ antrun-dependency-download ---
[INFO] Nothing to compile - all classes are up to date
[INFO]
[INFO] --- maven-resources-plugin:2.6:testResources (default-testResources) @ antrun-dependency-download ---
[INFO] Using 'UTF-8' encoding to copy filtered resources.
[INFO] Copying 1 resource
[INFO]
[INFO] --- maven-compiler-plugin:3.6.1:testCompile (default-testCompile) @ antrun-dependency-download ---
[INFO] Nothing to compile - all classes are up to date
[INFO]
[INFO] --- maven-surefire-plugin:2.20:test (default-test) @ antrun-dependency-download ---
[INFO]
[INFO] -------------------------------------------------------
[INFO]  T E S T S
[INFO] -------------------------------------------------------
[INFO] Running example.ODSTest
$Version: [Namespace: prefix "manifest" is mapped to URI "urn:oasis:names:tc:open
document:xmlns:manifest:1.0"] org.jopendocument.dom null
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 0.736 s - in example.ODSTest
[INFO]
[INFO] Results:
[INFO]
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
[INFO]
```
### NOTE:
the source code [downloads](https://www.jopendocument.org/download.html) is GPL. There appears no github repositiory by original vendor.
 
the version 3.x of `ant-run` plugin no longer accepts `tasks`:
```text
[ERROR] Failed to execute goal org.apache.maven.plugins:maven-antrun-plugin:3.0.
0:run (default) on project antrun-dependency-download: You are using 'tasks' whi
ch has been removed from the maven-antrun-plugin. Please use 'target' and refer
to the >>Major Version Upgrade to version 3.0.0<< on the plugin site. -> [Help 1
]
```
### See Also

  * https://stackoverflow.com/questions/64423111/javajopendocument-nullpointerexception-when-using-getcellat0-0
  * https://www.javaxt.com/Tutorials/Jar/How_to_Get_the_Version_Number_of_a_JAR_File
  * alternative OpenDocument files translating to HTML engine [repository](https://github.com/andiwand/OpenDocument.java)
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
