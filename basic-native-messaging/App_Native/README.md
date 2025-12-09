### NOTE:

build and run on JDK 1.8 -  in vanilla cmd console window

```cmd
set JAVA_HOME=c:\java\jdk1.8.0_202
c:\java\init.cmd
java -version
```



```text
Java(TM) SE Runtime Environment (build 1.8.0\_202-b08)
Java HotSpot(TM) 64-Bit Server VM (build 25.202-b08, mixed mode)
```
update `pom.xml` to use Java 1.8 and no jakarta /jxp jars.
```cmd
mvn clean package
```
in powershell console, confirm integration test to pass:

```powershell
. .\test2.ps1
```
```text
Response: {"message":"Hello, stranger!"}
```
confirm in logs:
```text

[2025-12-09 01:01:42] INFO  ge.vakho.native_messaging.main.Main - Length prefix bytes: 1E 00 00 00
[2025-12-09 01:01:42] INFO  ge.vakho.native_messaging.main.Main - Payload bytes: 7B 22 6D 65 73 73 61 67 65 22 3A 22 48 65 6C 6C 6F 2C 20 73 74 72 61 6E 67 65 72 21 22 7D
[2025-12-09 01:01:42] INFO  ge.vakho.native_messaging.main.Main - Response JSON {"message":"Hello, stranger!"}
```

```powershell
.\test6.ps1
```
```text
DEBUG: Response (read safely) length = 30
Response: {"message":"Hello, stranger!"}

```

this explains the real root cause: newer Java returns the declared Content-Length before the body is read — even if Content-Length is bogus (0, negative, or absent)

PowerShell  log:
```text
DEBUG: Response (read safely) length = 1314013527
```

…is *not* the actual number of bytes read.
It is simply the value returned by:

✔️ In Java:
```java
connection.getContentLength()
```

or
```java
connection.getHeaderField("Content-Length")
```
✔️ In PowerShell:

You are printing:

`$response.ContentLength`

