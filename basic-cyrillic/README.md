### Info


This directory contains example from [how to make cyrillic console output right](http://www.skipy.ru/technics/encodings_console_comp.html)(in Russian). Setting locale page turns out quite painful


### Usage
on Linux
```sh
mvn package
```
then 
```sh
java -jar target/example.cyrillic.jar
```
```text
АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ абвгдеёжзийклмнопрстцуфхцчшщъыьэюя 12345
```

```sh
mvn test
```
```text
Set system output encoding: utf-8
АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ абвгдеёжзийклмнопрстцуфхцчшщъыьэюя 12345
```
on Windows 

```sh
mvn -f pom.windows.xml package
```

```sh
C:\Windows\System32\chcp.com 65001
java  -DconsoleEncoding=utf-8 -jar target/example.cyrillic.jar
```
```text
Active code page: 65001
Set system output encoding: utf-8
АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ абвгдеёжзийклмнопрстцуфхцчшщъыьэюя 12345
```
```cmd
C:\Windows\System32\chcp.com 866
```
```text
Active code page: 866
```

```cmd
java.exe -DconsoleEncoding=866 -jar target\example.cyrillic.jar
```
```text
АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ абвгдеёжзийклмнопрстцуфхцчшщъыьэюя 12345

```

```cmd
C:\Windows\System32\chcp.com 65000
```
```text
Active code page: 65000
```

```cmd
java.exe -DconsoleEncoding=utf16 -jar target\example.cyrillic.jar
```

```text
þÿ                                     ! " # $ % & ' ( ) *  , - . /   0 1 2 3 4
  Q 6 7 8 9 : ; < = > ? @ A B F C D E F G H I J K L M N O   1 2 3 4 55
```
```cmd
C:\Windows\System32\chcp.com 65001
```
```text
Active code page: 65001
```
```
java.exe -DconsoleEncoding=65001 -jar target\example.cyrillic.jar
```
```
Unsupported encoding set for console: 65001
???????????????????????????????????????????????????????????????????
```

```cmd
Active code page: 65001
java.exe -DconsoleEncoding=UTF-8 -jar target\example.cyrillic.jar
```

```text
АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ абвгдеёжзийклмнопрстцуфхцчшщъыьэюя 12345�деёжзийклмнопрстцуфхцчшщъыьэюя 12345фхцчшщъыьэюя 12345эюя 12345345
```

* NOTE the defects (some of cyrillic character have effect of cusror keys?)

```cmd
mvn -f pom.windows.xml test
```
```text
[INFO] Running example.ConsoleTest
Çüéâäà≡åçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒ áíóúñÑ±ªº¿⌐¬½¼¡«»αßΓµπΣσµτΦΘΩδ∞φε∩ 12345
```

```cmd
C:\Windows\System32\chcp.com 866
mvn -f pom.windows.xml test
```
```text
Active code page: 866
[INFO] Running example.ConsoleTest
АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ абвгдеёжзийклмнопрстцуфхцчшщъыьэюя 12345
```
replace in `pom.xml`
```xml
 <	866</consoleEncoding>
```
with
```xml
 <consoleEncoding>utf-8</consoleEncoding>
```
```cmd
C:\Windows\System32\chcp.com 65001
mvn -f pom.windows.xml test
```
```text
Active code page: 65001
Set system output encoding: utf-8
????????????????????????????????? ?????????????????????????????????? 12345
```

### See Also

   * [how to fix Jenkins Console Log Encoding Issue on Windows](https://pacroy.com/how-to-fix-jenkins-console-log-encoding-issue-on-windows-a1f4b26e0db4)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

