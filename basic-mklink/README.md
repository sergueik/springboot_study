### Info

this directory contains notes about using symlink for spring boot jar onWindows

### Usage
```cmd
mklink example.jar example.basic-tests.jar
```
NOTE: this requires elevation
```text
You do not have sufficient privilege to perform this operation.
```
In elevated  prompt will get success message
```text
symbolic link created for example.jar <<===>> example.basic-tests.jar
```
```cmd
cd /d c:\temp
dir /x .
```
```
 Directory of c:\temp

11/24/2023  09:38 AM    <SYMLINK> example.jar [example.basic-tests.jar]
```

```powershell
remove-item .\b.txt
New-Item -ItemType SymbolicLink -Path "b.txt" -value "a.txt"
```
```powershell
cmd %% /c dir /x .
```
```text
 Directory of c:\temp

24.11.2023  17:51    <DIR>                       .
24.11.2023  17:51    <DIR>                       ..
24.11.2023  17:51                 6              a.txt
24.11.2023  17:51    <SYMLINK>                   b.txt [C:\temp\a.txt]
```

NOTE: on Windows version older than 10 this command will fail with the error

```text
New-Item : The type is not a known type for the file system. Only "file" and "directory" can be specified.
```
### See Also:

  * [symbolic link creation in Windows 10 with PowerShell](https://winaero.com/create-symbolic-link-windows-10-powershell/) - NOTE: advises the wrong `target` option name, should be `value`

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
