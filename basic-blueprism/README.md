# Blue Prism Learning Edition: The Runtime Dependency Saga

## Objective

Install Blue Prism Learning Edition on a clean Windows environment and document installation issues, investigation steps, evidence collected, and recovery.

---

# Environment

Product:
- Blue Prism Learning Edition

Target:
- Windows 11 clean installation

Database component:
- SQL Server LocalDB 16.0.1000.6

---

# Initial symptom

Blue Prism installation completed, but database configuration failed.

Windows Event Viewer:

Provider:
```
Application Popup
```

Event:
```
Event ID 26
```

Message:

```
sqlservr.exe - System Error

The code execution cannot proceed because VCRUNTIME140_1.dll was not found.
Reinstalling the program may fix this problem.
```

---

# Initial investigation

SQL Server executable existed:

```
C:\Program Files\Microsoft SQL Server\160\LocalDB\Binn\sqlservr.exe
```

Version:

```
SQL Server LocalDB 16.0.1000.6
```

The missing runtime file:

```
C:\Windows\System32\VCRUNTIME140_1.dll
```

was not present initially.

---

# Visual C++ Redistributable investigation

Reference:

https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist?view=msvc-170

After installing Microsoft Visual C++ 2015-2022 Redistributable:

Verification:

```
Get-ItemProperty HKLM:\SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64 |
Select-Object Installed, Version, Major, Minor, Bld
```

Result:

```
Installed : 1
Version   : v14.51.36247.00
Major     : 14
Minor     : 51
Bld       : 36247
```

---

x64 registry under WOW6432Node:

```
Get-ItemProperty HKLM:\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\x64 |
Select-Object Installed, Version, Major, Minor, Bld
```

Result:

```
Installed : 1
Version   : v14.51.36247.00
Major     : 14
Minor     : 51
Bld       : 36247
```

---

x86 registry:

```
Get-ItemProperty HKLM:\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\x86 |
Select-Object Installed, Version, Major, Minor, Bld
```

Result:

```
Installed : 1
Version   : v14.51.36247.00
Major     : 14
Minor     : 51
Bld       : 36247
```

---

DLL verification:

```
where.exe /r C:\Windows vcruntime140_1.dll
```

Result:

```
C:\Windows\System32\vcruntime140_1.dll
```

File:

```
C:\Windows\System32\vcruntime140_1.dll

05/27/2026 04:59 AM
50,112 bytes
```

---

# LocalDB investigation

Added SQL LocalDB binary path:

```
path=%path%;"c:\Program Files\Microsoft SQL Server\160\LocalDB\Binn"
```

List instances:

```
sqllocaldb.exe info
```

Result:

```
BluePrismLocalDB
MSSQLLocalDB
```

---

Details:

```
sqllocaldb.exe info BluePrismLocalDB
```

Result:

```
Name:               BluePrismLocalDB
Version:            16.0.1000.6
Shared name:
Owner:              DESKTOP-RNOG166\sergueik
Auto-create:        No
State:              Stopped
Instance pipe name:
```

---

Attempt:

```
sqllocaldb.exe start BluePrismLocalDB
```

Result:

```
Start of LocalDB instance "BluePrismLocalDB" failed because of the following error:

Error occurred during LocalDB instance startup:
SQL Server process failed to start.
```

---

# LocalDB files

Location:

```
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB
```

Contents:

```
error.log
error1.log
error2.log
error3.log
error4.log
error5.log
error6.log

master.mdf
mastlog.ldf

model.mdf
modellog.ldf

msdbdata.mdf
msdblog.ldf
```

---

# SQL Server error.log

Command:

```
pushd "C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB"

"c:\program files\git\usr\bin\tail.exe" -n 20 error.log
```

Output:

```
SQL Server is now ready for client connections.

Error: 17204, Severity: 16, State: 1.

FCB::Open failed:
Could not open file

D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\MSDBData.mdf

OS error: 21(The device is not ready.)

Error: 5120

Unable to open the physical file

D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\model.mdf

Error: 17207

FileMgr::StartLogFiles:
Operating system error 2

File activation failure.

Error: 945

Database 'model' cannot be opened due to inaccessible files.
```

---

# Interpretation

At this stage SQL Server executable was able to start.

The failure was no longer a VC++ runtime issue.

The existing LocalDB instance contained invalid database file references.

The referenced paths:

```
D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr...
```

were not valid paths on this machine.

---

# Recovery

Remove failed LocalDB instance:

```
sqllocaldb stop BluePrismLocalDB

sqllocaldb delete BluePrismLocalDB
```

Verify:

```
sqllocaldb info
```

Result:

```
MSSQLLocalDB
```

---

# Final result

Blue Prism database installation completed successfully.

Next step:

- load Blue Prism Learning Edition license file
- continue onboarding

---

# Future reproduction plan

Keep original failed VM snapshot.

For unbiased reproduction:

1. Start from clean snapshot.
2. Install Blue Prism Learning Edition.
3. Do not apply previous fixes.
4. Record:
   - exact popup messages
   - Event Viewer entries
   - LocalDB state
   - SQL error logs
5. Apply fixes only after evidence capture.


-----------------
"Blue Prism Learning Edition: The Runtime Dependency Saga"

w
https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist?view=msvc-170

Get-ItemProperty HKLM:\SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64 |
Select-Object Installed, Version, Major, Minor, Bld


Get-ItemProperty HKLM:\SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64 |
Select-Object Installed, Version, Major, Minor, Bld


Installed : 1
Version   : v14.51.36247.00
Major     : 14
Minor     : 51
Bld       : 36247


Get-ItemProperty HKLM:\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\x64 |
Select-Object Installed, Version, Major, Minor, Bld


Installed : 1
Version   : v14.51.36247.00
Major     : 14
Minor     : 51
Bld       : 36247


 HKLM:\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\x86 |
>> Select-Object Installed, Version, Major, Minor, Bld


Installed : 1
Version   : v14.51.36247.00
Major     : 14
Minor     : 51
Bld       : 36247


where.exe /r C:\Windows vcruntime140_1.dll
C:\Windows\System32\vcruntime140_1.dll

dir C:\Windows\System32\vcruntime140_1.dll  Volume in drive C has no label.
 Volume Serial Number is 547E-685D

 Directory of C:\Windows\System32

05/27/2026  04:59 AM            50,112 vcruntime140_1.dll
               1 File(s)         50,112 bytes
               0 Dir(s)   2,222,141,440 bytes free
 Volume in drive C has no label.
 Volume Serial Number is 547E-685D
 dir C:\Windows\SysWOW64\vcruntime140_1.dll

 Directory of C:\Windows\SysWOW64



path=%path%;"c:\Program Files\Microsoft SQL Server\160\LocalDB\Binn"

sqllocaldb.exe info
BluePrismLocalDB
MSSQLLocalDB

sqllocaldb.exe info MSSQLLocalDB
The automatic instance "MSSQLLocalDB" is not created.

sqllocaldb.exe info BluePrismLocalDB
Name:               BluePrismLocalDB
Version:            16.0.1000.6
Shared name:
Owner:              DESKTOP-RNOG166\sergueik
Auto-create:        No
State:              Stopped
Instance pipe name:

DO not

sqllocaldb delete MSSQLLocalDB
sqllocaldb create MSSQLLocalDB
sqllocaldb start MSSQLLocalDB

instead

sqllocaldb.exe start BluePrismLocalDB
Start of LocalDB instance "BluePrismLocalDB" failed because of the following error:
Error occurred during LocalDB instance startup: SQL Server process failed to start.


examine
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\


```
dir /b/s "C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB"
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\error.log
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\error1.log
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\error2.log
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\error3.log
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\error4.log
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\error5.log
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\error6.log
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300696207970000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300696241330000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300696266820000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300696301970000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300696339160000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300696369940000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300696425570000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300696467600000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300696499000000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\HkEngineEventFile_0_134300701623690000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\log_5.trc
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\log_6.trc
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\log_7.trc
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\log_8.trc
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\log_9.trc
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\master.mdf
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\mastlog.ldf
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\model.mdf
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\modellog.ldf
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\msdbdata.mdf
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\msdblog.ldf
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300696222380000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300696250270000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300696277750000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300696312440000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300696353220000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300696391660000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300696445570000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300696478690000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300696509010000.xel
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB\system_health_0_134300701633220000.xel












```

run:
```
pushd  "C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB"
"c:\program files\git\usr\bin\tail.exe" -n 20 error.log
```
```
C:\Users\sergueik\AppData\Local\Microsoft\Microsoft SQL Server Local DB\Instances\BluePrismLocalDB>"c:\program files\git\usr\bin\tail.exe" -n 20 error.log
2026-08-01 08:02:43.34 spid15s     Starting up database 'mssqlsystemresource'.
2026-08-01 08:02:43.35 spid21s     Dedicated administrator connection support was not started because it is disabled on this edition of SQL Server. If you want to use a dedicated administrator connection, restart SQL Server using the trace flag 7806. This is an informational message only. No user action is required.
2026-08-01 08:02:43.35 spid21s     SQL Server is now ready for client connections. This is an informational message; no user action is required.
2026-08-01 08:02:43.35 spid15s     The resource database build version is 16.00.1000. This is an informational message only. No user action is required.
2026-08-01 08:02:43.41 spid15s     Starting up database 'model'.
2026-08-01 08:02:43.93 spid13s     Error: 17204, Severity: 16, State: 1.
2026-08-01 08:02:43.93 spid13s     FCB::Open failed: Could not open file D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\MSDBData.mdf for file number 0.  OS error: 21(The device is not ready.).
2026-08-01 08:02:43.94 spid13s     Error: 5120, Severity: 16, State: 101.
2026-08-01 08:02:43.94 spid13s     Unable to open the physical file "D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\MSDBData.mdf". Operating system error 21: "21(The device is not ready.)".
2026-08-01 08:02:43.98 spid15s     Error: 17204, Severity: 16, State: 1.
2026-08-01 08:02:43.98 spid15s     FCB::Open failed: Could not open file D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\model.mdf for file number 1.  OS error: 21(The device is not ready.).
2026-08-01 08:02:43.98 spid15s     Error: 5120, Severity: 16, State: 101.
2026-08-01 08:02:43.98 spid15s     Unable to open the physical file "D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\model.mdf". Operating system error 21: "21(The device is not ready.)".
2026-08-01 08:02:44.27 spid15s     Error: 17207, Severity: 16, State: 1.
2026-08-01 08:02:44.27 spid15s     FileMgr::StartLogFiles: Operating system error 2(The system cannot find the file specified.) occurred while creating or opening file 'D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\modellog.ldf'. Diagnose and correct the operating system error, and retry the operation.
2026-08-01 08:02:44.27 spid15s     File activation failure. The physical file name "D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\modellog.ldf" may be incorrect.
2026-08-01 08:02:44.27 spid15s     Error: 945, Severity: 14, State: 2.
2026-08-01 08:02:44.27 spid15s     Database 'model' cannot be opened due to inaccessible files or insufficient memory or disk space.  See the SQL Server errorlog for details.
2026-08-01 08:02:44.27 spid15s     SQL Trace was stopped due to server shutdown. Trace ID = '1'. This is an informational message only; no user action is required.

```


It is not VC++ anymore. SQL Server actually starts:

SQL Server is now ready for client connections.

Then it dies because the system database files are pointing to impossible build-machine paths:

D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\MSDBData.mdf

and:

D:\dbs\sh\5uj5\1008_054209\cmd\11\obj\x64retail\sql\mkmastr\databases\mkmastr.nativeproj\model.mdf

That path is not from your machine. It is an internal Microsoft SQL build path that should only exist during SQL Server compilation.

The key line:

```
Error: 945

Database 'model' cannot be opened due to inaccessible files

```

```
sqllocaldb stop BluePrismLocalDB
sqllocaldb delete BluePrismLocalDB
```

```
sqllocaldb info
MSSQLLocalDB
```

use dummy password it prompts earlier okUcu2y1

copy licence file
net use
New connections will be remembered.


Status       Local     Remote                    Network

-------------------------------------------------------------------------------
             E:        \\vboxsrv\Downloads       VirtualBox Shared Folders
The command completed successfully.


C:\Users\sergueik>copy e:\*lic Desktop
e:\sergueik.lic
        1 file(s) copied.
:0
)
:0
O--------


https://en.wikipedia.org/wiki/Facepalm
https://www.youtube.com/watch?v=w7HKoZRqwJY
How To Use Blue Prism Learning Edition
https://www.youtube.com/watch?v=YJ9C4IJ9UtI
https://portal.blueprismn.com/user/register

How to download and install Blue Prism Learning Edition
https://www.youtube.com/watch?v=w7HKoZRqwJY
