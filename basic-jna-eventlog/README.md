### Info

Standalone Event log logger derived from [dblock/log4jna](https://github.com/dblock/log4jna) - removed the dependency on Log4j with the plan to use with Logback


### Usage

build
```sh
mvn clean package
```

#### Log to Application Event Log:

```sh
java -jar target\example.jna_eventlog.jar -message "the quick brown fox lamps over the dozy jug"
```


* review Event Log entries

![Event log Message](https://github.com/sergueik/springboot_study/blob/master/basic-jna-eventlog/screenshots/capture-message.png)

* query in console

```powershell
get-eventlog -logname Application -source 'Application Error' -newest 1 |format-list
```
reveals
```text
Index              : 100331
EntryType          : Information
InstanceId         : 1000
Message            : Faulting application name: the quick brown fox lamps over the dozy jug, version: %2, time stamp: 0x%3
                     Faulting module name: %4, version: %5, time stamp: 0x%6
                     Exception code: 0x%7
                     Fault offset: 0x%8
                     Faulting process id: 0x%9
                     Faulting application start time: 0x%10
                     Faulting application path: %11
                     Faulting module path: %12
                     Report Id: %13
Category           : (3)
CategoryNumber     : 3
ReplacementStrings : {the quick brown fox lamps over the dozy jug}
Source             : Application Error
TimeGenerated      : 4/14/2024 10:41:43 AM
TimeWritten        : 4/14/2024 10:41:43 AM
UserName           :




```

alternatively (NOTE, due to lack of query conditon need to run the following very soon after the java test has run
```cmd
wevtutil.exe query-events Application /rd:true /f:text /c:10
```
```text
Event[2]:
  Log Name: Application
  Source: Application Error
  Date: 2024-04-14T10:40:31.000
  Event ID: 1000
  Task: N/A
  Level: Information
  Opcode: Info
  Keyword: Classic
  User: N/A
  User Name: N/A
  Computer: sergueik42
  Description:
Faulting application name: the quick brown fox lamps over the dozy jug, version:
 %2, time stamp: 0x%3
Faulting module name: %4, version: %5, time stamp: 0x%6
Exception code: 0x%7
Fault offset: 0x%8
Faulting process id: 0x%9
Faulting application start time: 0x%10
Faulting application path: %11
Faulting module path: %12
Report Id: %13

```

#### Using Custom Event Log with Arbitrary id

  *  remove  the custom event log created earlier and create it. These steps need to be performed in elevated prompt:

```powershell
remove-eventlog -LogName 'log4jna_sample'
```
if  you see the error
```text
remove-eventlog : Requested registry access is not allowed.
```
switch to elevated Powershell console (a.k.a "Run As Administrator"

  *  create the same custom event log  specifying the categorymessagefile and eventmessagefile to be
```powershell
$resource_dll_path = '%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll'
new-eventLog -logName 'log4jna_sample' -Source 'example.log4jna_sample' -CategoryResourceFile $resource_dll_path -MessageResourceFile $resource_dll_path
```
* append to the same custom event log  specifying the categorymessagefile and eventmessagefile, source, application, name, id and message
NOTE: When  run in `cmd` console
```cmd
java -jar target\example.jna_eventlog.jar -message "the quick brown fox lamps over the dozy jug" -id 123  -r "%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll" -application "log4jna_sample" -source "example.log4jna_sample" -name "log4jna_sample" -debug
```
this will echo the options:
```text
Command line options:
m (message): the quick brown fox lamps over the dozy jug
i (id): 123
r (resource): C:\WINDOWS\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll
a (application): log4jna_sample
s (source): example.log4jna_sample
n (name): log4jna_sample
d (debug): null
null
```
NOTE: the "environment" expansion has taken place.
There appears to be no better way to address this underised environment while passing command line options than follows
```cmd
set FOO=SYSTEMROOT
java -jar target\example.jna_eventlog.jar -message "the quick brown fox lamps over the dozy jug" -id 123  -r "%%FOO%%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll" -application "log4jna_sample" -source "example.log4jna_sample" -name "log4jna_sample" -debug
```

* alternatively run the same from powershell console
```powershell
java -jar target\example.jna_eventlog.jar -message "the quick brown fox lamps over the dozy jug" -id 123 -resource "%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll" -application "log4jna_sample" -source "example.log4jna_sample" -name "log4jna_sample" -debug
```
this will print
```text
Command line options:
m (message): the quick brown fox lamps over the dozy jug
i (id): 123
r (resource): %SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll
a (application): log4jna_sample
s (source): example.log4jna_sample
n (name): log4jna_sample
d (debug): null
```

repeat without the `debug` flag
```powershell
java -jar target\example.jna_eventlog.jar -message "the quick brown fox lamps over the dozy jug" -id 123 -resource "%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll" -application "log4jna_sample" -source "example.log4jna_sample" -name "log4jna_sample"
```
this will log the message. Confirm the message
```powershell
get-eventlog -logname log4jna_sample -newest 1 |format-list
```
```text
Index              : 258
EntryType          : Information
InstanceId         : 123
Message            : the quick brown fox lamps over the dozy jug
Category           : %1
CategoryNumber     : 3
ReplacementStrings : {the quick brown fox lamps over the dozy jug}
Source             : example.log4jna_sample
TimeGenerated      : 4/14/2024 12:19:24 PM
TimeWritten        : 4/14/2024 12:19:24 PM
UserName           :
```
Alternatively can use full path to resource when creating the event log:
```powershell
$resource_dll_path = "C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll"
new-eventLog -logName log4jna_sample -Source 'example.log4jna_sample' -CategoryResourceFile $resource_dll_path -MessageResourceFile $resource_dll_path
```
and use the same in `resource` argument:

```cmd
java -jar target\example.jna_eventlog.jar -message "the quick lawn box lamps over the dazy jug" -id 123  -r "C:\Windows\Microsoft.NET\Framework\v4.0.30319\EventLogMessages.dll" -application "log4jna_sample" -source "example.log4jna_sample" -name "log4jna_sample"
```
```powershell
get-eventlog -logname log4jna_sample -newest 1 |format-list
```
```text
Index              : 46
EntryType          : Information
InstanceId         : 123
Message            : the quick lawn box lamps over the dazy jug
Category           : %1
CategoryNumber     : 3
ReplacementStrings : {the quick lawn box lamps over the dazy jug}
Source             : example.log4jna_sample
TimeGenerated      : 4/14/2024 5:24:21 PM
TimeWritten        : 4/14/2024 5:24:21 PM
UserName           :

```
![Custom Event Log Message](https://github.com/sergueik/springboot_study/blob/master/basic-jna-eventlog/screenshots/capture-message-custom.png)

NOTE: when the arguments are invalid and the program is not run in elevated way -  java  will throw runtime exception:
```text

Exception in thread "main" java.lang.RuntimeException: Could not register event source: Access is denied.
        at example.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:145)
        at example.Win32EventLogAppender.append(Win32EventLogAppender.java:155)
        at example.Win32EventLogAppender.append(Win32EventLogAppender.java:175)
        at example.App.main(App.java:87)
Caused by: com.sun.jna.platform.win32.Win32Exception: Access is denied.
        at com.sun.jna.platform.win32.Advapi32Util.registryCreateKey(Advapi32Util.java:1282)
        at com.sun.jna.platform.win32.Advapi32Util.registryCreateKey(Advapi32Util.java:1260)
        at example.Win32EventLogAppender.createAndSetAllKeys(Win32EventLogAppender.java:207)
        at example.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:191)
        at example.Win32EventLogAppender.registerEventSource(Win32EventLogAppender.java:142)
        ... 3 more
```

because the logger will attempt to install the event source, which is a privileged operation on Windows

### Note

```cmd
wevtutil.exe enum-logs |findstr -i Application
```

```text
Application
Microsoft-Windows-Application Server-Applications/Admin
Microsoft-Windows-Application Server-Applications/Analytic
Microsoft-Windows-Application Server-Applications/Debug
Microsoft-Windows-Application Server-Applications/Operational
Microsoft-Windows-Application-Experience/Problem-Steps-Recorder
Microsoft-Windows-Application-Experience/Program-Compatibility-Assistant
Microsoft-Windows-Application-Experience/Program-Compatibility-Troubleshooter
Microsoft-Windows-Application-Experience/Program-Inventory
Microsoft-Windows-Application-Experience/Program-Inventory/Debug
Microsoft-Windows-Application-Experience/Program-Telemetry
```

```cmd
wevtutil.exe get-log Application
```
```text
name: Application
enabled: true
type: Admin
owningPublisher:
isolation: Application
channelAccess: O:BAG:SYD:(A;;0xf0007;;;SY)(A;;0x7;;;BA)(A;;0x7;;;SO)(A;;0x3;;;IU)(A;;0x3;;;SU)(A;;0x3;;;S-1-5-3)(A;;0x3;;;S-1-5-33)(A;;0x1;;;S-1-5-32-573)
logging:
  logFileName: %SystemRoot%\System32\Winevt\Logs\Application.evtx
  retention: false
  autoBackup: false
  maxSize: 2097152
publishing:
  fileMax: 1  
```
```cmd  
wevtutil.exe get-loginfo Application
```
```text

creationTime: 2013-05-22T05:26:52.743Z
lastAccessTime: 2013-05-22T05:26:52.743Z
lastWriteTime: 2024-04-13T19:49:44.941Z
fileSize: 69632
attributes: 8224
numberOfLogRecords: 0
oldestRecordNumber: 0
  
```
```cmd
wevtutil.exe enum-publishers
```
```text

.NET Runtime
.NET Runtime Optimization Service
ACPI
ASP.NET 2.0.50727.0
ASP.NET 4.0.30319.0
AeLookupSvc
AmdK8
AmdPPM
Apache Service
AppDynamics Agent
AppDynamics Coordinator
Application
Application Error
Application Hang
Application Popup
Application-Addon-Event-Provider
AsyncMac
Bowser
Browser
CardSpace 3.0.0.0
CardSpace 4.0.0.0
CertObj
Chkdsk
Chrome
CoordinatorService
DBWriter
DatabaseMail
Desktop Window Manager
DiskQuota
Display
Dnsapi
Dnscache
E1G60
ESENT
FltMgr
Git Credential Manager
Group Policy
Handwriting Recognition
Help CacheLib
Help Index
Help Protocol
Help Zip
HelpLibAgent
HelpLibManager
HidBth
HostableWebCore
HpSAMD
IIS Config
IISADMIN
IISInfoCtrs
IISLOG
IPMIDRV
Interactive Services detection
LSI_FC
LSI_SAS
LSI_SAS2
LSI_SCSI
LmHosts
LocationNotifications
LsaSrv
MSDMine
MSDTC Gateway
MSDTC WS-AT Protocol
MSSOAP
MSSQLSERVER
MSSQLServerADHelper100
MSiSCSI
MTConfig
MegaSR
Microsoft (R) Visual Basic Compiler
Microsoft Document Explorer
Microsoft Help Viewer
Microsoft Visual Studio
Microsoft Visual Studio Tools for Applications
Microsoft-IE
Microsoft-IE-JSDumpHeap
Microsoft-IEDVTOOL
Microsoft-IEFRAME
Microsoft-JScript
Microsoft-PerfTrack-IEFRAME
Microsoft-PerfTrack-MSHTML
Microsoft-Windows-ADSI
Microsoft-Windows-AIT
Microsoft-Windows-API-Tracing
Microsoft-Windows-ATAPort
Microsoft-Windows-ActionQueue
Microsoft-Windows-AltTab
Microsoft-Windows-AppID
Microsoft-Windows-AppIDServiceTrigger
Microsoft-Windows-AppLocker
Microsoft-Windows-Application Server-Applications
Microsoft-Windows-Application-Experience
Microsoft-Windows-ApplicationExperience-Cache
Microsoft-Windows-ApplicationExperience-LookupServiceTrigger
Microsoft-Windows-ApplicationExperience-SwitchBack
Microsoft-Windows-ApplicationExperienceInfrastructure
Microsoft-Windows-Audio
Microsoft-Windows-Audit
Microsoft-Windows-AuthenticationProvider
Microsoft-Windows-AxInstallService
Microsoft-Windows-Backup
Microsoft-Windows-BdeTriggerProvider
Microsoft-Windows-BfeTriggerProvider
Microsoft-Windows-Biometrics
Microsoft-Windows-BitLocker-API
Microsoft-Windows-BitLocker-Driver
Microsoft-Windows-Bits-Client
Microsoft-Windows-Bluetooth-MTPEnum
Microsoft-Windows-CAPI2
Microsoft-Windows-CDROM
Microsoft-Windows-CEIP
Microsoft-Windows-COM
Microsoft-Windows-COMRuntime
Microsoft-Windows-Calculator
Microsoft-Windows-CertPolEng
Microsoft-Windows-CertificateServicesClient
Microsoft-Windows-CertificateServicesClient-AutoEnrollment
Microsoft-Windows-CertificateServicesClient-CertEnroll
Microsoft-Windows-CertificateServicesClient-CredentialRoaming
Microsoft-Windows-CertificationAuthorityClient-CertCli
Microsoft-Windows-ClearTypeTextTuner
Microsoft-Windows-CmiSetup
Microsoft-Windows-CodeIntegrity
Microsoft-Windows-ComDlg32
Microsoft-Windows-Complus
Microsoft-Windows-CorruptedFileRecovery-Client
Microsoft-Windows-CorruptedFileRecovery-Server
Microsoft-Windows-CredUI
Microsoft-Windows-Crypto-RNG
Microsoft-Windows-D3D10Level9
Microsoft-Windows-DCLocator
Microsoft-Windows-DHCPv6-Client
Microsoft-Windows-DNS-Client
Microsoft-Windows-DSC
Microsoft-Windows-DUI
Microsoft-Windows-DUSER
Microsoft-Windows-DXGI
Microsoft-Windows-DXP
Microsoft-Windows-DateTimeControlPanel
Microsoft-Windows-Defrag
Microsoft-Windows-Deplorch
Microsoft-Windows-DesktopWindowManager-Diag
Microsoft-Windows-DeviceSync
Microsoft-Windows-DeviceUx
Microsoft-Windows-DfsSvc
Microsoft-Windows-Dhcp-Client
Microsoft-Windows-Dhcp-Nap-Enforcement-Client
Microsoft-Windows-DiagCpl
Microsoft-Windows-Diagnosis-DPS
Microsoft-Windows-Diagnosis-MSDE
Microsoft-Windows-Diagnosis-PCW
Microsoft-Windows-Diagnosis-PLA
Microsoft-Windows-Diagnosis-Scheduled
Microsoft-Windows-Diagnosis-Scripted
Microsoft-Windows-Diagnosis-ScriptedDiagnosticsProvider
Microsoft-Windows-Diagnosis-TaskManager
Microsoft-Windows-Diagnosis-WDC
Microsoft-Windows-Diagnosis-WDI
Microsoft-Windows-Diagnostics-Networking
Microsoft-Windows-Diagnostics-PerfTrack
Microsoft-Windows-Diagnostics-PerfTrack-Counters
Microsoft-Windows-Diagnostics-Performance
Microsoft-Windows-Direct3D10
Microsoft-Windows-Direct3D10_1
Microsoft-Windows-Direct3D11
Microsoft-Windows-DirectShow-Core
Microsoft-Windows-DirectShow-KernelSupport
Microsoft-Windows-DirectSound
Microsoft-Windows-DirectWrite
Microsoft-Windows-DirectWrite-FontCache
Microsoft-Windows-Directory-Services-SAM
Microsoft-Windows-Disk
Microsoft-Windows-DiskDiagnostic
Microsoft-Windows-DiskDiagnosticDataCollector
Microsoft-Windows-DiskDiagnosticResolver
Microsoft-Windows-Display
Microsoft-Windows-DisplayColorCalibration
Microsoft-Windows-DisplaySwitch
Microsoft-Windows-DistributedCOM
Microsoft-Windows-Documents
Microsoft-Windows-DomainJoinManagerTriggerProvider
Microsoft-Windows-DriverFrameworks-KernelMode
Microsoft-Windows-DriverFrameworks-UserMode
Microsoft-Windows-Dwm-Api
Microsoft-Windows-Dwm-Core
Microsoft-Windows-Dwm-Dwm
Microsoft-Windows-Dwm-Redir
Microsoft-Windows-Dwm-Udwm
Microsoft-Windows-DxgKrnl
Microsoft-Windows-DxpTaskRingtone
Microsoft-Windows-DxpTaskSyncProvider
Microsoft-Windows-EFS
Microsoft-Windows-EFSTriggerProvider
Microsoft-Windows-ELS-Hyphenation
Microsoft-Windows-EapHost
Microsoft-Windows-EaseOfAccess
Microsoft-Windows-EnergyEfficiencyWizard
Microsoft-Windows-EnhancedStorage-EhStorCertDrv
Microsoft-Windows-Eqos-SQM-Provider
Microsoft-Windows-ErrorReportingConsole
Microsoft-Windows-EtwCollector
Microsoft-Windows-EventCollector
Microsoft-Windows-EventLog-WMIProvider
Microsoft-Windows-EventSystem
Microsoft-Windows-Eventlog
Microsoft-Windows-FMS
Microsoft-Windows-FailoverClustering-Client
Microsoft-Windows-Fault-Tolerant-Heap
Microsoft-Windows-Feedback-Service-TriggerProvider
Microsoft-Windows-FileInfoMinifilter
Microsoft-Windows-FilterManager
Microsoft-Windows-Firewall
Microsoft-Windows-Firewall-CPL
Microsoft-Windows-Folder Redirection
Microsoft-Windows-Forwarding
Microsoft-Windows-FunctionDiscovery
Microsoft-Windows-FunctionDiscoveryHost
Microsoft-Windows-GettingStarted
Microsoft-Windows-GroupPolicy
Microsoft-Windows-GroupPolicyTriggerProvider
Microsoft-Windows-HAL
Microsoft-Windows-HealthCenter
Microsoft-Windows-HealthCenterCPL
Microsoft-Windows-Help
Microsoft-Windows-HomeGroup-ControlPanel
Microsoft-Windows-HomeGroup-ListenerService
Microsoft-Windows-HomeGroup-ProviderService
Microsoft-Windows-HotStart
Microsoft-Windows-Http-SQM-Provider
Microsoft-Windows-HttpEvent
Microsoft-Windows-HttpService
Microsoft-Windows-IE-F12-Provider
Microsoft-Windows-IIS-APPHOSTSVC
Microsoft-Windows-IIS-Configuration
Microsoft-Windows-IIS-FTP
Microsoft-Windows-IIS-IISManager
Microsoft-Windows-IIS-IISReset
Microsoft-Windows-IIS-IisMetabaseAudit
Microsoft-Windows-IIS-W3SVC
Microsoft-Windows-IIS-W3SVC-PerfCounters
Microsoft-Windows-IIS-W3SVC-WP
Microsoft-Windows-IIS-WMSVC
Microsoft-Windows-IPBusEnum
Microsoft-Windows-IPSEC-SRV
Microsoft-Windows-IdleTriggerProvider
Microsoft-Windows-International
Microsoft-Windows-International-RegionalOptionsControlPanel
Microsoft-Windows-Iphlpsvc
Microsoft-Windows-Iphlpsvc-Trace
Microsoft-Windows-Kernel-Acpi
Microsoft-Windows-Kernel-Boot
Microsoft-Windows-Kernel-BootDiagnostics
Microsoft-Windows-Kernel-Disk
Microsoft-Windows-Kernel-EventTracing
Microsoft-Windows-Kernel-File
Microsoft-Windows-Kernel-General
Microsoft-Windows-Kernel-Memory
Microsoft-Windows-Kernel-Network
Microsoft-Windows-Kernel-PnP
Microsoft-Windows-Kernel-Power
Microsoft-Windows-Kernel-Prefetch
Microsoft-Windows-Kernel-Process
Microsoft-Windows-Kernel-Processor-Power
Microsoft-Windows-Kernel-Registry
Microsoft-Windows-Kernel-StoreMgr
Microsoft-Windows-Kernel-Tm
Microsoft-Windows-Kernel-Tm-Trigger
Microsoft-Windows-Kernel-WDI
Microsoft-Windows-Kernel-WHEA
Microsoft-Windows-KnownFolders
Microsoft-Windows-L2NACP
Microsoft-Windows-LDAP-Client
Microsoft-Windows-LUA
Microsoft-Windows-LanGPA
Microsoft-Windows-LanguagePackSetup
Microsoft-Windows-LoadPerf
Microsoft-Windows-MCT
Microsoft-Windows-MF
Microsoft-Windows-MP4SDECD
Microsoft-Windows-MPEG2_DLNA-Encoder
Microsoft-Windows-MPRMSG
Microsoft-Windows-MPS-CLNT
Microsoft-Windows-MPS-DRV
Microsoft-Windows-MPS-SRV
Microsoft-Windows-MSDTC
Microsoft-Windows-MSDTC 2
Microsoft-Windows-MSDTC Client
Microsoft-Windows-MSDTC Client 2
Microsoft-Windows-MSMPEG2VDEC
Microsoft-Windows-MSMQ
Microsoft-Windows-MSMQTriggers
Microsoft-Windows-MSPaint
Microsoft-Windows-MUI
Microsoft-Windows-Magnification
Microsoft-Windows-MediaFoundation-MFReadWrite
Microsoft-Windows-MediaFoundation-Performance
Microsoft-Windows-MediaFoundation-Platform
Microsoft-Windows-MediaFoundation-PlayAPI
Microsoft-Windows-MemoryDiagnostics-Results
Microsoft-Windows-MemoryDiagnostics-Schedule
Microsoft-Windows-MobilityCenter
Microsoft-Windows-MountMgr
Microsoft-Windows-MsiServer
Microsoft-Windows-NAPIPSecEnf
Microsoft-Windows-NCSI
Microsoft-Windows-NDF-HelperClassDiscovery
Microsoft-Windows-NDIS
Microsoft-Windows-NDIS-PacketCapture
Microsoft-Windows-NTLM
Microsoft-Windows-NWiFi
Microsoft-Windows-Narrator
Microsoft-Windows-Netshell
Microsoft-Windows-Network-and-Sharing-Center
Microsoft-Windows-NetworkAccessProtection
Microsoft-Windows-NetworkGCW
Microsoft-Windows-NetworkManagerTriggerProvider
Microsoft-Windows-NetworkProfile
Microsoft-Windows-Networking-Correlation
Microsoft-Windows-NlaSvc
Microsoft-Windows-OLEACC
Microsoft-Windows-OOBE-Machine
Microsoft-Windows-OcSetup
Microsoft-Windows-OneX
Microsoft-Windows-OobeLdr
Microsoft-Windows-P2PIMSvc
Microsoft-Windows-PCI
Microsoft-Windows-PDH
Microsoft-Windows-PNRPSvc
Microsoft-Windows-ParentalControls
Microsoft-Windows-PeerToPeerDrtEventProvider
Microsoft-Windows-PeopleNearMe
Microsoft-Windows-PerfCtrs
Microsoft-Windows-PerfDisk
Microsoft-Windows-PerfNet
Microsoft-Windows-PerfOS
Microsoft-Windows-PerfProc
Microsoft-Windows-Perflib
Microsoft-Windows-Photo-Image-Codec
Microsoft-Windows-PnPMgrTriggerProvider
Microsoft-Windows-PortableDeviceStatusProvider
Microsoft-Windows-PortableDeviceSyncProvider
Microsoft-Windows-Power-Troubleshooter
Microsoft-Windows-PowerCfg
Microsoft-Windows-PowerCpl
Microsoft-Windows-PowerShell
Microsoft-Windows-PowerShell-DesiredStateConfiguration-FileDownloadManager
Microsoft-Windows-PrimaryNetworkIcon
Microsoft-Windows-PrintService
Microsoft-Windows-ProcessExitMonitor
Microsoft-Windows-Program-Compatibility-Assistant
Microsoft-Windows-QoS-Pacer
Microsoft-Windows-QoS-qWAVE
Microsoft-Windows-RPC
Microsoft-Windows-RPC-Events
Microsoft-Windows-RPC-FirewallManager
Microsoft-Windows-RPC-Proxy-LBS
Microsoft-Windows-RPCSS
Microsoft-Windows-RasServer
Microsoft-Windows-RasSstp
Microsoft-Windows-ReadyBoost
Microsoft-Windows-ReadyBoostDriver
Microsoft-Windows-Recovery
Microsoft-Windows-Registry-SQM-Provider
Microsoft-Windows-Reliability-Analysis-Engine
Microsoft-Windows-RemoteApp and Desktop Connections
Microsoft-Windows-RemoteAssistance
Microsoft-Windows-RemoteDesktopServices-RemoteDesktopSessionManager
Microsoft-Windows-Remotefs-UTProvider
Microsoft-Windows-Resource-Exhaustion-Detector
Microsoft-Windows-Resource-Exhaustion-Resolver
Microsoft-Windows-Resource-Leak-Diagnostic
Microsoft-Windows-ResourcePublication
Microsoft-Windows-RestartManager
Microsoft-Windows-SCPNP
Microsoft-Windows-SMBServer
Microsoft-Windows-SQM-Events
Microsoft-Windows-Search
Microsoft-Windows-Search-Core
Microsoft-Windows-Search-ProfileNotify
Microsoft-Windows-Search-ProtocolHandlers
Microsoft-Windows-Security-Audit-Configuration-Client
Microsoft-Windows-Security-Auditing
Microsoft-Windows-Security-IdentityListener
Microsoft-Windows-Security-Kerberos
Microsoft-Windows-Security-Netlogon
Microsoft-Windows-Security-SPP
Microsoft-Windows-Sens
Microsoft-Windows-ServiceReportingApi
Microsoft-Windows-Services
Microsoft-Windows-Services-Svchost
Microsoft-Windows-Servicing
Microsoft-Windows-Setup
Microsoft-Windows-SetupCl
Microsoft-Windows-SetupQueue
Microsoft-Windows-SetupUGC
Microsoft-Windows-ShareMedia-ControlPanel
Microsoft-Windows-SharedAccess_NAT
Microsoft-Windows-Shell-AuthUI
Microsoft-Windows-Shell-Core
Microsoft-Windows-Shell-DefaultPrograms
Microsoft-Windows-Shell-Shwebsvc
Microsoft-Windows-Shell-ZipFolder
Microsoft-Windows-Shsvcs
Microsoft-Windows-Sidebar
Microsoft-Windows-Smartcard-Server
Microsoft-Windows-SoftwareRestrictionPolicies
Microsoft-Windows-Speech-UserExperience
Microsoft-Windows-Spell-Checking
Microsoft-Windows-SpellChecker
Microsoft-Windows-SpoolerTCPMon
Microsoft-Windows-StartupRepair
Microsoft-Windows-StickyNotes
Microsoft-Windows-StorDiag
Microsoft-Windows-StorPort
Microsoft-Windows-StorSqm
Microsoft-Windows-Subsys-Csr
Microsoft-Windows-Subsys-SMSS
Microsoft-Windows-Superfetch
Microsoft-Windows-Sysprep
Microsoft-Windows-SystemHealthAgent
Microsoft-Windows-TBS
Microsoft-Windows-TCPIP
Microsoft-Windows-TPM-WMI
Microsoft-Windows-TSF-msctf
Microsoft-Windows-TSF-msutb
Microsoft-Windows-TZUtil
Microsoft-Windows-TabletPC-CoreInkRecognition
Microsoft-Windows-TabletPC-InputPanel
Microsoft-Windows-TabletPC-InputPersonalization
Microsoft-Windows-TabletPC-MathInput
Microsoft-Windows-TabletPC-MathRecognizer
Microsoft-Windows-TabletPC-Platform-Input-Core
Microsoft-Windows-TabletPC-Platform-Manipulations
Microsoft-Windows-TaskScheduler
Microsoft-Windows-TaskbarCPL
Microsoft-Windows-Tcpip-SQM-Provider
Microsoft-Windows-TelnetServer
Microsoft-Windows-TerminalServices-ClientActiveXCore
Microsoft-Windows-TerminalServices-ClientUSBDevices
Microsoft-Windows-TerminalServices-LocalSessionManager
Microsoft-Windows-TerminalServices-MediaRedirection
Microsoft-Windows-TerminalServices-MediaRedirection-DShow
Microsoft-Windows-TerminalServices-RemoteConnectionManager
Microsoft-Windows-ThemeCPL
Microsoft-Windows-ThemeUI
Microsoft-Windows-Time-Service
Microsoft-Windows-TriggerEmulatorProvider
Microsoft-Windows-TunnelDriver
Microsoft-Windows-TunnelDriver-SQM-Provider
Microsoft-Windows-UAC
Microsoft-Windows-UAC-FileVirtualization
Microsoft-Windows-UIAnimation
Microsoft-Windows-UIAutomationCore
Microsoft-Windows-UIRibbon
Microsoft-Windows-USB-USBHUB
Microsoft-Windows-USB-USBPORT
Microsoft-Windows-User Profiles General
Microsoft-Windows-User Profiles Service
Microsoft-Windows-User-ControlPanel
Microsoft-Windows-User-Loader
Microsoft-Windows-UserModePowerService
Microsoft-Windows-UserPnp
Microsoft-Windows-UxTheme
Microsoft-Windows-VAN
Microsoft-Windows-VDRVROOT
Microsoft-Windows-VHDMP
Microsoft-Windows-VWiFi
Microsoft-Windows-Video-For-Windows
Microsoft-Windows-VolumeControl
Microsoft-Windows-VolumeSnapshot-Driver
Microsoft-Windows-WABSyncProvider
Microsoft-Windows-WAS
Microsoft-Windows-WAS-ListenerAdapter
Microsoft-Windows-WCN-Config-Registrar
Microsoft-Windows-WCN-Config-Registrar-Secure
Microsoft-Windows-WER-Diag
Microsoft-Windows-WER-SystemErrorReporting
Microsoft-Windows-WFP
Microsoft-Windows-WHEA-Logger
Microsoft-Windows-WLAN-AutoConfig
Microsoft-Windows-WLGPA
Microsoft-Windows-WMI
Microsoft-Windows-WMI-Activity
Microsoft-Windows-WPD-CompositeClassDriver
Microsoft-Windows-WPD-MTPClassDriver
Microsoft-Windows-WPDClassInstaller
Microsoft-Windows-WSC-SRV
Microsoft-Windows-WUSA
Microsoft-Windows-WWAN-MM-EVENTS
Microsoft-Windows-WWAN-NDISUIO-EVENTS
Microsoft-Windows-WWAN-SVC-EVENTS
Microsoft-Windows-WWAN-UI-EVENTS
Microsoft-Windows-WebIO
Microsoft-Windows-WebServices
Microsoft-Windows-WebdavClient-LookupServiceTrigger
Microsoft-Windows-Win32k
Microsoft-Windows-WinHttp
Microsoft-Windows-WinINet
Microsoft-Windows-WinRM
Microsoft-Windows-Windeploy
Microsoft-Windows-Windows Defender
Microsoft-Windows-Windows Firewall With Advanced Security
Microsoft-Windows-WindowsBackup
Microsoft-Windows-WindowsColorSystem
Microsoft-Windows-WindowsSystemAssessmentTool
Microsoft-Windows-WindowsUpdateClient
Microsoft-Windows-Wininit
Microsoft-Windows-Winlogon
Microsoft-Windows-Winsock-AFD
Microsoft-Windows-Winsock-SQM
Microsoft-Windows-Winsock-WS2HELP
Microsoft-Windows-Winsrv
Microsoft-Windows-Wired-AutoConfig
Microsoft-Windows-WlanConn
Microsoft-Windows-WlanPref
Microsoft-Windows-Wordpad
Microsoft-Windows-XWizards
Microsoft-Windows-mobsync
Microsoft-Windows-ntshrui
Microsoft-Windows-osk
Microsoft-Windows-propsys
Microsoft-Windows-stobject
Microsoft.Transactions.Bridge 3.0.0.0
Microsoft.Transactions.Bridge 4.0.0.0
MsiInstaller
Mup
MyCustomEventSource
MyCustomEventSource1
MySQL
NdisWan
NetBIOS
NetBT
Netlogon
Ntfs
Ntfs
Parport
Parvdm
PerlMsg
PipeServer
PipeServerLog
PipeServerService
PlugPlayManager
Power
PowerShell
PptpMiniport
Processor
Puppet
RasAuto
RasClient
Rasman
RemoteAccess
RsFx0102
SMSvcHost 3.0.0.0
SMSvcHost 4.0.0.0
SNMPTRAP
SPP
SQLBrowser
SQLCTR
SQLDumper
SQLISPackage100
SQLISService100
SQLNCLI10
SQLNCLI11.1
SQLSERVERAGENT
SQLWEP
SceCli
SceSrv
Schannel
SecurityCenter
Serial
Server
Service Control Manager
Service1
ServiceModel Audit 3.0.0.0
ServiceModel Audit 4.0.0.0
SiSRaid2
SiSRaid4
SideBySide
Smb
SomeApplication
Srv
StillImage
Syslogd
SyslogdInstaller
System
System Restore
System.IO.Log 3.0.0.0
System.IO.Log 4.0.0.0
System.IdentityModel 3.0.0.0
System.IdentityModel 4.0.0.0
System.Runtime.Serialization 3.0.0.0
System.Runtime.Serialization 4.0.0.0
System.ServiceModel 3.0.0.0
System.ServiceModel 4.0.0.0
TCPMon
Tcpip
Tcpip6
TermDD
TestLog
TestService
TimeService
TraceLog Profiler
TracerX - PowerShellPlus
TransactionLog
TransactionService
USER32
VBRuntime
VBoxVideo
VDS Basic Provider
VDS Dynamic Provider
VDS Virtual Disk Provider
VSPerf
VSS
VSSetup
VSTO 4.0
VSTTExecution
ViaC7
Virtual Disk Service
Visual Studio - VsTemplate
Visual Studio Profiler
Volsnap
VsJITDebugger
WINSATAPI_ETW_PROVIDER
WMI.NET Provider Extension
WMIxWDM
WPC
WSH
WacomPen
Wd
WebDeployEventProvider
WerSvc
Win32k
Windows Activation Technologies
Windows Backup
Windows Disk Diagnostic
Windows Error Reporting
Windows Script Host
Workstation
adp94xx
adpahci
adpu320
aic78xx
amdsata
amdsbs
amdxata
arc
arcsas
atapi
b06bdrv
b57nd60x
beep
cdrom
devenv
disk
ebdrv
elxstor
eventlog
exFAT
i8042prt
iScsiPrt
iaStorV
idsvc
iirsp
intelppm
isapnp
kbdclass
kbdhid
lltdio
log4jna_sample
megasas
mirrorv3
mouclass
mouhid
mpio
mrxsmb
nfrd960
nvraid
nvstor
partmgr
pcmcia
ql2300
ql40xx
rdbss
rspndr
sbp2port
sermouse
stexstor
tunnel
usbperf
vga
volmgr
vsmraid
vsta
wdf01000
wecsvc
```
```cmd
wevtutil.exe enum-publishers |findstr -i application
```
```text
Application
Application Error
Application Hang
Application Popup
Application-Addon-Event-Provider
Microsoft Visual Studio Tools for Applications
Microsoft-Windows-Application Server-Applications
Microsoft-Windows-Application-Experience
Microsoft-Windows-ApplicationExperience-Cache
Microsoft-Windows-ApplicationExperience-LookupServiceTrigger
Microsoft-Windows-ApplicationExperience-SwitchBack
Microsoft-Windows-ApplicationExperienceInfrastructure
SomeApplication
  
```
```cmd
wevtutil.exe get-publisher Application
name: Application
guid: 00000000-0000-0000-0000-000000000000
helpLink:
message:
channels:
  channel:
    name: Application
    id: 9
    flags: 1
    message:
levels:
opcodes:
tasks:
  task:
    name: Devices

    value: 1
    eventGUID: 00000000-0000-0000-0000-000000000000
    message: 1
  task:
    name: Disk

    value: 2
    eventGUID: 00000000-0000-0000-0000-000000000000
    message: 2
  task:
    name: Printers

    value: 3
    eventGUID: 00000000-0000-0000-0000-000000000000
    message: 3
  task:
    name: Services

    value: 4
    eventGUID: 00000000-0000-0000-0000-000000000000
    message: 4
  task:
    name: Shell

    value: 5
    eventGUID: 00000000-0000-0000-0000-000000000000
    message: 5
  task:
    name: System Event

    value: 6
    eventGUID: 00000000-0000-0000-0000-000000000000
    message: 6
  task:
    name: Network

    value: 7
    eventGUID: 00000000-0000-0000-0000-000000000000
    message: 7
keywords:
```
```cmd
wevtutil.exe get-publisher "Application Error"
name: Application Error
guid: 00000000-0000-0000-0000-000000000000
helpLink: http://go.microsoft.com/fwlink/events.asp?CoName=Microsoft%20Corporation&ProdName=Microsoft%c2%ae%20Windows%c2%ae%20Operating%20System&ProdVer=6.1.7600.16385&FileName=wer.dll&FileVer=6.1.7600.16385
messageFileName: %SystemRoot%\System32\wer.dll
message:
channels:
  channel:
    name: Application
    id: 9
    flags: 1
    message:
levels:
opcodes:
tasks:
keywords:
  
```
```cmd
wevtutil.exe get-publisher "Application Popup"
```
```text
name: Application Popup
guid: 00000000-0000-0000-0000-000000000000
helpLink: http://go.microsoft.com/fwlink/events.asp?CoName=Microsoft%20Corporation&ProdName=Microsoft%c2%ae%20Windows%c2%ae%20Operating%20System&ProdVer=6.1.7600.16385&FileName=ntdll.dll&FileVer=6.1.7600.16385
messageFileName: %SystemRoot%\System32\ntdll.dll
message:
channels:
  channel:
    name: System
    id: 8
    flags: 1
    message:
levels:
opcodes:
tasks:
keywords:
```

### See Also

   * [Windows Event Log Functions](https://learn.microsoft.com/en-us/windows/win32/eventlog/event-logging-functions)
   * [evaluate if the current user is a member the "well-known" sid Administator group](https://www.rgagnon.com/javadetails/java-detect-if-current-user-is-admin-using-jna.html) - not a UAC replacement
   * https://superuser.com/questions/809901/check-for-elevation-at-command-prompt
   * http://blogs.msdn.com/b/virtual_pc_guy/archive/2010/09/23/a-self-elevating-powershell-script.aspx
   * https://stackoverflow.com/questions/7985755/how-to-detect-if-cmd-is-running-as-administrator-has-elevated-privileges
   * https://stackoverflow.com/questions/1894967/how-to-request-administrator-access-inside-a-batch-file
   * [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)
   * https://stackoverflow.com/questions/30970729/batch-output-file-without-expanding-variables


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

