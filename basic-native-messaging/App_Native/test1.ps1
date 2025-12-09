$payload = '{"cmd":"ping"}'
$payload = @{
	"cmd" = "ping" ;
	"message" = "stranger";
	} | convertto-json -depth 4
$bytes = [System.Text.Encoding]::UTF8.GetBytes($payload)

$len = [BitConverter]::GetBytes($bytes.Length)
$java_runtime = 'c:\java\jdk1.8.0_202\bin\java.exe'
$jar = 'ge.vakho.native_messaging-jar-with-dependencies.jar'
$argument = ('-jar "{0}"' -f ((resolve-path -path 'target').path + '\'+ $jar )) 
write-output ('{0} {1}' -f $java_runtime, $argument)
$proc = Start-Process -FilePath $java_runtime -argumentlist $argument -NoNewWindow -RedirectStandardInput true -RedirectStandardOutput true -PassThru
<#

NOTE:this will require Powershell 7.x
Start-Process : This command cannot be run because either the parameter
"RedirectStandardInput 'C:\developer\sergueik\springboot_study\basic-native-mes
saging\App_Native\true'" has a value that is not valid or cannot be used with
this command. Give a valid input and Run your command again.




on classic PowerShell 5.x, so:

❗ Start-Process DOES NOT SUPPORT STREAMING STDIN/STDOUT

It only supports:

-RedirectStandardInput <filePath>
-RedirectStandardOutput <filePath>


NOT streams.

#>

$proc.StandardInput.BaseStream.Write($len,0,4)
$proc.StandardInput.BaseStream.Write($bytes,0,$bytes.Length)
$proc.StandardInput.Close()

$resp = $proc.StandardOutput.BaseStream.Read()

