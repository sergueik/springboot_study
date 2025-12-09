$psi = New-Object System.Diagnostics.ProcessStartInfo
$psi.FileName = "c:\java\jdk1.8.0_202\bin\java.exe"

$jar = 'ge.vakho.native_messaging-jar-with-dependencies.jar'
$argument = ('-jar "{0}"' -f ((resolve-path -path 'target').path + '\'+ $jar )) 
write-output ('{0} {1}' -f $java_runtime, $argument)
$psi.Arguments = $argument
$psi.CreateNoWindow = $true
$psi.UseShellExecute = $false
$psi.RedirectStandardInput = $true
$psi.RedirectStandardOutput = $true

$proc = New-Object System.Diagnostics.Process
$proc.StartInfo = $psi
$proc.Start() | Out-Null

# Build request
$payload = @{
	"cmd" = "ping" ;
	"message" = "stranger";
	} | convertto-json -depth 4
$bytes = [System.Text.Encoding]::UTF8.GetBytes($payload)
$lenBytes = [BitConverter]::GetBytes($bytes.Length)

# Write
$proc.StandardInput.BaseStream.Write($lenBytes, 0, 4)
$proc.StandardInput.BaseStream.Write($bytes, 0, $bytes.Length)
$proc.StandardInput.Flush()

# Read length prefix
$respLenBytes = New-Object byte[] 4
$proc.StandardOutput.BaseStream.Read($respLenBytes, 0, 4) | Out-Null
$respLen = [BitConverter]::ToInt32($respLenBytes, 0)

# Read JSON
$respBytes = New-Object byte[] $respLen
$proc.StandardOutput.BaseStream.Read($respBytes, 0, $respLen) | Out-Null
$response = [System.Text.Encoding]::UTF8.GetString($respBytes)

Write-Host "Response: $response"

