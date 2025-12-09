$psi = New-Object System.Diagnostics.ProcessStartInfo
$java  = 'c:\java\jdk-11.0.12\bin\java.exe'
$psi.FileName = $java
$jar = 'ge.vakho.native_messaging-jar-with-dependencies.jar'
$argument = ('-jar "{0}"' -f ((resolve-path -path 'target').path + '\'+ $jar )) 
write-output ('{0} {1}' -f $java, $argument)
$psi.Arguments = $argument
$psi.RedirectStandardInput = $true
$psi.RedirectStandardOutput = $true
$psi.UseShellExecute = $false
$psi.CreateNoWindow = $true

$proc = New-Object System.Diagnostics.Process
$proc.StartInfo = $psi
$proc.Start() | Out-Null

# Prepare JSON payload
$payload = @{
    "cmd" = "ping"
    "message" = "stranger"
} | ConvertTo-Json -Depth 4

# Convert payload to UTF-8 bytes
$payloadBytes = [System.Text.Encoding]::UTF8.GetBytes($payload)
$lenBytes = [BitConverter]::GetBytes($payloadBytes.Length)

# Write length prefix + payload
$proc.StandardInput.BaseStream.Write($lenBytes, 0, 4)
$proc.StandardInput.BaseStream.Write($payloadBytes, 0, $payloadBytes.Length)
$proc.StandardInput.Close()

# Read 4-byte response length
$respLenBytes = New-Object byte[] 4
$proc.StandardOutput.BaseStream.Read($respLenBytes, 0, 4) | Out-Null
$respLen = [BitConverter]::ToInt32($respLenBytes, 0)

# Read exact response bytes
$respBytes = New-Object byte[] $respLen
$proc.StandardOutput.BaseStream.Read($respBytes, 0, $respLen) | Out-Null

# Decode UTF-8 string
$response = [System.Text.Encoding]::UTF8.GetString($respBytes)
Write-Host "Response: $response"

$proc.WaitForExit()
$proc.Dispose()
