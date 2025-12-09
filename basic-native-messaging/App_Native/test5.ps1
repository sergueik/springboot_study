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

try {
    # Prepare payload
    $payload = @{
        "cmd"     = "ping"
        "message" = "stranger"
    } | ConvertTo-Json -Depth 4

    $payloadBytes = [System.Text.Encoding]::UTF8.GetBytes($payload)
    $lenBytes = [BitConverter]::GetBytes($payloadBytes.Length)

    # Write length prefix + payload
    $proc.StandardInput.BaseStream.Write($lenBytes, 0, 4)
    $proc.StandardInput.BaseStream.Write($payloadBytes, 0, $payloadBytes.Length)
    $proc.StandardInput.Close()

    # Read 4-byte response length
    $respLenBytes = New-Object byte[] 4
    $read = $proc.StandardOutput.BaseStream.Read($respLenBytes, 0, 4)
    if ($read -ne 4) { throw "Failed to read response length. Only read $read bytes." }

    $respLen = [BitConverter]::ToInt32($respLenBytes, 0)
    Write-Host "DEBUG: Response length = $respLen"

    # Sanity check
    if ($respLen -gt 1024*1024) { throw "Response too big ($respLen bytes)" }

    $respBytes = New-Object byte[] $respLen
    $offset = 0
    while ($offset -lt $respLen) {
        $read = $proc.StandardOutput.BaseStream.Read($respBytes, $offset, $respLen - $offset)
        if ($read -le 0) { throw "Unexpected end of stream at offset $offset" }
        $offset += $read
    }

    $response = [System.Text.Encoding]::UTF8.GetString($respBytes)
    Write-Host "Response: $response"
}
catch [System.OutOfMemoryException] {
    Write-Error "OOM encountered! Response length = $($respLen)"
    Write-Error $_.Exception
}
catch {
    Write-Error "Exception: $($_.Exception.Message)"
    Write-Error $_.Exception
}
finally {
    if ($proc -ne $null) {
        $proc.WaitForExit()
        $proc.Dispose()
    }
}
