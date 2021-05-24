

param(
  [String]$username = '',
  [String]$password = '',
  [String]$cookie_string = '',
  [String]$url = 'http://localhost:8080/employees/',
  [String]$request_method = 'GET',
  [String]$auth_key = '',
  [String]$auth_value = '',
  [String]$body = ''
)
# origin: http://poshcode.org/6368
function ConvertTo-UnsecureString  {
  param(
    [System.Security.SecureString][Parameter(Mandatory = $true)] $SecurePassword
  )
  $unmanagedString = [System.IntPtr]::Zero
  try {
    $unmanagedString = [Runtime.InteropServices.Marshal]::SecureStringToGlobalAllocUnicode($SecurePassword)
    return [Runtime.InteropServices.Marshal]::PtrToStringUni($unmanagedString)
  } finally {
    [Runtime.InteropServices.Marshal]::ZeroFreeGlobalAllocUnicode($unmanagedString)
  }
}

if ($username -eq '' -or $password -eq ''){
  # password prompt
  $Credentials = $(Get-Credential)

  $username = $Credentials.UserName
  $password = ConvertTo-UnsecureString $Credentials.Password
}

$headers = @{ 'Authorization' =
  ('Basic {0}' -f ([System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes(
     ('{0}:{1}' -f $username,$password)
   )
   )))
}

[System.Net.ServicePointManager]::ServerCertificateValidationCallback = { $true }
write-output ('{0} {1}' -f $request_method, $url)
$webRequest = [System.Net.WebRequest]::Create($url)
$webRequest.Method = $request_method
$webRequest.ContentType = 'application/json'
if ($cookie_string -ne ''){
  $webRequest.Headers.Add([System.Net.HttpRequestHeader]::Cookie, $cookie_string)
}


[System.Collections.Specialized.NameValueCollection] $requestHeaders = new-object System.Collections.Specialized.NameValueCollection
$headers.Keys | foreach-object {
  $key = $_
  $value = $headers.Item($key)
  $requestHeaders.Add($key, $value)
}
if ($auth_key -ne '' -and $auth_value -ne '' ) { 
  $requestHeaders.Add($auth_key, $auth_value) # add custom authentication
}
$webRequest.Headers.Add($requestHeaders)
# Write-output ("The HttpHeaders are \\n{0}" -f $webRequest.Headers )
# Invoke-WebRequest -uri $url -Headers $headers -Method $request_method -body $body -ContentType 'application/json' -UseBasicParsing
if ($webRequest.Method -match '(?:POST|PUT)') {
  [System.IO.Stream]$webRequestStream = $webRequest.GetRequestStream()
  [byte[]]$postDataBytesArray = [System.Text.Encoding]::GetEncoding('ASCII').GetBytes($body)
  $webRequestStream.Write($postDataBytesArray, 0, $postDataBytesArray.Length)
  $webRequestStream.Close()
}

try {
  [System.Net.WebResponse] $response = $webRequest.GetResponse()
  # NOTE: no HTTP status code in this snippet
  [System.IO.StreamReader] $sr = new-object System.IO.StreamReader($response.GetResponseStream())
  [string]$Result = $sr.ReadToEnd()
  write-output ('Content: {0}' -f  $Result )
} catch [Exception] {
   # System.Management.Automation.ErrorRecord -> System.Net.WebException
   $e = $_[0].Exception
   write-output ('Exception: Status:{0} StatusCode: {1} Message: "{2}"' -f $e.Status, $e.Response.StatusCode, $e.Message )
}
