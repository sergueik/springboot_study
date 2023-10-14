#Copyright (c) 2023 Serguei Kouzmine
#
#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:
#
#The above copyright notice and this permission notice shall be included in
#all copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#THE SOFTWARE.

# this version uses Invoke-WebRequest
param (
  [String]$config_dir = '',
  [String]$config_filename = 'data.json',
  [string]$base_url = 'http://localhost:8085/configs/file_hash_status'
)

# http://poshcode.org/2887
# http://stackoverflow.com/questions/8343767/how-to-get-the-current-directory-of-the-cmdlet-being-executed
# https://msdn.microsoft.com/en-us/library/system.management.automation.invocationinfo.pscommandpath%28v=vs.85%29.aspx
# https://gist.github.com/glombard/1ae65c7c6dfd0a19848c

function Get-ScriptDirectory {

  if ($global:scriptDirectory -eq $null) {
    [string]$global:scriptDirectory = $null

    if ($host.Version.Major -gt 2) {
      $global:scriptDirectory = (Get-Variable PSScriptRoot).Value
      write-debug ('$PSScriptRoot: {0}' -f $global:scriptDirectory)
      if ($global:scriptDirectory -ne $null) {
        return $global:scriptDirectory;
      }
      $global:scriptDirectory = [System.IO.Path]::GetDirectoryName($MyInvocation.PSCommandPath)
      write-debug ('$MyInvocation.PSCommandPath: {0}' -f $global:scriptDirectory)
      if ($global:scriptDirectory -ne $null) {
        return $global:scriptDirectory;
      }

      $global:scriptDirectory = Split-Path -Parent $PSCommandPath
      write-debug ('$PSCommandPath: {0}' -f $global:scriptDirectory)
      if ($global:scriptDirectory -ne $null) {
        return $global:scriptDirectory;
      }
    } else {
      $global:scriptDirectory = [System.IO.Path]::GetDirectoryName($MyInvocation.MyCommand.Definition)
      if ($global:scriptDirectory -ne $null) {
        return $global:scriptDirectory;
      }
      $Invocation = (Get-Variable MyInvocation -Scope 1).Value
      if ($Invocation.PSScriptRoot) {
        $global:scriptDirectory = $Invocation.PSScriptRoot
      } elseif ($Invocation.MyCommand.Path) {
        $global:scriptDirectory = Split-Path $Invocation.MyCommand.Path
      } else {
        $global:scriptDirectory = $Invocation.InvocationName.Substring(0,$Invocation.InvocationName.LastIndexOf('\'))
      }
      return $global:scriptDirectory
    }
  } else {
      write-debug ('Returned cached value: {0}' -f $global:scriptDirectory)
      return $global:scriptDirectory
  }
}


# see also: https://github.com/sergueik/powershell_ui_samples/blob/master/utils/hash_files.ps1
function checksum_file {
  param(
    [string]$unc_path)
  # TODO: refactor 
  return [System.BitConverter]::ToString((New-Object -TypeName 'System.Security.Cryptography.MD5CryptoServiceProvider').ComputeHash([System.IO.File]::ReadAllBytes((Get-Item -Path $unc_path).FullName)))
  # alternatively , just 
  # return (get-filehash -algorithm MD5 -literalpath $unc_path ).Hash
}

# wrapper around FileHelper class o write the config file
function updateConfig {
  param(
    [String]$filepath,
    [String]$text,
    [int]$retries = 3,
    [int]$interval = 250,
    [bool]$debug
  )
# safe read and write with retry for the "File in use by another process"
# based on: https://stackoverflow.com/questions/876473/is-there-a-way-to-check-if-a-file-is-in-use
$data_class = 'FileHelper'

if (-not ($data_class -as [type])) {
  # interpolate class name
  add-type -TypeDefinition @"
  using System;
  using System.IO;
  using System.Text;
  using System.Threading;

  public class $data_class {
    private int interval = 500;
    private string filePath = null;
    public int Retries { get; set; }
    public int Interval { set { interval = value; } get {return interval;}}
    public string FilePath { set { filePath = value; }  get {return filePath;}}
    public bool Debug { get; set; }
    public string Text { get; set; }
    private byte[] bytes;
    public byte[] Bytes { get { return bytes; } }

    private FileStream stream = null;

    public void WriteContents() {
      Boolean done = false;
      if (!string.IsNullOrEmpty(filePath)) {
        // Console.Error.WriteLine(String.Format("Writing data to {0}.", filePath));
        for (int cnt = 0; cnt != Retries; cnt++) {
          if (done)
            break;
          try {
            stream = new FileInfo(filePath).Open(FileMode.OpenOrCreate, FileAccess.Write, FileShare.None);
            bytes = Encoding.ASCII.GetBytes(Text);
            // stream.Lock(0, bytes.Length);
            // have to truncate
            stream.SetLength(0);
            if (Debug)
              Console.Error.WriteLine(String.Format("Writing text {0}.", Text));
            stream.Write(bytes, 0, bytes.Length);
            stream.Flush();
            if (Debug)
              Console.Error.WriteLine(String.Format("Written text {0}.", Text));
            // stream.Unlock(0, bytes.Length);
            done = true;

          } catch (IOException e) {
            Console.Error.WriteLine(String.Format("Got Exception during Write: {0}. " + "Wait {1, 4:f2} sec and retry", e.Message, (interval / 1000F)));
          } finally {
            if (stream != null)
              stream.Close();
          }
          // wait and retry
          if (!done)
            Thread.Sleep(interval);
        }
      }
      return;
    }

    // retries if "File in use by another process"
    // because file is being processed by another thread
    public void ReadContents() {
      if (Debug)
        Console.Error.WriteLine(String.Format("Reading the file {0}", filePath));
      Text = null;
      Boolean done = false;
      if (!string.IsNullOrEmpty(filePath) && File.Exists(filePath)) {
        for (int cnt = 0; cnt != Retries && Text == null; cnt++) {

          if (done)
            break;
          try {
            stream = new FileInfo(filePath).Open(FileMode.Open, FileAccess.Read, FileShare.None);

            int numBytesToRead = (int)stream.Length;
            if (numBytesToRead > 0) {
              bytes = new byte[numBytesToRead];
              int numBytesRead = 0;
              while (numBytesToRead > 0) {
                if (Debug)
                  Console.Error.WriteLine(String.Format("{0} bytes to read", numBytesToRead));
                int n = stream.Read(bytes, numBytesRead, numBytesToRead);
                if (n == 0)
                  break;

                numBytesRead += n;
                numBytesToRead -= n;
              }
              numBytesToRead = bytes.Length;
              if (bytes.Length > 0)
                Text = Encoding.ASCII.GetString(bytes);
              // the below call is race condition prone
              // text =  System.IO.File.ReadAllText(filePath);
              done = true;
            }
          } catch (IOException e) {
            if (Debug)
              Console.Error.WriteLine(String.Format("Got Exception during Read: {0}. " + "Wait {1, 4:f2} sec and retry", e.Message, (interval / 1000F)));
          } finally {
            if (Debug)
              Console.Error.WriteLine(String.Format("Read text \"{0}\". Retry: {1}", Text, cnt));
            if (stream != null)
              stream.Close();
          }
          // wait and retry
          if (Text == null) {
            if (Debug)
              Console.Error.WriteLine(String.Format("Wait {0, 4:f2} sec and retry: {1}", (interval / 1000F), cnt));
            Thread.Sleep(interval);
          }
        }
      }
      return;
    }
  }
"@
  # NOTE: the previous line with "@" marker should not be indented
}
  $o = new-object -typeName $data_class
  $o.Debug = $debug
  $o.Retries = $retries
  $o.Interval = $interval
  $o.FilePath = $filepath
  if ($debug) {
    write-host ('Read {0} safely' -f $o.FilePath )
  }
  $o.Text = $text
  $o.WriteContents()
  return
}

# NOTE: current mixing logic of reading the status from JSON and HTTP code 
# "/file_hash" route "/file_hash_status" route
# makes code convoluted

function download_status2 {
  param(
    $statuscode,
    [String]$outfile
  )
  [int] $status = 0
  if ($statuscode -eq 200) {
    write-host ('success')
    $status = 1
  } elseif (($statuscode -eq 304) -or ($statuscode -eq 208)) {
    write-host ('unmodified')
    $status = 2
  } else {
    write-host ('server error or misconfiguration')
    $status = 3
  }
  return $status
}

# use Invoke-WebRequest cmdlet to read HTTP Status
function getHttpStatusCode {
  param(
    [string]$url,
    [string]$outfile,
    [boolean]$debug
  
  )
  # workaround for the error ininvoke-webrequest cmdlet:
  # the underlying connection was closed: could not establish trust relationship for the SSL/TLSsecure channel
  # see also: https://stackoverflow.com/questions/11696944/powershell-v3-invoke-webrequest-https-error
  # https://learn.microsoft.com/en-us/dotnet/api/system.net.icertificatepolicy?view=netframework-4.0
  $helper_class = 'TrustAllCertsPolicy'
  if ( -not ( $helper_class -as [type])) {
    # ICertificatePolicy Interface validates a server certificate
    # ignore self-signed certificates
    add-type @"
    using System.Net;
    using System.Security.Cryptography.X509Certificates;
    public class ${helper_class} : ICertificatePolicy {
      public bool CheckValidationResult( ServicePoint srvPoint, X509Certificate certificate, WebRequest request, int certificateProblem) {
        return true;
      }
    }
"@
# NOTE: the line above should not be indented
  }
  
  # https://www.cyberforum.ru/powershell/thread2589305.html
  [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]'Ssl3,Tls,Tls11,Tls12'
  [System.Net.ServicePointManager]::CertificatePolicy = new-object -typename $helper_class
  # alternatively define as a lambda
  # [System.Net.ServicePointManager]::ServerCertificateValidationCallback = {$true}
  $statuscode = $null
  try {
    $ProgressPreference = 'Stop'
    # NOTE : do not set to "Stop" or the status code error will abort 
    # The running command stopped because the preference variable "ProgressPreference" or common parameter is set to Stop: Reading web response
    $ProgressPreference = 'SilentlyContinue'
    # let the invoke-restmethod write the server response to the file, read it afterwards
    if ($debug)  {
      write-host ('Invoke-WebRequest -uri {0} -OutFile {1} -passthru' -f $url, $outfile)
    }
    $response  = Invoke-WebRequest -uri $url -OutFile $outfile -passthru

    if ($debug)  {
      write-host ('Response Headers' -f $response.Headers )
      $headers = $response.Headers
      $headers.Keys | foreach-object { write-host ( '{0} = {1}' -f $_ ,$headers[$_] ) }

    }

    # alternative is to work with [System.Net.HttpWebRequest] or [System.Net.WebRequest]
    $statuscode =  $response.StatusCode
    if ($debug)  {
      write-host ('status code: {0}' -f $statuscode)
    }
    if ($debug)  {
      write-host ('saved the server response into {0}' -f $outfile )
    }
    # https://powershellmagazine.com/2012/11/13/pstip-get-the-contents-of-a-file-in-one-string/
    # alternatively 
    # $page = [System.IO.File]::ReadAllText($outfile)
    $page = Get-Content -literalpath $outfile -raw
    if ($debug)  {
      write-host ($page)
    }

    $ProgressPreference = 'Continue'
  } catch [Exception]{
    write-host ('Exception (intercepted): {0}' -f $_.Exception.Message)
    # uncomment the code below when debugging
    $exception = $_.Exception
    # TODO:
    # https://learn.microsoft.com/en-us/dotnet/api/system.net.webexceptionstatus?view=netframework-4.8
    # 1 NameResolutionFailure  
    # 15 ProxyNameResolutionFailure	
    # 2 ConnectFailure
    # 3 ReceiveFailure
    # 7 ProtocolError
    # 9 TrustFailure

    # $exception | select-object -property *
    $exception_response = $exception.Response
    # $exception_response | select-object -property *
    write-host ('Status Description: {0}' -f $exception_response.StatusDescription)
    $exception_statuscode = $exception_response.StatusCode
    write-host ('Status code: {0}' -f [int] $exception_statuscode)
    write-host ('Status code: {0}' -f $exception_statuscode.value__)
    # write-output $statuscode | select-object -property *

    $statuscode = $exception_statuscode.value__
  }
  return  $statuscode
}


# origin: https://4sysops.com/archives/convert-json-to-a-powershell-hash-table/
# see also: https://github.com/sergueik/powershell_ui_samples/blob/master/external/parse_json_hash.ps1

function ConvertTo-Hashtable {
    [CmdletBinding()]
    [OutputType('hashtable')]
    param (
        [Parameter(ValueFromPipeline)]
        $InputObject
    )
    process {
        if ($null -eq $InputObject) {
            return $null
        }

        if ($InputObject -is [System.Collections.IEnumerable] -and $InputObject -isnot [string]) {
            $collection = @(
                # NOTE: this is a breaking change
                # $InputObject | foreach-object { 
                #     $object = $_ 
                foreach ($object in $InputObject) {
                  # write-host ('object  is {0}' -f $object.gettype().Name)
                  # ConvertTo-Hashtable -InputObject $object
                  # write-host ('ConvertTo-Hashtable -InputObject "{0}"' -f  $object)
                  $result = ConvertTo-Hashtable -InputObject $object
                  # write-host ('result type is {0}' -f $result.gettype().Name)
                  write-output $result
                }
            )
            Write-Output -NoEnumerate $collection
        } elseif ($InputObject -is [psobject]) { 
            # If the object has properties that need enumeration
            # Convert it to its own dictionary table and return it
            # convertFrom-Json produces System.Management.Automation.PSCustomObject 
            # https://stackoverflow.com/questions/14012773/difference-between-psobject-hashtable-and-pscustomobject
            # which properties enumeration is fastest through its PSObject
            $dictionary = @{}
            foreach ($property in $InputObject.PSObject.Properties) {
                # write-host ('processing {0}' -f $property.Name)
                $dictionary[$property.Name] = ConvertTo-Hashtable -InputObject $property.Value
            }
            $dictionary
        } else {
            # the object isn't an array, collection, or other object, it's already a dictionary table
            # so just return it.
            $InputObject
        }
    }
}

# Main
if ($config_dir -eq '' ) {
   $config_dir = Get-ScriptDirectory 
}

$config_file_path = $config_dir + '\' + $config_filename 
if (test-path -path $config_file_path -pathtype Leaf) {
  $md5sum = checksum_file -unc_path $config_file_path
  $md5sum =  (get-filehash -algorithm MD5 -literalpath $config_file_path ).Hash
  [string]$url = ('{0}?filename={1}&hash={2}' -f $base_url, $config_filename, $md5sum)
} else {
  [string]$url = ('{0}?filename={1}' -f $base_url, $config_filename)
}
write-host ('GET {0}' -f $url )
$temp_file = $env:TEMP + '\' + $config_filename
$statuscode = getHttpStatusCode -url $url -outfile $temp_file -debug $true

write-output ('HTTP Stasus: {0}' -f $statuscode)
$download_status = download_status2 -statuscode $statuscode -outfile $temp_file

if ($download_status -eq 1) { 
  write-host ('Updating: {0}' -f $config_file_path)
  $page = Get-Content -literalpath $temp_file -raw 
  write-host $page
  # "copy-item" may not be entirely race condition safe
  # copy-item -destination $config_file_path -path $temp_file -force
  updateConfig -filepath $config_file_path -text $page
} elseif ($download_status -eq 3) {
   Write-host ('server error')
} else {
  write-host ('not updating {0}' -f $config_filename )
}
