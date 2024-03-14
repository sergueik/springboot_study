# based on:
# https://www.outsidethebox.ms/blog/wp-content/uploads/files/Backup-MsftBlog.zip
param(
  [Uri]$uri
) 

$ErrorActionPreference = 'stop'
$shared_assemblies_path = "${env:USERPROFILE}\Downloads"
pushd $shared_assemblies_path
# dependency load: https://www.nuget.org/packages/HtmlAgilityPack/
# .NET code library parser that allows  to "parse 'out of the web' HTML files in a  is very tolerant with 'real world' malformed HTML" way

# version 1.9.0 is OK
# latest version is https://www.nuget.org/api/v2/package/HtmlAgilityPack/1.11.58
$shared_assemblies = @(
  'HtmlAgilityPack.dll'
)
$shared_assemblies | ForEach-Object {
  if ($host.Version.Major -gt 2) {
    Unblock-File -Path $_
  }
  write-debug $_
  Add-Type -Path $_
} 
popd


Add-Type -AssemblyName 'System.Web'

$wr = Invoke-WebRequest -Uri $uri.AbsoluteUri
$doc = New-Object htmlagilitypack.htmldocument
$doc.LoadHtml($wr.Content)
$name = [Web.HttpUtility]::HtmlDecode($name)
$bookmark = $doc.DocumentNode.SelectSingleNode("//a[@rel='bookmark']")

write-host $bookmark.InnerText

