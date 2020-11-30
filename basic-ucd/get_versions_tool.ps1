param (
  [switch]$debug
)

$DEFAULT_RELEASE_DESCRIPTOR = 'release.json'
$RELEASE_DESCRIPTOR = $DEFAULT_RELEASE_DESCRIPTOR
echo "Processing release descriptor file ${RELEASE_DESCRIPTOR}"

# original command:
# $jq -r '.versions|flatten' $RELEASE_DESCRIPTOR|$jq -r '[.[]|keys]'|$jq -cr 'flatten|.[]'

$release = get-content -path $RELEASE_DESCRIPTOR | convertfrom-json

<#
C:\tools\jq-win64.exe -r '.versions|flatten' $RELEASE_DESCRIPTOR|C:\tools\jq-win64.exe -r '[.[]|keys]'|C:\tools\jq-win64.exe -cr 'flatten|.[]'
#>
<#
component_1
component_2
component_3
component_4
component_5
component_6
#>
<#
  # TODO: Cannot use variables in this command
  $jq = 'C:\tools\jq-win64.exe'
  $jq -r '.versions|flatten' $RELEASE_DESCRIPTOR|$jq -r '[.[]|keys]'|$jq -cr 'flatten|.[]'
  # + ... %% - /c $jq -r '.versions|flatten' $RELEASE_DESCRIPTOR|$jq -r '[.[]|k
  # +                                                                ~~
  # Unexpected token '-r' in expression or statement.
#>

$release = get-content -path $RELEASE_DESCRIPTOR | convertfrom-json

$component_names = @()
$components = @{}

0..($release."versions".length - 1) |
foreach-object {
  $row = $_
  $entry = $release.versions[$row];
  if ($debug) {
    write-output $entry| get-member -type NoteProperty
  }
  # https://stackoverflow.com/questions/27642169/looping-through-each-noteproperty-in-a-custom-object
  $entry | get-member -type NoteProperty | foreach-object {
    $name = $_.Name
    $version = $entry."${name}"
    if ($debug) {
      write-output ("row = {0}`nname = {1}`nversion = {2}" -f $row,$name,$version)
    }
    $component_names += $name
    $components[$name] = $version
  }
}

write-output $component_names

$SNAPSHOT_API_RESPONSE = 'snapshot.json'
echo "Processing API response file ${SNAPSHOT_API_RESPONSE}"

# original command: 
# SNAPSHOT_COMPONENT_NAMES=($(jq -cr '.[]|.name' $SNAPSHOT_API_RESPONSE))
# C:\tools\jq-win64.exe -cr '.[]|.name' $SNAPSHOT_API_RESPONSE
$cnt = 0
$snapshot = get-content -path $SNAPSHOT_API_RESPONSE | convertfrom-json
$snapshot_component_names = @()
$snapshot_component_versions = @{}
$snapshot | foreach-object {
	$component = $_
    if ($debug) {
      if ($cnt -eq 1 ){
        write-output 	$component| get-member -type NoteProperty
      }
      $cnt ++
    }
    $name = $component."name"
    $versions = $component."desiredVersions"
    $snapshot_component_names += $name
    $snapshot_component_versions[$name] = $versions.count
}
write-output $snapshot_component_names
write-output $snapshot_component_versions | format-list

$SNAPSHOTS_API_RESPONSE = 'snapshots.json'
echo "Processing API response file ${SNAPSHOTS_API_RESPONSE}"

$cnt = 0
$snapshots = get-content -path $SNAPSHOTS_API_RESPONSE | convertfrom-json

$snapshot_names = @()
$snapshots | foreach-object {
    $snapshot = $_
    if ($debug) {
      if ($cnt -eq 1 ){
        write-output $snapshot| get-member -type NoteProperty
      }
      $cnt ++
    }
    $name = $snapshot."name"
    $snapshot_names += $name
}

write-output $snapshot_names 
# https://stackoverflow.com/questions/27951561/use-invoke-webrequest-with-a-username-and-password-for-basic-authentication-on-t
<#
$cred = get-Credential
$BASE_URL = 'http://localhost:8443'
# NOTE: not really REST API
Invoke-WebRequest -Uri "$BASE_URL/rest/deploy/application/${APPLICATION}/snapshots/false" -ContentType 'text/json' -Credential $cred
$username = 'user'

$password = 'pass'

$pair = "${username}:${password}"

$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))

$basic_auth = "Basic $encodedCreds"

$Headers = @{
    Authorization = $basic_auth
}

Invoke-WebRequest -Uri "$BASE_URL/cli/snapshot/getSnapshotVersions?application=${APPLICATION}&snapshot=${SNAPSHOT}" -ContentType 'text/json' -Headers $Headers
#>