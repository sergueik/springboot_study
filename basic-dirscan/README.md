
### Info
Plain directory file based data reporter

### Usage

* NOTE: replace `/tmp` with `$env:TEMP` for Windows host.

* set up few directories and datafiles
```sh
echo 'host1' > /tmp/hosts1
mkdir /tmp/host1
touch /tmp/host1/data.txt
mvn test
```
```sh
mvn spring-boot:run
```

* create few host lists
`/tmp/hosts1`:
```txt
host1
host2
host3
```
`/tmp/hosts2`:
```txt
host1
host3
host5
```
`/tmp/hosts3`:
```txt
host6
host7
```
* create data
```sh
for H in $(cat /tmp/hosts1) ;  do D=/tmp/$H; mkdir $D; touch $D/data.txt; K=datakey; echo -e "$K: value for $H\n" > $D/data.txt ;  done
```
or in Powershell:
```powershell
cd $env:TEMP
mkdir host1,host2,host3
'host1','host2','host3' | foreach-object { ('dataKey: value for ' + $_) | out-file -literalpath "$_/data.txt" -encoding ascii}
remove-item 'hosts1' -force
'host1','host2','host3','host4' | foreach-object { $_ | out-file -literalpath 'hosts1' -append -encoding ascii}
```
alternartively start with the `hosts` file that may have some structure:
```text
$server_tag $hostname/$extra_server_info $additional_fields
```
iterate over items of inerest and mock up the data files:
```powershell
$F='hosts2'
$K='datakey'
$V='value'
$T='server tag'
get-content $F  | where-object {
$_ -match $T -and  (-not ($_ -match '^#')) } | 
foreach-object { 
$fields = convertfrom-string $_; 
$H = $fields.P2 -replace '/.*$', ''; 
write-output $H; 
mkdir "${env:TEMP}/$H" -erroraction silentlycontinue;"${K}: ${V}" | out-file -literalpath "${env:TEMP}/$H/data.txt" -encoding ascii }
```
* test
```sh
curl 'http://localhost:8080/data?name=hosts1&key=datakey' | jq '.'
```
```json
{
  "host1": "value for host1",
  "host3": "value for host3",
  "host2": "value for host2"
}
```
```sh
curl 'http://localhost:8080/data?name=hosts2&key=datakey' | jq '.'
```
```json
{
  "host5": null,
  "host1": "value for host1",
  "host3": "value for host3"
}
```

```sh
curl 'http://localhost:8080/data?name=hosts3&key=datakey' | jq '.'
```
```json
{
  "host7": null,
  "host6": null
}
```

```sh
curl http://localhost:8080/data
```
```json
{}
```
```sh
curl 'http://localhost:8080/typeddata?name=hosts1&key=dataKey' | jq '.'
```
```json
[
  {
    "hostname": "host1",
    "key": "dataKey",
    "value": "value for host1"
  },
  {
    "hostname": "host2",
    "key": "dataKey",
    "value": "value for host2"
  }
]
```
```sh
curl 'http://localhost:8080/typeddata_v2?name=hosts1&key=dataKey' | jq '.'
```
```json
{
  "host1": {
    "key": "dataKey",
    "value": "value for host1"
  },
  "host2": {
    "key": "dataKey",
    "value": "value for host2"
  }
}
```
### See Also
  *  https://www.baeldung.com/spring-optional-path-variables
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


