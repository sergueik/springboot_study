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
for H in $(cat hosts1) ;  do D=/tmp/$H; mkdir $D; touch $D/data.txt; K=datakey; echo -e "$K:value for $H\n" > $D/data.txt ;  done
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
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

