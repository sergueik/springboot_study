### Info 

replica of [Opvolger/json.grafana.datasources](https://github.com/Opvolger/json.grafana.datasources) a [simple-json](https://github.com/grafana/simple-json-backend-datasource)(NOTE: deprecated in favor []()) datasource with alerting support- requires one to run own REST http 1.1 server
### Usage

NOTE: now installation of the now legacy plugin on the Grafana node may be required:

```sh
grafana-cli plugins install grafana-simple-json-datasource
```

### Data initial table load /info
Post the following info into `http://localhost:8181/storedata/set_info`, or via [swagger](http://localhost:8181/swagger)

```json
{
  "name": "test_enkel",
  "info": {
    "description": "Overzicht voor test_enkel",
    "type": "default"
  },
  "table": [
    {
        "jsonvalue":  "key",
        "type":  "string",
        "text":  "Machinename"
    },
    {
        "jsonvalue":  "kolom_bool",
        "type":  "bool",
        "text":  "bool kolom"
    },
    {
        "jsonvalue":  "kolom_time",
        "type":  "time",
        "text":  "tijd kolom"
    },
    {
        "jsonvalue":  "kolom_string",
        "type":  "string",
        "text":  "string kolom"
    }
  ]
}
```
This will lead to creation of a 'test' directory within the data folder, containing two files: `table.json` and `info.json`.

### Sending Data

POST the following information to [storedata/send_data](http://localhost:8181/storedata/send_data); this can be done via [Swagger](http://localhost:8181/swagger).

```json
{
  "name": "test_enkel",
  "json_data": [ 
    { "key": "machine1", "kolom_bool": true, "kolom_time": "2020-10-27T21:24:31.78Z", "kolom_string": "iets" },
    { "key": "machine2", "kolom_bool": true, "kolom_time": "2020-10-27T21:24:31.78Z", "kolom_string": "iets meer" },
    { "key": "machine3", "kolom_bool": false, "kolom_time": "2020-10-28T21:24:31.78Z", "kolom_string": "iets minder" },
    { "key": "machine4", "kolom_bool": false, "kolom_time": "2020-11-27T21:24:31.78Z", "kolom_string": "niets" },
    { "key": "machine5", "kolom_bool": true, "kolom_time": "2019-10-27T21:24:31.78Z", "kolom_string": "hoi iets" }
  ] 
}
```

For futher information see original project.
