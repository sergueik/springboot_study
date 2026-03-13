### Info

Trimmed replica of the  project [avro-converter](https://github.com/cheikh-kebe/Avro-converter) exercising convert json file and swagger openapi into avro schema

### Usage
* JSON → Avro Schema

```sh
jq < data.json
```
```json
{
  "type": "record",
  "name": "User",
  "namespace": "com.example",
  "fields": [
    {
      "name": "id",
      "type": "int"
    },
    {
      "name": "username",
      "type": "string"
    },
    {
      "name": "followers",
      "type": {
        "type": "array",
        "items": "string"
      }
    }
  ]
}

```
```cmd
java -jar target\example.avro.jar data.json schema.avsc
```
```text
Converting JSON to Avro schema...
  Input:  data.json
  Output: schema.avsc
Conversion completed successfully!
```

Avro Schema → JSON exemple
```sh

java -jar target\example.avro.jar  generate schema.avsc result.json record
```
```text
Generating sample JSON from Avro schema...
  Schema: schema.avsc
  Output: result.json
  Record: record
JSON generation completed successfully!
```
```json
{
  "type": "example_string",
  "name": "example_string",
  "namespace": "example_string",
  "fields": [
    {
      "name": "example_string",
      "type": "example_string"
    }
  ]
}

```
### Trouleshooting
caused by: org.apache.maven.plugin.PluginExecutionException: Execution default-testCompile of goal org.apache.maven.plugins:maven-compiler-plugin:3.6.0:testCompile failed: multiple points

### See Also
  * https://github.com/michelin/avro-xml-mapper
  * https://github.com/fge/json-schema-avro/
  * https://github.com/java-json-tools/json-schema-core
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
