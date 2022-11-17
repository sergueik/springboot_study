### Info
Perl script example to modify individual field in the Grafana Dashboard exported JSON

### Usage

```sh
perl edit.pl --input example.json  -debug
```
### Note

Internally modifies the hash deserialized from the sample JSON, directly:
```perl
$root->{'panels'}->[0]->{'type'} = 'new panel type';
$root->{'panels'}->[0]->{'targets'}->[0]->{'datasource'}->{'type'} = 'new datasource type';
```
this may become unmanageable quickly
it appears to be less laborous to simply convert the Grafana Dashboard JSON into a template and use brute string replace
```json
  "panels": [
    {
      "datasource": {
        "type": "PANEL_0_TARGET_0_DATASOURCE_TYPE",
        "uid": "PANEL_0_TARGET_0_DATASOURCE_UID"
      },
      "description": "PANEL_0_DESCRIPTION",
      "targets": [
        {
          "datasource": {
            "type": "PANEL_0_TARGET_0_DATASOURCE_TYPE",
            "uid": "PANEL_0_TARGET_0_DATASOURCE_UID"
          },
          "editorMode": "builder",
          "expr": "PANEL_O_TARGET_0_EXPR",
          "legendFormat": "__auto",
          "range": true,
          "refId": "A"
        }
      ],
      "title": "PANEL_0_TITLE",
      "type": "PANEL_0_TYPE"
    }
  ],
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


