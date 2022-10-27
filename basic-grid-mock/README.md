### Info

this directory contains a mock of Selenium 4 status page endpoint for testing the
__REST client__
[app](https://github.com/sergueik/powershell_ui_samples/tree/master/external/csharp/light-rest) and eventually
__Sysem Tray Selenum Grid Status Checker__
[app](https://github.com/sergueik/powershell_ui_samples/tree/master/external/csharp/selenium-grid-status-checker)

### Usage

```sh
mvn spring-boot:run
```

access via `http://localhost:4444/status`

will return static JSON with the following schema:
```JSON
{
  "value": {
    "ready": true,
    "message": "Selenium Grid ready.",
    "nodes": [
      {
        "id": "340232e5-36dd-4014-9a86-7770e45579a6",
        "uri": "http://10.0.2.15:5555",
        "maxSessions": 1,
        "osInfo": {
          "arch": "amd64",
          "name": "Windows 10",
          "version": "10.0"
        },
        "heartbeatPeriod": 60000,
        "availability": "UP",
        "version": "4.0.0 (revision 3a21814679)",
        "slots": [
          {
            "lastStarted": "1970-01-01T00:00:00Z",
            "session": null,
            "id": {
              "hostId": "340232e5-36dd-4014-9a86-7770e45579a6",
              "id": "49d6090b-798d-4b0b-9ce7-8a7a7400e962"
            },
            "stereotype": {
              "browserName": "firefox",
              "platformName": "WIN10"
            }
          },
          {
            "lastStarted": "1970-01-01T00:00:00Z",
            "session": null,
            "id": {
              "hostId": "340232e5-36dd-4014-9a86-7770e45579a6",
              "id": "e6928dba-2a7b-4f4c-9c39-51e2ed542db6"
            },
            "stereotype": {
              "browserName": "chrome",
              "platformName": "WIN10"
            }
          }
        ]
      },
      {
        "id": "node1 added manually",
        "uri": null,
        "maxSessions": 0,
        "osInfo": null,
        "heartbeatPeriod": 0,
        "availability": "UP",
        "version": null,
        "slots": []
      },
      {
        "id": "node2 added manually",
        "uri": null,
        "maxSessions": 0,
        "osInfo": null,
        "heartbeatPeriod": 0,
        "availability": "DOWN",
        "version": null,
        "slots": []
      }
    ]
  }
}

```
To render it nicely in the Chrome browser will need to install the __JSON Formatter__ [chrome extension](https://chrome.google.com/webstore/detail/json-formatter/bcjindcccaagfpapjjmafapmmgkkhgoa/related) extension

![status-mock](https://github.com/sergueik/springboot_study/blob/master/basic-grid-mock/screenshots/capture-grid-status.png)

The Java code has all backing `Data`, `Node`, `Slot` etc. classes in the `model`
package so it can dynamically *add* "nodes" to "grid" like this:
```java
Data data = gson.fromJson(jsonString, Data.class);
Node node = new Node();
node.setAvailability("UP");
node.setId("node added manually");
Value value = data.getValue();
List<Node> nodes = value.getNodes();
nodes.add(node);
value.setNodes(nodes);
data.setValue(value);
return data;
```

### Author


[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
