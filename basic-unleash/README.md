### Info

Replica of  the [demo project](https://github.com/findinpath/unleash-demo)
[Java Spring Boot Feature Flag Unleash Examples](https://docs.getunleash.io/guides/feature-flag-examples-spring-boot) from [Featur Flag Tutorial](https://docs.getunleash.io/guides/implement-feature-flags-in-spring-boot)

### Usage
```sh
docker pull postgres:12.1-alpine
docker pull unleashorg/unleash-server:3.11.0
```
```sh
docker-compose up --build -d
```
```sh
 docker-compose ps
```
```text
NAME                      COMMAND                  SERVICE             STATUS              PORTS
docker-postgres-1         "docker-entrypoint.sΓÇª"   postgres            running (healthy)   0.0.0.0:5432->5432/tcp
docker-unleash-server-1   "docker-entrypoint.sΓÇª"   unleash-server      running             0.0.0.0:4242->4242/tcp
```
```sh
docker-compose logs unleash-server
```

will be blank if the application stack is healthy

```
docker-machine ip
```
```
192.168.99.100
```
open http://192.168.99.100:4242


![capture unleash login](screenshots/capture-unleash-login.png)


on 

![capture unleash config](screenshots/capture-unleash-server-config.png)

and create several feature toggles:
|feature toggle| strategy| details |
|--------------|---------|---------|
|`blueHeader`  |default on/off |   |
|`qotd`| `userWithId` |Add `bob` as a user `ID`|
|`specialGreeting` |custom made `deviceClass` |First create `deviceClass` strategy <br>with the parameter `device` of type `list`.<br/> When configuring the strategy for <br/>the `specialGreeting` feature toggle, enter <br/>`Desktop` and `Tablet` values in <br/>the preffered values for the device parameter|

```sh
curl -s http://192.168.99.100:4242/api/client/features | jq
```
```json
{
  "version": 1,
  "features": [
    {
      "name": "blueHeader",
      "type": "experiment",
      "enabled": true,
      "stale": false,
      "strategies": [
        {
          "name": "default",
          "parameters": {}
        }
      ],
      "variants": []
    }
  ]
}
```

> NOTE: after all features added the JSON will look like below:
```JSON
{
  "version": 1,
  "features": [
    {
      "name": "blueHeader",
      "description": "",
      "enabled": true,
      "strategies": [
        {
          "name": "default"
        }
      ],
      "variants": null,
      "createdAt": "2020-09-30T21:20:46.154Z"
    },
    {
      "name": "qotd",
      "description": "Quote of the day",
      "enabled": true,
      "strategies": [
        {
          "name": "userWithId",
          "parameters": {
            "userIds": "bob,john"
          }
        }
      ],
      "variants": null,
      "createdAt": "2020-09-30T21:34:37.612Z"
    },
    {
      "name": "specialGreeting",
      "description": "",
      "enabled": true,
      "strategies": [
        {
          "name": "deviceClass",
          "parameters": {
            "device": "Desktop,Tablet"
          }
        }
      ],
      "variants": null,
      "createdAt": "2020-10-01T20:03:16.114Z"
    }
  ]
}

```
the JSON might look slighly different if the different version of the unleash server is use
* App outside of Docker

update the `unleash.endpoint` from `localhost` to ip address of Docker machine:

```sh
sed -i 's|endpoint: http://localhost|endpoint: http://192.168.99.100|' src/main/resources/application.yml
```
```sh
mvn -Dskiptest clean package
```

```
java -jar target\com.findinpath.findinpath.jar
```
### Background

The definition:
> __Feature Toggles__ _decouple_ deployment of code from release of new features



### See Also
  * [Feature Flags with Spring](https://www.baeldung.com/spring-feature-flags)
  * [implement FF with Unleash](https://www.baeldung.com/java-unleash-feature-flags)

  * [ guide to configure feature toggling with FF4J on a Springboot Project](https://github.com/jeandlv/ff4j-feature-toggling)

  * https://learn.microsoft.com/en-us/azure/azure-app-configuration/feature-management-overview
  * https://learn.microsoft.com/en-us/azure/azure-app-configuration/concept-feature-management

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

