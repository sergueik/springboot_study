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

```cmd
java -jar target\com.findinpath.findinpath.jar
```
visit the application landing page `http://localhost:8080/index`, login
### Background

The definition:
> __Feature Toggles__ _decouple_ deployment of code from release of new features

### Distorted Perpsective

Exactly — and that changes how I would evaluate the Harness article.

Harness is not writing a neutral history of conditional statements in programming languages. It is a vendor selling software-delivery capabilities, including feature flags and controlled releases. In that context, a title like “A Brief History of Feature Toggles: How the Software Tool Has Evolved” invites readers to understand the history of a product category that Harness itself wants to sell.

The crucial distinction is between the history of a programming technique and the history of the commercial problem that a vendor's product solves.

A conditional statement is not a feature-management platform. A configuration switch is not necessarily a release-control system. And a runtime flag is not automatically a remotely managed, targeted rollout.

The vendor's own industry's history undermines the simplistic timeline
There is a particularly useful contemporary source here: __LaunchDarkly__'s own 2015 retrospective says __Flickr__ was discussing feature flags as an engineering practice in 2009, __Facebook__ was popularizing dark launches in 2009, and __Etsy__ was discussing their use in 2011. __LaunchDarkly__ itself started in 2014 to make this sort of capability available without every company building its own infrastructure.

That gives us a more relevant timeline for the thing Harness actually sells:

| Milestone |What the evidence supports |
|-----------|---------------------------|
|1970s| __Harness__ asserts that feature toggles existed. Its article does not provide a concrete, dated example establishing what the toggle did |
|2009 | Documented industry discussion of feature flags and dark launches at __Flickr__ and __Facebook__|
|2014| __LaunchDarkly__ begins building a commercial service around centrally managed feature flags|
|2010s onward| Feature management becomes a distinct product category encompassing targeted rollout, experimentation and release control|

The qualification is important: these are documented milestones, not proof that nobody implemented similar systems earlier.

My criticism of Harness would therefore be sharper than simply saying its history is wrong. Its article makes an unsubstantiated leap from a broad, old programming concept to a modern software-delivery capability, while presenting that leap as the history of the tool.

A vendor can legitimately explain how an old technique evolved into a product. What it should not do is use the antiquity of the underlying technique to imply that the modern product category has an equally long history.

And there's an irony here: the most revealing history may be the one the article largely skips — why organizations that could already write a Boolean condition in their code eventually started paying vendors to manage those conditions centrally.

That is the commercially relevant story. It concerns deployment frequency, independent release control, targeting, governance, experimentation, and the operational burden of maintaining homegrown infrastructure - not the invention of if.

### Lessons Learned

#### __Windows Vista__ and the __Airbus A380__: *Building for Runways* That Don't Exist

There is a funny similarity between hardware and software, specifically __Windows Vista__ and the __Airbus A380__. Both ran into a similar problem: they were designed around assumptions about an operating environment that the market was not prepared to provide.

The A380 had a limited secondary market and could not operate at every airport without suitable infrastructure. Vista was designed for the PC of the future, but vendors tried to install it on their existing inventory of aging, underpowered machines.

The irony goes deeper, though. In both cases, the critical flaw was hiding in a part of the problem that the designers apparently had not even included in their task list.

Airbus engineers do not build airport runways. They can design an aircraft around particular runway and airport requirements, but they cannot assume that airports will rebuild their infrastructure to accommodate it.

Likewise, the Windows Vista team did not manufacture PCs. Someone needed to visit an actual computer shop and see what was on the shelves before designing an operating system around the capabilities of the next generation of hardware. The reality was an inventory of machines with limited memory, weak graphics capabilities, and drivers that were not ready for the new platform.

In both cases, a seemingly mundane observation from outside the design team could have been a showstopper: a finding serious enough to halt the project, force a fundamental redesign, or at least change the deployment strategy.

The missing questions were deceptively simple:

- **Airbus:** Who is going to build the runways and upgrade the airports needed to operate this aircraft?
- **Microsoft:** What hardware are PC vendors actually shipping to customers who will be expected to run this operating system?

Neither question was fundamentally about engineering the product itself. Both were about the ecosystem required to make the product viable.

The A380 was not going to build its own runways. Vista was not going to magically upgrade the computers on which vendors installed it.

And that is the broader lesson: a technically ambitious product can be undermined by a dependency that its designers treat as somebody else's problem.

Sometimes the most important engineering requirement is the one nobody put on the requirements list.

#### Sligthly Different varianat

## Windows Vista and the Airbus A380: Building for Runways That Don't Exist

There is a funny similarity between hardware and software, specifically Windows Vista and the Airbus A380. Both ran into a similar problem: they were designed around assumptions about an operating environment that the market was not prepared to provide.

The A380 had a limited secondary market and could not operate at every airport without suitable infrastructure. Vista was designed for the PC of the future, but vendors tried to install it on their existing inventory of aging, underpowered machines.

The irony goes deeper, though. In both cases, the critical flaw was hiding in a part of the problem that the designers apparently had not even included in their task list.

Airbus engineers do not build airport runways. They can design an aircraft around particular runway and airport requirements, but they cannot assume that airports will rebuild their infrastructure to accommodate it.

Likewise, the Windows Vista team did not manufacture PCs. Someone needed to visit an actual computer shop and see what was on the shelves before designing an operating system around the capabilities of the next generation of hardware. The reality was an inventory of machines with limited memory, weak graphics capabilities, and drivers that were not ready for the new platform.

In both cases, a seemingly mundane observation from outside the design team could have been a showstopper: a finding serious enough to halt the project, force a fundamental redesign, or at least change the deployment strategy.

The missing questions were deceptively simple:

- **Airbus:** Who is going to build the runways and upgrade the airports needed to operate this aircraft?
- **Microsoft:** What hardware are PC vendors actually shipping to customers who will be expected to run this operating system?

Neither question was fundamentally about engineering the product itself. Both were about the ecosystem required to make the product viable.

The A380 was not going to build its own runways. Vista was not going to magically upgrade the computers on which vendors installed it.

And there is one final similarity worth noting: both were enormous undertakings, involving extraordinary engineering effort and vast investments. Their respective makers had every reason to remember the experience as a lesson in what happens when a product's ambitions outrun the ecosystem that must support it.

One can almost imagine both organizations signing the same pledge in fresh blood: **never again will we build something this enormous without first checking whether the world around it is ready.**

Sometimes the most important engineering requirement is the one nobody put on the requirements list. And sometimes the most expensive lesson is the one an organization promises itself it will never have to learn twice.

### See Also
  * [Feature Flags with Spring](https://www.baeldung.com/spring-feature-flags)
  * [implement FF with Unleash](https://www.baeldung.com/java-unleash-feature-flags)

  * [guide to configure feature toggling with FF4J on a Springboot Project](https://github.com/jeandlv/ff4j-feature-toggling)

  * https://learn.microsoft.com/en-us/azure/azure-app-configuration/feature-management-overview
  * [FM Fundamentals](https://learn.microsoft.com/en-us/azure/azure-app-configuration/concept-feature-management)
    + [switch](https://learn.microsoft.com/en-us/azure/azure-app-configuration/concept-feature-management#switch-onoff)
    + [rollout](https://learn.microsoft.com/en-us/azure/azure-app-configuration/concept-feature-management#rollout)
    + [experiment](https://learn.microsoft.com/en-us/azure/azure-app-configuration/concept-feature-management#experiment)

  *  apparently a false answer: [the evolution of feature toggles in software development](https://www.harness.io/blog/a-brief-history-of-feature-toggles-how-the-software-tool-has-evolved)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

