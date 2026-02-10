# Graphviz Java HTTP Server (Pure Java Rendering)

Clone of [graphviz-server](https://github.com/omerio/graphviz-server), a lightweight Java HTTP server wrapping the locally installed [Graphviz](http://www.graphviz.org/) binary.

This version replaces direct `/usr/bin/dot` execution with **pure Java** using **Nashorn** or **GraalVM JS**.

---

## Usage

### Build

```sh
mvn -Psingle clean package
```

### Run in foreground

```sh
java -jar target/example.graphviz-java-fat.jar 8080
```

### Interact with the server

```sh
curl -v -X POST http://localhost:8080/ -d @../basic-graphviz/color.dot -o color.png
```

**Example curl request headers and response:**

```text
> POST / HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> Content-Length: 278
> Content-Type: application/x-www-form-urlencoded

* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Date: Fri, 06 Feb 2026 20:46:00 GMT
< Server: DotGraphics/1.1
< Content-Length: 32547
< Content-Type: image/png
```

**Console log sample:**

```text
20:11:07.181 INFO  example.GraphViz - Using Graphviz engine: graal
20:11:07.182 INFO  example.DotGraphics - Listening on port 8080
20:11:07.186 INFO  example.GraphvizGraalEngine - Initializing GraalJS context...
20:11:07.411 INFO  example.GraphvizGraalEngine - Loading /META-INF/resources/webjars/viz.js-graphviz-java/2.1.3/viz.js
20:11:08.108 INFO  example.GraphvizGraalEngine - Loaded viz.js, length=11717
20:11:10.672 INFO  example.GraphvizGraalEngine - Loaded full.render.js, length=2402990
20:11:10.672 INFO  example.GraphvizGraalEngine - Viz.js loaded successfully.
20:11:10.673 INFO  example.GraphViz - Graphviz engine warmup complete
```

---

## Docker Packaging

```sh
mvn clean package
```

```sh
docker pull eclipse-temurin:17-jre-alpine
export NAME=example-graphviz-graalvm-java
docker build -t $NAME -f Dockerfile .
docker container stop $NAME || true
docker container rm $NAME || true
docker run -p 8080:8080 --name $NAME -d $NAME
```

Test from host:

```sh
curl -sfX POST http://localhost:8080/ -d @../basic-graphviz/color.dot -o color.png
echo $?
```

Test from VM:

```sh
curl -sf --connect-timeout 5 --max-time 10 -X POST http://192.168.99.102:8080/ -d @../basic-graphviz/color.dot -o color.png
```

View container logs:

```sh
docker logs $NAME
```

### JVM Property Overrides

Pass properties via command-line:

```sh
docker run --rm \
  -p 8080:8080 \
  -e JAVA_OPTS="-Dgraphviz.engine=nashorn -Dserver.port=8080 -Dfile.encoding=UTF-8" \
  --name $NAME \
  $NAME
```

Pass `application.properties` via volume:

```sh
docker run --rm \
  -p 8080:8080 \
  -v /path/to/local/application.properties:/config/application.properties:ro \
  --name $NAME \
  $NAME
```

---

## Troubleshooting

- GraalJS is **much faster** than Nashorn but increases JAR size (~50 MB).
- `Viz.js` 2.x is fully **asynchronous**; using `while(!done){}` blocks the event loop and will hang rendering.
- Use a newer Docker environment to avoid thread creation errors:

```txt
[0.007s][warning][os,thread] Failed to start thread "VM Thread" - pthread_create failed
```

- Avoid aggressive manual `rank` constraints across clusters in Graphviz; mixing `cluster_*` and `rank=same` can cause warnings or crashes:

```text
node X was already in a rankset, deleted from cluster
Segmentation fault
```

---

## Authentication vs Authorization Concepts

### Authentication (AuthN)

- **Identity Store**: LDAP, Active Directory, RHDS
- **Credential Store**: Password hashes, certificates, MFA
- **Directory Service**: Hierarchical storage of users and principals

### Authorization (AuthZ)

- **Authorization Store**: Roles, groups, permissions
- **Policy Store**: XACML, OPA, etc.
- **Access Control Database**: Maps subjects to resources
- **Entitlement Store**: Enterprise IAM permissions

### OAuth / Delegated Authorization

For services like GitHub Copilot:

| Concept              | GitHub Flow                          | OAuth 2 Equivalent                 |
|--------------------- |------------------------------------ |---------------------------------- |
| User login           | Engineer authenticates via GitHub/SSO | Resource Owner authenticates      |
| MFA / 2FA            | Phone app, RSA token, TOTP           | Optional second factor             |
| Plugin request       | Plugin opens browser, waits for code | OAuth Client requests Auth Code   |
| Copy code / continue | User pastes code back into plugin    | Authorization Code Grant / PKCE   |

**Root cause pattern**:

- Subject exists in **Identity Store** but not yet in **Authorization / Entitlement Store**.
- Common in AD ForeignSecurityPrincipals, cross-realm Kerberos, OAuth external users, and federation mapping.

---

## Historical Context: ChiWriter

**ChiWriter** (MS-DOS, 1986) was a WYSIWYG scientific editor for mathematical and technical content:

![ChiWriter Screenshot 1](https://winworldpc.com/res/img/screenshots/ChiWriter%204.20%20-%20Edit.png)
![ChiWriter Screenshot 2](https://www.delfijn.nl/ChiWriter/chibox.gif)
![ChiWriter Screenshot 3](https://www.delfijn.nl/ChiWriter/chiallft.gif)

- Interactive, extended character support
- Visual feedback while typing
- Alternative to TeX/LaTeX
- Platform-locked, proprietary, niche

**Lesson**: Code-oriented, interactive editors have always offered clarity and productivity, long before modern tools like Graphviz or declarative pipelines.

---

## Advantages of Code-Oriented Diagrams

- Clarity of intent
- Readable abstraction
- Compactness
- Maintainability
- Expressiveness

**Example**: Graphviz vs. Drawio or XML pipelines. Declarative code shows *what* you want rather than *how to serialize it*.

---
### Graphviz Diagrams

One of the strengths of Graphviz (especially when combined with an agentic tool) is how naturally intent can be expressed and refined.
Instead of manually dragging boxes, the author describes the flow and then adjusts it using plain language.

Below is an example based on a familiar OAuth2 “on behalf of the user” delegated authorization flow.

The first version is generated automatically from a textual description.
It is logically correct, but visually suboptimal.

```dot

digraph OAuth2_Delegated_Flow {
    rankdir=LR
    splines=ortho
    node [shape=rect, style=rounded]

    subgraph cluster_IDE {
        label="IDE / Plugin"
        style=dashed
        IDE_Login [label="Start Login / Click 'Sign in with GitHub'"]
        IDE_ReceiveToken [label="Receive OAuth Token\n(Store in secret storage)"]
        IDE_API_Call [label="Use token for API calls"]
    }

    subgraph cluster_Browser {
        label="Browser"
        style=dashed
        Browser_Auth [label="User authenticates\n(SSO / Password / MFA)"]
        Browser_Consent [label="Consent / 'Copy code and continue'"]
    }

    subgraph cluster_GitHub {
        label="GitHub / OAuth Server"
        style=dashed
        AuthServer_AuthCode [label="Issue Authorization Code"]
        AuthServer_Token [label="Exchange Code for Access Token"]
        AuthServer_Check [label="Check Org / Group / License"]
    }

    IDE_Login -> Browser_Auth [label="Redirect with PKCE challenge"]
    Browser_Auth -> Browser_Consent [label="MFA / Login complete"]
    Browser_Consent -> IDE_ReceiveToken [label="Authorization Code"]
    IDE_ReceiveToken -> AuthServer_Token [label="Exchange code + PKCE"]
    AuthServer_Token -> IDE_ReceiveToken [label="Access token issued"]
    IDE_ReceiveToken -> IDE_API_Call [label="Calls Copilot API on user behalf"]
    IDE_API_Call -> AuthServer_Check [label="Token validated & permissions checked"]
}
```
Rendered with:
```sh
dot -Tpng -o screenshots/oauth2-delegated-auth-flow-poor-layout.png scratch/oauth2-delegated-auth-flow-poor-layout.dot
```

**Initial generated diagram (poor layout)**

![Oauth2 delegated flow](screenshots/oauth2-delegated-auth-flow-poor-layout.png) 

**Natural-language correction prompt**

Instead of manually repositioning boxes, the layout is corrected using intent:

> the flow heavily gravitates west.
i like to ask put actors in the following order
on the left plugin / ide
in the middle the browser
on the right oauth server.
currently the browser is on the left, ide on the right and oauth server underneath


**Updated diagram description**:
```dot
digraph OAuth2_Delegated_Flow {
    rankdir=LR
    splines=ortho
    node [shape=rect, style=rounded]

    // Actor lanes
    subgraph cluster_IDE {
        label="IDE / Plugin"
        style=dashed
        IDE_Login [label="Start Login / Click 'Sign in with GitHub'"]
        IDE_ReceiveToken [label="Receive OAuth Token\n(Store in secret storage)"]
        IDE_API_Call [label="Use token for API calls"]
    }

    subgraph cluster_Browser {
        label="Browser"
        style=dashed
        Browser_Auth [label="User authenticates\n(SSO / Password / MFA)"]
        Browser_Consent [label="Consent / 'Copy code and continue'"]
    }

    subgraph cluster_OAuth {
        label="GitHub / OAuth Server"
        style=dashed
        AuthServer_AuthCode [label="Issue Authorization Code"]
        AuthServer_Token [label="Exchange Code for Access Token"]
        AuthServer_Check [label="Check Org / Group / License"]
    }

    // Align all actors vertically
    { rank=same; IDE_Login; Browser_Auth; AuthServer_AuthCode }
    { rank=same; Browser_Consent; IDE_ReceiveToken; AuthServer_Token }
    { rank=same; IDE_API_Call; AuthServer_Check }

    // Flow arrows
    IDE_Login -> Browser_Auth [label="Redirect with PKCE challenge"]
    Browser_Auth -> Browser_Consent [label="MFA / Login complete"]
    Browser_Consent -> IDE_ReceiveToken [label="Authorization Code"]
    IDE_ReceiveToken -> AuthServer_Token [label="Exchange code + PKCE"]
    AuthServer_Token -> IDE_ReceiveToken [label="Access token issued"]
    IDE_ReceiveToken -> IDE_API_Call [label="Calls Copilot API on user behalf"]
    IDE_API_Call -> AuthServer_Check [label="Token validated & permissions checked"]
}
```
Final result:

![Oauth2 delegated flow](screenshots/oauth2-delegated-auth-flow.png)

#### Key takeaway

This demonstrates how Graphviz supports an intent-driven workflow:

* Start from a conceptual description  
* Render a first draft  
* Refine the diagram using natural language (“move actors left/right”, “align lanes”, “clarify flow”)  
* Preserve the diagram as reproducible code  

Instead of pixel-level manipulation, the author works at the level of meaning and structure, which makes the diagram:

* auditable  
* versionable  
* repeatable  
* and easy to evolve as understanding improves  

An additional advantage is that such diagrams are **combinable and scalable**.  
Individual subflows can be refined independently and later merged into larger system-level views.  
Unlike XML/JSON-based workflow tools (for example, [n8n](https://en.wikipedia.org/wiki/N8n) or 
[UiPath](https://en.wikipedia.org/wiki/UiPath), which quickly become hard to tweak and reason about
even for relatively simple flows, DOT remains readable and malleable as complexity grows. 
Compare the classic [Jenkins](https://en.wikipedia.org/wiki/Jenkins_(software)) 
"Freestyle project" with [Pipeline](https://www.jenkins.io/doc/book/pipeline/).

With a DSL-based diagrams, “polishing” a large flow piece by piece is not intimidating.  
Changes remain local, understandable, and reversible.  
This encourages iterative improvement rather than discouraging modification through visual clutter or brittle GUI layouts.

From a management perspective, this enables work to be distributed across a team according to **subject-matter expertise** rather than tooling expertise.  
Each contributor can focus on the part of the flow they understand best (authentication, authorization, data flow, infrastructure, policy), without needing to master a complex visual editor.  
The “language-like” nature of DOT makes collaboration practical and inclusive.

This also aligns with established practice in the [Jupyter](https://en.wikipedia.org/wiki/Project_Jupyter) and data science communities.  
Just as [gnuplot](https://en.wikipedia.org/wiki/Gnuplot) has long been recognized for reproducible data visualization, Graphviz and DOT are implicitly used in modern AI and ML tooling.  
Frameworks such as TensorFlow and scikit-learn already model computation and dependency graphs internally in ways that closely resemble DOT.  
For AI practitioners, this style of representation is natural and familiar.

This provides a strong argument when communicating with senior management:

* “Everyone can read it.”
* “No one is locked into a single tool or editor.”
* “It integrates naturally with engineering, data science, and documentation workflows.”
* “It is already aligned with how modern AI systems think about structure and flow.”

This is not exotic, this is already mainstream among the most demanding communities.
This approach is already standard in the Jupyter and AI communities.
In fact Graphviz plays the same pivital role with flows that gnuplot plays with data in the Jupyter ecosystem.
Even when people don’t write DOT directly, their tools generate and consume Graphviz diagrams every day. We’re simply making that practice explicit and reusable for architecture and flows.
If you embrace Jupyter, and [TensorFlow](https://en.wikipedia.org/wiki/TensorFlow), [scikit-learn](https://en.wikipedia.org/wiki/Scikit-learn), or [PyTorch](https://en.wikipedia.org/wiki/PyTorch) you already embraced __Graphviz__


The focus shifts away from individual technical achievement in a narrow programming environment and toward shared understanding of system intent and behavior.  
This makes it easier for management to approve lightweight, standard tooling (for example, installing Graphviz via apt, yum, or chocolatey) in place of heavyweight proprietary diagram editors.

In short, Graphviz turns diagrams into living artifacts:
not pictures to be manually drawn and forgotten, but structured knowledge that can be reviewed, evolved, and trusted over time.

## Memorable ChatGPT Feedback Openers

- ![That makes sense](screenshots/capture-chatgpt1.png)
- ![That explains it completely](screenshots/capture-chatgpt2.png)
- ![That's exactly what I was expecting](screenshots/capture-chatgpt3.png)
- ![Crossed all Three Hard Boundaries](screenshots/capture-chatgpt4.png)

### See Also

  * https://github.com/omerio/graphviz-server
  * https://github.com/omerio/graphviz-webapp
  * https://hub.docker.com/r/mejran/graphviz-server
  * https://github.com/blackears/svgSalamander SVG engine for Java
  * https://mvnrepository.com/artifact/guru.nidi/graphviz-java/0.18.1
  * https://javadoc.io/doc/guru.nidi/graphviz-java/latest/index.html
  * https://sergeytihon.com/tag/graphviz/
  * https://pikabu.ru/story/rub_goldberg_i_ego_mashinyi_9986808?ysclid=mlfs8nk32o584914016
  * https://youtu.be/GvnEBX9aedY
---
### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)  
