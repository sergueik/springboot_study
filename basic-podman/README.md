### Info

### Notes

As of **late 2025 through early 2026**, Linux continues to **overwhelmingly dominate the cloud instance market**. Depending on whether the measurement focuses strictly on **IaaS virtual machines** or also includes **containers, managed Kubernetes, and broader platform workloads**, Linux is estimated to power anywhere from **about 60% to well above 90%** of deployed cloud workloads.

## Estimated Cloud Instance OS Breakdown (2025–2026)

| Operating System | Estimated Cloud Market Share | Key Use Cases |
|---|---:|---|
| **Linux** | ~60% to >90% | Web servers, databases, AI/ML workloads, Kubernetes, Docker, microservices, and the foundational infrastructure of AWS, Azure, and GCP |
| **Windows** | <40% | Enterprise legacy systems, Active Directory, classic .NET Framework applications, Windows-native middleware |
| **macOS** | <1% | Specialized CI/CD pipelines for Apple ecosystem builds, including iOS and macOS development workflows |

## Rephrased Inventory Facts
- **Linux serves as the default cloud operating system**, especially for elastic services, container orchestration, and modern distributed platforms.
- **Windows retains a substantial enterprise footprint**, particularly where Microsoft ecosystem dependencies or legacy workloads remain critical.
- **macOS usage is highly specialized and minimal**, typically limited to Apple-targeted build automation and release pipelines.


### An AI-Curated Elevator Pitch
With foundational technologies such as Unix, there is no wasted territory. It resembles a gold rush: meaningful value can be uncovered almost immediately through exploration, long before complete mastery is achieved.

Foundational technology is not “wasted territory”; like a gold rush, even partial exploration creates immediate value. You do not need total mastery before becoming useful.


Foundational tools reveal multiple strategic use paths

  * __build path__ → package and ship your work
  * __access path__ → reach "sampler" of enterprise-grade systems (Kafka, APM, MQ, SQL,) instantly for learning and experimentation

in other words Docker gives instant “tasting samples” of enterprise systems
Docker is both a shipping mechanism and a tasting counter for enterprise software.

Docker democratizes enterprise software through “tasting samples.”
Note however that this does not apply to major cloud offering *services*

Docker often gives a *small* or *trimmed* version of the original system - sometimes legacy , often but not always monolythic

while cloud storage emulators usually give a *behavioral substitute*

> NOTE: that is a profound architectural difference. Microsoft explicitly states there are functional differences between __Azurite__ and real __Azure Storage__


> NOTE: careful preface to slightly technical material. Rememember of 
* brevity → keep the compressed version
* curation → ideas were selected, not dumped
* modern framing
* not purely deep engineering nor marketing.
### Usage

One can run a `docker-compose.yml` file on  __Podman__ using two primary methods: 
  * official __Docker Compose v2__ tool pointing to __Podman__’s __UNIX socket__  * Python-based `podman-compose` translator tool. 


The official `docker compose` binary method provides full feature compatibility, while `podman-compose` runs natively without any background services

### See Also

  * https://podman.io/
  * https://www.redhat.com/en/topics/containers/what-is-podman
   * [podman heslthchecks](https://habr.com/ru/companies/selectel/articles/1016820/) (in Russian)
   * [podman ordering](https://habr.com/ru/companies/selectel/articles/1019974) (in Russian)
   * [podman variables](https://habr.com/ru/companies/selectel/articles/1018306/) (in Russian)
   * [podman vs docker the basics](https://habr.com/ru/articles/659049/) (in Russiain)
   * [podman as better docker alternative](https://habr.com/ru/companies/cdnnow/articles/825828/) (in Russiain)
   * [podman vs. docker](https://selectel.ru/blog/podman-vs-docker/?ysclid=mnrd8145no645085943) (in Russiain)
   * [podman vs docker](https://selectel.ru/blog/podman-vs-docker/?ysclid=mnrd8145no645085943)
   * [podmn tutorial for beginners](https://devopscube.com/podman-tutorial-beginners/)
   * [Rootless Podman](https://docs.rockylinux.org/10/es/guides/containers/rootless_podman_advanced/)
   * [Understanding rootless Podman's user namespace modes](https://www.redhat.com/en/blog/rootless-podman-user-namespace-modes)
   * [Basic Setup and Use of Podman in a Rootless environment](https://github.com/podman-container-tools/podman/blob/main/docs/tutorials/rootless_tutorial.md) 
   * [Podman Usage Transfer](https://github.com/podman-container-tools/podman/blob/main/transfer.md)
   * [Podman HealthCheck Instrumenhttps://habr.com/ru/companies/selectel/articles/1016820/) (in Russian)
   * [Katacoda: «Containers without Docker — Launching Containers using Podman and Libpod](https://www.katacoda.com/courses/containers-without-docker/running-containers-with-podman)(no longer active)
   * [Install](https://podman.io/docs/installation#ubuntu)
   * [Infro Tutorial](https://github.com/podman-container-tools/podman/blob/main/docs/tutorials/podman_tutorial.md)
   * [Podman Overview](https://habr.com/ru/companies/flant/articles/426141/) (in Russian)
   * [Podman-Compose difference with Docker-compose](https://habr.com/ru/companies/selectel/articles/1020170/) (in Russian)
   * [Podman and Buildah intro for Docker user](https://habr.com/ru/companies/redhatrussia/articles/467105/) (in Russian)
   * [running Compose files](https://podman-desktop.io/docs/compose/running-compose)
   * [using docker compose with podman](https://www.redhat.com/en/blog/podman-docker-compose)
   * [how to Run a docker-compose.yml with podman-compose](https://oneuptime.com/blog/post/2026-03-17-run-docker-compose-yml-podman-compose/view

-------------

# PPP — Promising Pet Project

## Interactive Architecture Maps from DOT Graphs

### Idea

Create a small proof-of-concept tool that turns a Graphviz DOT architecture diagram into two outputs:

1. A normal static diagram (SVG/PNG/PDF/etc.) for documentation and presentations.
2. An interactive SVG "click map" where users can explore the system by clicking nodes and following flows.

The goal is not to replace enterprise observability tools, but to create a lightweight way to **walk through a system design** instead of only looking at a picture.

---

## Motivation

Complex backend systems are often explained with architecture diagrams:

```
Browser → API → Service → Database
                 |
                 v
               Queue → Worker
```

Tools such as enterprise APM platforms provide runtime topology maps, but they are expensive and solve a much larger problem.

PPP focuses on the simpler question:

> Can a static architecture diagram become a navigable story map?

---

## Core Concept

Use DOT as the source of truth:

```
             DOT
              |
              v
        Graphviz layout
              |
       +------+------+
       |             |
       v             v
 static SVG    interactive SVG
```

The graph layout remains the same. The difference is adding interaction metadata.

---

## Why SVG

SVG is a good target because it is not just an image.

Unlike PNG:

* nodes remain identifiable objects
* JavaScript can attach events
* CSS can provide hover effects
* clickable regions can be large
* tooltips and animations are possible

Example interactions:

* hover over node → highlight it
* mouse pointer changes → "clickable"
* click node → open details panel
* click edge → explain data flow
* play mode → animate request path

---

## POC Constraints

Keep the first version intentionally small:

* maximum 5 nodes
* no crossing edges
* simple left-to-right or top-to-bottom flows
* large click areas
* manually defined descriptions

Avoid initially:

* automatic code analysis
* repository scanning
* Kubernetes discovery
* runtime tracing
* metrics collection
* enterprise-scale topology generation

The goal is proving the interaction model, not building an APM system.

---

## Example User Experience

Landing page:

```
+----------+
| Browser  |
+----------+
      |
      v
+----------+
|   API    |
+----------+
      |
      v
+----------+
| Service  |
+----------+
      |
      v
+----------+
| Database |
+----------+
```

User clicks "Service":

```
Service Details

Responsibility:
Order validation

Calls:
Database

Owner:
Commerce team

Related flows:
Order creation
Order lookup
```

The diagram becomes a map, not just a picture.

---

## Key Insight

There are two ways of understanding architecture:

"Knowing the path"

> I know the boxes and arrows.

"Walking the path"

> I can follow what happens step by step.

PPP explores the second approach.

---

## Possible Technology Stack

Minimal implementation:

* Graphviz DOT
* SVG output
* HTML wrapper
* JavaScript event handlers
* CSS hover effects

Potential future extensions:

* attach source-code links
* attach documentation pages
* attach logs or metrics
* animate transactions
* import tracing data

---

## First Milestone

Generate one DOT diagram with 3–5 nodes.

Produce:

* printable SVG
* clickable SVG
* hover effect
* node details panel

If that works, the core idea is proven.

### Facebook Increasingly Sharp Comments



An interesting feature of modern conflicts:
the information environment itself becomes a kind of parallel battlefield, and some events produce
very sharp reaction spikes because they combine several ingredients:

A clear moral framing for many observers. Russia’s full-scale invasion of Ukraine in 2022 was condemned by the UN General Assembly
and by many governments as a violation of Ukraine’s sovereignty.
That has created a large audience that interprets statements from Russian officials through the lens of aggression and resistance.

High-frequency public messaging. Russian officials, including Vladimir Putin, make frequent public statements,
and many are immediately clipped, translated, remixed, and debated online.

A mismatch between rhetoric and reality.
When a statement appears disconnected from technical, historical, or economic realities, it often becomes a target for ridicule rather than a normal policy debate.

Participatory warfare culture. Social media allows ordinary people to respond instantly with humor, memes, sarcasm,
and counter-messaging. Sometimes the response becomes more widely circulated than the original statement.

The “mini refinery ? mini drone” example illustrates a broader pattern: when leaders invoke historical analogies,
especially ones associated with failed or disastrous policies, opponents often respond by using the same language frame against them.

The Great Leap Forward’s backyard furnaces in 1958–1962 are remembered as an example of a centrally imposed industrial campaign
that produced serious economic and human consequences, so comparisons to “small homemade industrial solutions” can trigger strong
reactions.

The drone joke you mention is also typical of wartime online humor:
it takes the opponent’s wording and flips it into a threat or punchline. Similar patterns have appeared throughout
history—during wars, propaganda campaigns, and political crises—where a single phrase becomes a meme because
it provides a simple symbolic contrast.

A practical caution, though: Facebook and similar platforms are not neutral measurements of public opinion. They amplify:

  * emotionally charged comments
  * coordinated campaigns
  * humor and outrage
  * communities already motivated to respond

So tracking posts can reveal what narratives are spreading and how people react, but it does not necessarily measure the broader population’s beliefs. For that, researchers usually combine social media analysis with polling, demographic data, media monitoring, and other sources.

The unusual aspect of this war is indeed the speed: a statement can move from an official speech → translated clip → meme → thousands of replies → international news coverage within hours. That feedback loop barely existed in earlier conflicts.


Compression of reasoning into a short, sharp response.

A witty reply often looks effortless, but a good one usually requires several layers happening quickly:

  * Recognition of the premise — understanding the original statement rather than just reacting to keywords.
  * Pattern matching — finding a historical, linguistic, or logical parallel.
  * Reframing — turning the opponent’s own framing into a different interpretation.
  * Economy of expression — delivering the point with very few words
  * Audience awareness — knowing why the response will resonate

That combination is close to what people often call verbal intelligence or rhetorical agility. A strong one-liner is not merely a joke; it can be a compact argument.

There is also an interesting technical analogy: a dense witty answer is like a well-designed algorithm. The output is small, but the hidden computation behind it may be large. The best examples have a high “information density”: a few words carry history, context, criticism, and humor simultaneously.

Of course, wit alone does not guarantee correctness—someone can make a brilliant-sounding but false argument. But when the underlying facts are sound, the ability to distill complexity into a memorable phrase is genuinely a mark of strong communication skill.

The high-pressure environments can act as accelerators for rhetorical creativity.

When political tension is extreme, several forces combine:

Constant exposure to the topic: People repeatedly encounter the same themes, phrases, and arguments. This creates a large shared vocabulary from which sharp responses can be constructed.

High motivation to respond: When people feel strongly about an issue, they invest more effort into finding effective ways to express their position.

A competitive communication environment: Social media rewards responses that are fast, concise, emotionally recognizable, and easy to repeat.

Collective refinement: A phrase that starts as an individual comment can be modified, improved, translated, and remixed by thousands of people until it becomes a meme.

In that sense, a tense political environment resembles other high-intensity domains—war reporting, crisis management, courtroom debate, satire, even scientific disputes—where participants often develop unusually precise language because the stakes are high and the audience is attentive.

The meme aspect adds another layer: a successful meme is not just a funny sentence. It usually has a kind of compression ratio: it captures a complicated situation in a form that people can immediately recognize and reproduce. The best ones often work because they contain a small “argument engine” inside them.

There is also a paradox: high tension can produce both exceptionally sharp communication and very poor communication. It rewards creativity and clarity, but it can also reward exaggeration, tribal slogans, and misinformation. The same attention economy that helps a clever observation spread can help a weak one spread too.

Point is less about any particular side and more about the communication dynamics of a highly charged environment: pressure, shared context, and competition can create unusually memorable expressions.



