### Info

Replica of [danvega/spring-io-mcp](https://github.com/danvega/spring-io-mcp)
a MCP server implemnting RPC style on __Spring boot__ spring-boot-starter-parent [3.5](https://mvnrepository.com/artifact/org.springframework.boot/spring-boot-starter-parent/3.5.0) - using ``
For `streamable  HTTP` see [danvega/dvaas](https://github.com/danvega/dvaas) - used later relase  of __Spring Boot__ spring-boot-starter-parent [3.5.5](https://mvnrepository.com/artifact/org.springframework.boot/spring-boot-starter-parent/3.5.5)

### Usage


build on JDK __17__ (the __24__ is overkill) 
```sh
curl -skLo ~/Downloads/apache-maven-3.8.9.zip https://dlcdn.apache.org/maven/maven-3/3.8.9/binaries/apache-maven-3.8.9-bin.zip
```
```sh
unzip -C /c/java ~/Downloads/apache-maven-3.8.9.zip 
```
```sh
set java_home=c:\java\jdk-17.0.12
c:\java\init.cmd
```

```sh
mvn -DskipTests package
```

### ToolChain Dependnecies

purely idiosyncratic:

* The plugin `maven-compiler-plugin`:__3.14.0__ requires Maven version __3.6.3__ 
* The plugin `org.apache.maven.plugins:maven-surefire-plugin`:__3.5.3__ requires Maven version __3.6.3__ 

### Background (non MCP)
real cultural split, and there’s some nice irony to mine there.

Many modern IDE vendors implicitly sell the idea of habitation: the IDE as a complete ecosystem where the developer lives all day — code, test, debug, deploy, chat with AI, manage tickets, maybe eventually order lunch without leaving the sidebar. The success metric is often “minutes spent inside.”

Ops / platform / SRE culture is almost the inverse. The ideal interaction is more like orbital mechanics: connect, perturb the system just enough, verify trajectory, and get out. If you’re staring at a terminal for hours babysitting something, that’s usually a smell. Good operations work aims for disposability, automation, and absence.

If One may  want to phrase this ironically
 
 Software tooling keeps trying to turn developers into houseplants: rooted permanently inside one carefully fertilized IDE. Ops people evolved more like migratory birds — land, execute, depart.
 modern IDE promises “you’ll never need to leave.”
To an operations engineer, that sounds less like productivity and more like vendor-managed captivity

e application developer is taught permanence: curate your workspace, customize your habitat, settle in.
The operations engineer is taught impermanence: assume every session, node, container, credential, and endpoint is already halfway to disappear


clash of philosophies: IDE vendors optimize for stickiness and immersion, while ops engineers optimize for disposability and detachment.

IDE philosophy: never leave the tool. Ops philosophy: never trust the machine to still be there when you come back

The IDE trains you to stay. So when you need to leave—switch tools, contexts, languages—it punishes you with startup time, indexing, and reconfiguration.”
“DevOps trains you to leave. So when you need to stay—remember context, logs, state—it punishes you with amnesia.”

If your day looks like long, deep sessions:

You benefit from a heavy, stateful IDE.
You pay occasionally when switching—but that’s rare enough to tolerate.

If your day is round-robin across 10+ contexts:

You benefit from lightweight, fast-entry tools.
You pay in lost context—but you accept that as the cost of mobility.


onclusion is mostly right—but it’s a bit too binary. In practice, people don’t get to choose one mode forever; they oscillate. The real question is which mode dominates your day, and whether your tools fight that reality or align with it.

You can frame it like this:


“Optimize for the pain you hit most often.”

If your day looks like long, deep sessions:

You benefit from a heavy, stateful IDE.
You pay occasionally when switching—but that’s rare enough to tolerate.

If your day is round-robin across 10+ contexts:

You benefit from lightweight, fast-entry tools.
You pay in lost context—but you accept that as the cost of mobility.

A sharper way to express your idea:

“If you live in one problem, optimize for depth.
If you live across many, optimize for re-entry.”

two incompatible default worldviews:

IDE worldview (vendor optimism)

“Everything just works out of the box.”

abstraction hides complexity
failures are exceptions, not expectations
friction is treated as a bug to eliminate
productivity = seamless continuity
DevOps worldview (operational realism)

“Everything is broken, but can be made to work—if you know the right steps.”

systems are assumed to be degraded or drifting
failure is normal state, not edge case
friction is information, not just noise
productivity = ability to recover, not avoid failure

Put together, the collision is almost philosophical:

IDEs train trust in systems.
DevOps trains distrust in systems—but confidence in repair.

Or more pointedly:

...

IDE mindset

“After a successful connection to the server, the green light turns on.”

Everything is reduced to a single observable state:

connect → success indicator
abstraction hides the machinery
success is visible and binary
DevOps mindset

“For a successful remote node connection: route must exist, firewall must allow traffic, service must be running, DNS must resolve, credentials must be valid, and nothing upstream must be silently broken.”

Success is not a state—it’s a chain of fragile preconditions


IDE mindset (tool-bound knowledge)

“Sorry, I can’t help you—I only know how to do it in X. No idea about Y.”

knowledge is tool-specific
workflows are embedded in the IDE’s ecosystem
switching tools = switching mental universe
expertise is “deep but narrow”

The implicit assumption:

“Correct solution = correct tool”

DevOps mindset (tool-agnostic execution)

“There are usually three or four ways to do this—depends what you’re allowed to touch.”

knowledge is system-centric, not tool-centric
tools are interchangeable interfaces to the same reality
constraints determine method, not preference
expertise is “portable and redundant”

The implicit assumption:

“Correct solution = whatever works under current constraints”

### Background (Non MCP)



Claim offer
in russian language it is interesting to observe high popularity of vague "pizza delivery operation-style" verbs used very often in misc. technical discussions but seemingly very poor on conveyed content: one may easily build joke sound phtarses from those verbs, funny sound, but without particular meaning other than indicating the subject has no idea:

дергаем накидываем подтягиваем и пробрасываем

You’re noticing a real linguistic pattern, not just a meme.

Those verbs — дергаем, накидываем, подтягиваем, пробрасываем — belong to a broader class of low-specificity operational verbs in Russian tech slang. They give a sense of activity and control, but carry almost no concrete information unless the context is already shared.

What’s going on under the hood:

1. Aspect without substance
Russian is very rich in verbs of action and direction. Prefixes like под-, про-, на- add a feeling of motion or transformation:

подтянуть → “pull up / improve”

пробросить → “pass through / forward”

накинуть → “throw on / add quickly”

But in tech talk, these often become placeholders:

“ну тут просто пробрасываем порт и подтягиваем конфиг”

Sounds competent, explains nothing.

2. Illusion of workflow
They mimic pipeline thinking:

something comes in → дергаем

something gets added → накидываем

something gets fixed → подтягиваем

something gets passed along → пробрасываем

It feels like a process description, but avoids specifics like:

what exactly?

where?

how?

3. Social function
These verbs are often used when:

the speaker doesn’t fully understand the mechanism

or assumes the listener already does

or wants to avoid going into detail

So instead of precision, you get shared vagueness as a bonding mechanism.

4. Why it’s especially common in Russian tech speech
Russian informal technical language heavily favors:

metaphorical motion verbs

compression (dropping objects, subjects)

context-heavy phrasing

Compared to English, which leans more on nouns:

“we map the port, inject config, forward traffic”

Russian prefers:

“порт пробрасываем, конфиг накидываем”

Same idea, but much easier to make empty.

Your “pizza delivery style” analogy is actually spot-on:

“подкинули, закинули, прокинули — всё, готово”

It sounds like logistics, not engineering.

If you push this to parody, you get sentences that are syntactically valid but meaningless:

“сначала дергаем сервис, потом накидываем слой, дальше подтягиваем зависимости и пробрасываем результат”

It sounds like architecture. It’s basically noise.

There’s a rough English equivalent, but it’s weaker:

“we just hook this up”

“spin it up”

“wire it through”

“push it down the pipeline”

Still vague, but Russian allows much denser stacking of these verbs, which makes the effect stronger — and funnier.

If you want, I can break down how each of those verbs is actually used in precise contexts (networking vs build systems vs UI), which is where the real meaning reappears.


in russian (technical or not) is is not uncommon to attempt to convey multiple meanings in a well said phrase, ideally, opposite in nature or "not what it sounds".  btu with those pizza delivery boy volabulary phrases there is no direct meaning at all, and that is harmful to a reader

You’re right to draw that distinction. Russian absolutely has a tradition of layered phrasing—where a sentence can carry irony, reversal, or even a quiet “opposite meaning.” But those “pizza-delivery verbs” are something else entirely.

They’re not ambiguous in a rich way — they’re underspecified to the point of being empty.

Two very different phenomena
1) High-context, layered Russian speech (valuable)
This is where meaning is compressed, not missing:

irony, understatement, double meaning

often relies on shared cultural or situational context

can even say the opposite of literal wording

Example tone:

“ну да, ‘немного поправили’”
(meaning: something was heavily reworked)

Here, the reader actually gets more than is said.

2) Operational filler verbs (harmful in tech)
Your examples:

дергаем, накидываем, подтягиваем, пробрасываем

These do the opposite:

they pretend to describe action

but hide all key variables (what, where, why, constraints)

So instead of compressed meaning, you get:

missing meaning disguised as process

Why it’s particularly bad for a reader
In technical writing or discussions, these verbs break a few critical things:

• No object clarity

“пробрасываем порт”
Which port? From where to where? Static? Dynamic? NAT? SSH tunnel?

• No boundary definition

“подтягиваем зависимости”
From where? At build time? Runtime? Cached? Version-pinned?

• No failure model
Everything sounds trivial:

“просто накинули”
— until it isn’t.

The deeper issue: false shared understanding
These phrases rely on an implicit assumption:

“you already know what I mean”

But unlike the richer Russian style you mentioned, here:

there is no second layer to recover

if you don’t already know — you’re stuck

So instead of:

implicit but reconstructable meaning

you get:

implicit and unrecoverable meaning

Why people still use them
Not just ignorance — a mix of factors:

Speed over precision (spoken standups, chats)

Cargo-culted patterns (“that’s how everyone says it”)

Defensive vagueness (avoid being pinned down)

Illusion of competence (sounds like action)

A useful test
You can expose these phrases with a simple check:

Take:

“пробрасываем и подтягиваем”

Ask:

What exactly is being moved?

Between which components?

By what mechanism?

When does it fail?

If the sentence collapses under these questions — it wasn’t informative.

Cleaner alternative style (still Russian, still natural)
Instead of:

“пробрасываем порт и накидываем конфиг”

Say:

“открываем порт 8080 на хосте и проксируем его в контейнер; конфиг добавляем через env-переменные при старте”

Still concise, but now:

objects exist

boundaries exist

mechanism exists

Bottom line
You’re pointing at a real anti-pattern:

Good Russian ambiguity → adds meaning beyond words

“Pizza verbs” → remove meaning while sounding technical

So yes — for a reader (especially outside the author’s immediate context), they’re not just annoying, they actively degrade understanding.

If you want, we can map common ones (дергать, пробрасывать, накидывать) to their precise equivalents in networking / Docker / CI, which is often where this habit becomes most dangerous.


it raises a concern - how do i talk to a peson who us speaking this lanfguage? i doubt i will

You probably will have to talk to them at some point—but not on their terms.

Trying to “decode” that style conversationally is a losing game. The trick is to force the conversation out of vague verbs and into concrete objects, without turning it into a confrontation.

First, accept what’s really happening
When someone says:

“ну тут дергаем, потом пробрасываем и чуть подтягиваем”

There are only two possibilities:

They actually know, but are speaking lazily

They don’t know in detail

In both cases, you won’t get clarity unless you change the shape of the conversation

Practical strategy that works
Don’t challenge the verbs.
Anchor on missing parameters.

Instead of:

“what do you mean ‘пробрасываем’?”

Ask:

“откуда → куда?”

“через что?”

“на каком этапе?”

Short, almost mechanical questions.

This does two things:

forces specificity

avoids sounding confrontational

Even better: switch to restating
Take their vague sentence and rebuild it concretely:

“правильно ли я понял: порт 8080 открыт на хосте и через nginx проксируется в контейнер?”

Now one of two things happens:

they confirm → you got clarity

they correct → you extract real info

Either way, you win.

If they keep staying vague
That’s your signal: stop trying to “discuss,” start pinning decisions.

Use constraints:

“какой именно порт?”

“это на уровне docker или приложения?”

“это делаем на старте или вручную?”

You’re essentially converting:

verbs → architecture

When it’s not worth engaging
You’re right to be skeptical. Sometimes it’s just noise.

Red flags:

answers remain verb-only after 2–3 clarifications

no nouns appear (ports, files, services, endpoints)

everything is “просто” and “чуть-чуть”

At that point:

don’t debate — ask for artifacts

config

command

diagram

repo link

Because if it’s real, it exists somewhere concretely.

The mindset shift
You’re not “talking their language.”
You’re translating it into engineering.

Think of their speech as:

lossy compression

Your job is:

reconstruct the missing data or reject the packet

Bottom line
You don’t need to avoid these people—you just need a different interface:

ignore the verbs

extract the nouns

force boundaries

confirm with restatement

If they can’t operate under that—then your doubt is justified, and the issue isn’t language, it’s competence.




### See Also 
  * [Model Context Protocol (MCP) with Spring AI in Minutes](https://www.youtube.com/watch?v=MarSC2dFA9g)
  * [avaialble MCP SDKs](https://modelcontextprotocol.io/docs/sdk#available-sdks)
  * [MCP spec](https://modelcontextprotocol.io/specification/2025-06-18)
  * [MCP basic](https://habr.com/ru/articles/960538/)( in Russian )

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
