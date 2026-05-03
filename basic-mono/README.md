### Usage

compiling and running the [basic-httpserver](https://github.com/sergueik/powershell_samples/tree/master/external/csharp/basic-httpserver) (a replica of [httpServer](https://github.com/qinyuanpei/HttpServer) with some fixes) on [Mono](https://www.mono-project.com/)

> NOTE: the original project also managed (?) to install and run [Nunit](https://docs.nunit.org/) [Console Test](https://github.com/nunit/nunit-console)

```sh
IMAGE=basic-mono
docker pull ubuntu:22.04
docker build -f Dockerfile -t $IMAGE .
```
followed by
```sh
NAME=$IMAGE
docker run --name $NAME -d -p 4050:4050 $IMAGE
```
```sh
echo '{"foo": "bar" }' > ./data.json
```
```sh
docker cp data.json $NAME:/tmp/
```
```text
Successfully copied 2.05kB to c9df5669806f:/tmp/app
```
```sh
curl -s -H 'Content-Type: application/json' http://localhost:4050/data.json
```

```json
{"foo": "bar"}
```

### Cleanup
```sh
docker container stop $NAME
docker container prune -f
docker image prune -f 
docker image rm $IMAGE
```


### Troubleshooting

```text
ERROR] FATAL UNHANDLED EXCEPTION: System.DllNotFoundException: urlmon.dll assembly:<unknown assembly> type:<unknown type> member:(null)
  at (wrapper managed-to-native) Utils.ResponseHelper.FindMimeFromData(intptr,string,byte[],int,string,int,intptr&,int)
  at Utils.ResponseHelper.GetMimeFromFile (System.String filePath) [0x00063] in <3befbba5c0784c24bfed9fefb3c22509>:0 
  at Utils.ResponseHelper.FromFile (Utils.HttpResponse response, System.String fileName) [0x00034] in <3befbba5c0784c24bfed9fefb3c22509>:0 
  at Program.ExampleServer.OnGet (Utils.HttpRequest request, Utils.HttpResponse response) [0x00056] in <bda7b169b55f46d099e918b96df97ff8>:0 
  at Utils.HttpServer.ProcessRequest (System.Net.Sockets.TcpClient handler) [0x00064] in <3befbba5c0784c24bfed9fefb3c22509>:0
```

Need to implement `MIME` mapping in pure __.Net__ - worked around with a  stub

### Background Information - Not Related to Mono
in russian language it is interesting to observe high popularity of vague "pizza delivery operation-style" verbs used very often in misc. technical discussions but seemingly very poor on conveyed content: one may easily build joke sound phtarses from those verbs, funny sound, but without particular meaning other than indicating the subject has no idea: дергаем накидываем подтягиваем и пробрасываем
You’re noticing a real linguistic pattern, not just a meme.
Those verbs — дергаем, накидываем, подтягиваем, пробрасываем — belong to a broader class of low-specificity operational verbs in Russian tech slang. They give a sense of activity and control, but carry almost no concrete information unless the context is already shared.
What’s going on under the hood:
1. Aspect without substance
Russian is very rich in verbs of action and direction. Prefixes like под-, про-, на- add a feeling of motion or transformation:
    • подтянуть → “pull up / improve”
    • пробросить → “pass through / forward”
    • накинуть → “throw on / add quickly”
But in tech talk, these often become placeholders:
“ну тут просто пробрасываем порт и подтягиваем конфиг”
Sounds competent, explains nothing.
2. Illusion of workflow
They mimic pipeline thinking:
    • something comes in → дергаем
    • something gets added → накидываем
    • something gets fixed → подтягиваем
    • something gets passed along → пробрасываем
It feels like a process description, but avoids specifics like:
    • what exactly?
    • where?
    • how?
3. Social function
These verbs are often used when:
    • the speaker doesn’t fully understand the mechanism
    • or assumes the listener already does
    • or wants to avoid going into detail
So instead of precision, you get shared vagueness as a bonding mechanism.
4. Why it’s especially common in Russian tech speech
Russian informal technical language heavily favors:
    • metaphorical motion verbs
    • compression (dropping objects, subjects)
    • context-heavy phrasing
Compared to English, which leans more on nouns:
    • “we map the port, inject config, forward traffic”
Russian prefers:
    • “порт пробрасываем, конфиг накидываем”
Same idea, but much easier to make empty.

Your “pizza delivery style” analogy is actually spot-on:
“подкинули, закинули, прокинули — всё, готово”
It sounds like logistics, not engineering.

If you push this to parody, you get sentences that are syntactically valid but meaningless:
“сначала дергаем сервис, потом накидываем слой, дальше подтягиваем зависимости и пробрасываем результат”
It sounds like architecture. It’s basically noise.

There’s a rough English equivalent, but it’s weaker:
    • “we just hook this up”
    • “spin it up”
    • “wire it through”
    • “push it down the pipeline”
Still vague, but Russian allows much denser stacking of these verbs, which makes the effect stronger — and funnier.

If you want, I can break down how each of those verbs is actually used in precise contexts (networking vs build systems vs UI), which is where the real meaning reappears.
in russian (technical or not) is is not uncommon to attempt to convey multiple meanings in a well said phrase, ideally, opposite in nature or "not what it sounds". btu with those pizza delivery boy volabulary phrases there is no direct meaning at all, and that is harmful to a reader
You’re right to draw that distinction. Russian absolutely has a tradition of layered phrasing—where a sentence can carry irony, reversal, or even a quiet “opposite meaning.” But those “pizza-delivery verbs” are something else entirely.
They’re not ambiguous in a rich way — they’re underspecified to the point of being empty.
Two very different phenomena
1) High-context, layered Russian speech (valuable)
This is where meaning is compressed, not missing:
    • irony, understatement, double meaning
    • often relies on shared cultural or situational context
    • can even say the opposite of literal wording
Example tone:
“ну да, ‘немного поправили’”
(meaning: something was heavily reworked)
Here, the reader actually gets more than is said.

2) Operational filler verbs (harmful in tech)
Your examples:
дергаем, накидываем, подтягиваем, пробрасываем
These do the opposite:
    • they pretend to describe action
    • but hide all key variables (what, where, why, constraints)
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
    • there is no second layer to recover
    • if you don’t already know — you’re stuck
So instead of:
implicit but reconstructable meaning
you get:
implicit and unrecoverable meaning

Why people still use them
Not just ignorance — a mix of factors:
    • Speed over precision (spoken standups, chats)
    • Cargo-culted patterns (“that’s how everyone says it”)
    • Defensive vagueness (avoid being pinned down)
    • Illusion of competence (sounds like action)

A useful test
You can expose these phrases with a simple check:
Take:
“пробрасываем и подтягиваем”
Ask:
    • What exactly is being moved?
    • Between which components?
    • By what mechanism?
    • When does it fail?
If the sentence collapses under these questions — it wasn’t informative.

Cleaner alternative style (still Russian, still natural)
Instead of:
“пробрасываем порт и накидываем конфиг”
Say:
“открываем порт 8080 на хосте и проксируем его в контейнер; конфиг добавляем через env-переменные при старте”
Still concise, but now:
    • objects exist
    • boundaries exist
    • mechanism exists

Bottom line
You’re pointing at a real anti-pattern:
    • Good Russian ambiguity → adds meaning beyond words
    • “Pizza verbs” → remove meaning while sounding technical
So yes — for a reader (especially outside the author’s immediate context), they’re not just annoying, they actively degrade understanding.
If you want, we can map common ones (дергать, пробрасывать, накидывать) to their precise equivalents in networking / Docker / CI, which is often where this habit becomes most dangerous.
it raises a concern - how do i talk to a peson who us speaking this lanfguage? i doubt i will
You probably will have to talk to them at some point—but not on their terms.
Trying to “decode” that style conversationally is a losing game. The trick is to force the conversation out of vague verbs and into concrete objects, without turning it into a confrontation.
First, accept what’s really happening
When someone says:
“ну тут дергаем, потом пробрасываем и чуть подтягиваем”
There are only two possibilities:
    1. They actually know, but are speaking lazily
    2. They don’t know in detail
In both cases, you won’t get clarity unless you change the shape of the conversation

Practical strategy that works
Don’t challenge the verbs.
Anchor on missing parameters.
Instead of:
“what do you mean ‘пробрасываем’?”
Ask:
    • “откуда → куда?”
    • “через что?”
    • “на каком этапе?”
Short, almost mechanical questions.
This does two things:
    • forces specificity
    • avoids sounding confrontational

Even better: switch to restating
Take their vague sentence and rebuild it concretely:
“правильно ли я понял: порт 8080 открыт на хосте и через nginx проксируется в контейнер?”
Now one of two things happens:
    • they confirm → you got clarity
    • they correct → you extract real info
Either way, you win.

If they keep staying vague
That’s your signal: stop trying to “discuss,” start pinning decisions.
Use constraints:
    • “какой именно порт?”
    • “это на уровне docker или приложения?”
    • “это делаем на старте или вручную?”
You’re essentially converting:
verbs → architecture

When it’s not worth engaging
You’re right to be skeptical. Sometimes it’s just noise.
Red flags:
    • answers remain verb-only after 2–3 clarifications
    • no nouns appear (ports, files, services, endpoints)
    • everything is “просто” and “чуть-чуть”
At that point:
don’t debate — ask for artifacts
    • config
    • command
    • diagram
    • repo link
Because if it’s real, it exists somewhere concretely.

The mindset shift
You’re not “talking their language.”
You’re translating it into engineering.
Think of their speech as:
lossy compression
Your job is:
reconstruct the missing data or reject the packet

Bottom line
You don’t need to avoid these people—you just need a different interface:
    • ignore the verbs
    • extract the nouns
    • force boundaries
    • confirm with restatement
If they can’t operate under that—then your doubt is justified, and the issue isn’t language, it’s competence.


