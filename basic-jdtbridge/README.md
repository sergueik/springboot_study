### Info

Replica of the [JDT Bridge](https://github.com/kaluchi/jdtbridge) project
discoverd after the [article](https://habr.com/ru/articles/1008274/) (in Russian)
### Backgound

Eclipse [JDT](https://projects.eclipse.org/projects/eclipse.jdt) (__Java Development Tools__) is a set of plugins for the Eclipse Foundation platform that transforms it into a fully functional Java IDE.


### See Also
  * [JDT Tutorial: Creating Eclipse Java Projects Programmatically](https://sdq.kastel.kit.edu/wiki/JDT_Tutorial:_Creating_Eclipse_Java_Projects_Programmatically)
  * [JDT Plug-in Developer Guide](https://help.eclipse.org/latest/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Fguide%2Fjdt_int.htm)
  * https://github.com/kaluchi/jdtbridge/issues


### Rebase First Note

Reviewer etiquette (important)

It should not be dogmatic like:

“Always rebase before I review”

Better:

“Please rebase to ensure it works with latest changes”

Even better:

Let CI simulate merge (many systems do this automatically now)

🧩 Bottom line
❌ Not a hard requirement
✅ A risk-reduction practice

⚠️ Git being “able to merge” ≠ “safe to merge”

“Rebasing before merge is not strictly required by Git, but some teams (or individuals) strongly prefer it based on past incidents where complex merges led to hard resets or history rewrites.”

If someone has witnessed:

a *bad merge into main*

followed by a __forced history rewrite__ (git push --force)

and the fallout (broken clones, lost commits, confused CI, etc.)

…then their instinct to say “keep branches clean and rebased” is not theoretical — it’s defensive engineering.  this was a hard-way experience that can’t be un-learned”

Modern Git hosting systems (e.g. GitHub, GitLab, Bitbucket) do provide branch protection rules and granular permissions, but they are not purely “Git itself” features — they’re enforced by the hosting layer around Git.
Git by itself  is intentionally permissive at the protocol level:

Does not enforce permissions
Does not understand “main branch protection”

Will happily accept: 'push -force'

### UI-boundedness

How strongly the tool forces developer into a GUI vs letting one operate as code/CLI/text.
It’s actually a very useful mental model, especially the one who care about CI/CD, diffability, and *escaping the tool*

#### 🧭 UI-boundedness spectrum (high → low)
🟠 Postman

Strong GUI-first workflow (collections, tabs, environments)

Heavy reliance on its app + cloud workspace

CLI (Newman) exists but is secondary

Collaboration features are UI-centric

👉 Verdict:

“You live inside the UI, automation is an export.”

🟠 SoapUI

Also GUI-heavy (drag-and-drop test building)

More “enterprise GUI IDE” feel than Postman

Supports scripting, but UI is still dominant

👉 Verdict:

Slightly more scriptable than Postman, but still very UI-anchored.

2. Hybrid (UI + code, but UI still visible)

(None of your four sits perfectly here, but worth noting the gap)

This is where tools like Insomnia / Apidog sit — not your question, but useful anchor:

UI is optional-ish

Data model closer to files/specs


🟡 Bruno

Low UI-bound (text-first, UI optional/minimal). Stores requests as plain text files (Git-friendly)

CLI-oriented workflows

UI exists but is intentionally thin

From community sentiment:

“collections are just plain text, easy to keep in Git”

👉 Verdict:

“UI is just a viewer/editor — not the source of truth.”


🟢 Karate

Least UI-bound (code as the primary interface)

No real dependency on UI at all

Tests written in Gherkin DSL

Runs via CLI / build tools (Maven, Gradle)

UI (if any) is just reporting

Karate explicitly uses text-based BDD syntax

👉 Verdict:

“There is no UI — only code that happens to test APIs.”

🩰 “GUI choreography” (sharp definition)

GUI choreography = the non-transferable sequence of UI interactions required to create, modify, or execute test artifacts, which cannot be fully captured, versioned, or replayed outside the tool.

Examples:

“click collection → add request → open Tests tab → paste script → save”

“right-click project → new test step → configure assertion dialog → OK”

👉 If that sequence cannot be expressed as code or data, it is choreography.

☠️ “Wasted effort” (your deadly metric)

Let’s define:

Wasted effort % = proportion of user effort spent on non-portable, non-automatable UI interactions

This includes:

Navigation (menus, tabs, dialogs)

Context switching inside UI

Repeating actions that could be scripted

Re-learning the same flows in another tool

It excludes:

Actual test logic

Data modeling

Assertions (if portable)

📊 Ranking with GUI choreography + wasted effort
🔴 1. Postman

GUI choreography: Extreme

Tabs (Params / Headers / Body / Tests)

Hidden execution model (pre-request vs tests)

Environment switching via UI

Wasted effort: ~60–75%

Why so high:

Collections look exportable, but creation/editing is UI-driven

Many flows are muscle memory, not reproducible artifacts

Switching tools = relearning entire “dance”

👉 Lock-in mechanism:

The workflow is the product, not just the data.

🔴 2. SoapUI

GUI choreography: Very high (arguably worse)

Tree navigation + modal dialogs everywhere

Assertion builders, property transfers, etc.

Wasted effort: ~65–80%

Why worse than Postman:

Heavier “IDE-like” ceremony

More steps per action (classic enterprise GUI syndrome)

👉 Lock-in mechanism:

“Right-click → wizard → dialog” becomes institutional knowledge.

🟡 3. Bruno

GUI choreography: Low

UI is mostly a thin layer over text files

Actions map closely to file edits

Wasted effort: ~10–25%

Why much lower:

You can bypass UI entirely

Git is the source of truth

👉 Lock-in resistance:

You’re learning a data format, not a dance.

🟢 4. Karate

GUI choreography: Near zero

No required UI

Everything is code / DSL

Wasted effort: ~0–10%

What remains:

Maybe report viewing UI

Occasional IDE assistance

👉 Lock-in resistance:

You’re learning concepts + syntax, fully portable.

🧠 The key insight you’re getting at
GUI choreography creates muscle-memory lock-in

Not just:

“I know Postman”

But:

“I know how to move inside Postman”

That knowledge is:

❌ not versioned

❌ not reviewable in PR

❌ not automatable

❌ not transferable

🧱 Why this becomes a real engineering cost
1. Onboarding tax

New dev must learn:

Tool

AND its choreography

2. Migration paralysis

Switching tools means:

Rebuilding artifacts

Relearning workflows

Losing speed for weeks

3. CI/CD impedance mismatch

GUI choreography cannot run in:

Docker

Headless CI

Minimal environments

So you get:

“export → translate → patch → pray”

4. PR review blindness

You cannot review:

“clicked this tab”

“added assertion via dialog”

Only the result, not the process

⚖️ Your “ballet master” analogy — refined

Tools like Postman and SoapUI don’t just give you a language —
they train you in a choreography under a specific ballet master.

Switching tools is not:

learning new syntax

It is:

retraining muscle memory

🔥 Final distilled model

You can summarize everything as:

Tool	Choreography	Wasted Effort	Lock-in Type
Postman	🩰🩰🩰🩰	60–75%	Workflow lock-in
SoapUI	🩰🩰🩰🩰🩰	65–80%	Ceremony lock-in
Bruno	🩰	10–25%	Format familiarity
Karate	—	0–10%	Conceptual only

If you want, we can take this one step further and turn this into a PR review argument or architectural guideline — this framing is actually very effective in pushing teams away from UI-heavy tooling without sounding dogmatic.

and note the "shocking fact that in the ultra UI heavy IDE like VS Code and  IDEA Injellij some times the instruction is  pure vi style: to start with a keyboard shortcut

Yes — and that “shocking fact” actually strengthens your whole GUI choreography argument rather than contradicting it.

⚡ The paradox you’re pointing at

Even in ultra GUI-heavy IDEs like:

Visual Studio Code

IntelliJ IDEA

…the most efficient instructions often start with:

“Press ⌘/Ctrl + Shift + …”

That’s basically **vi-style thinking sneaking back in through the side door.


*someone who assumed the door would be open—but not for him*

Anecdotal illustration: SAP ABAP editor (SE38 / SE80)

In the SAP ecosystem (e.g., via SAP GUI), ABAP development is done inside tools like SE38 / SE80.

In SAP-style systems:

The cost per operation is higher (navigation, dialogs, context switching)

But the scope per operation tends to stay small

So you get:

High choreography cost × small, bounded code changes

Which results in a workflow where:

Each “dance” produces a relatively small delta measured in entropy

The cumulative output per sprint still resembles a compact patch of logic


API tests are fundamentally execution artifacts, not interactive design artifacts, so the trade-offs that justify GUI-heavy IDEs don’t really apply.
API tests benefit from being small, portable, declarative bundles, optimized for automation and reproducibility, not for interactive authoring.

🧭 1. “Relic IDEs” → enforced coarse granularity

Early/legacy environments (including systems like SAP GUI) were built around:

Centralized repositories

Object-based editing (not file-based)

UI-mediated access to everything

Strong boundaries between units of work

Key property:

The UI itself limits how much you can reasonably modify at once.

This creates:

Natural caps on commit size

Slower traversal across dependencies

Less temptation (or ability) to refactor broadly in one go

So paradoxically:

The UI enforces discipline by friction, not by guidance.

⚙️ 2. Modern IDEs → removing limits, then encouraging expansion

Tools like Visual Studio Code and IntelliJ IDEA evolved in the opposite direction:

They aim to:

Remove friction in navigation

Enable cross-file refactoring

Make dependencies visible

Allow large-scale structural changes

Result:

“Find usages”, “rename symbol”, “refactor across module”

Seamless jumping across definitions

Multi-file edits in a single logical operation

Side effect:

Developers are now capable of making much larger coherent changes in one commit.

This is where your observation comes in:

The IDE no longer constrains granularity

Instead, it amplifies composability and cross-cutting edits

🧩 3. The “went too far into components” phase

Modern IDE ecosystems + frameworks encourage:

Componentization

Layering

Dependency injection

Modular architectures

This is partly driven by:

Need to manage complexity at scale

Tooling that makes cross-component work easy

Consequence:

Systems become more decomposed

Changes span multiple components

Commits naturally become multi-file and multi-layer

So instead of limiting change size:

The IDE + ecosystem enables larger, more distributed change units

🧪 4. Testing (especially API testing) pulls in the opposite direction

Now your key insight:

API testing does not benefit from components beyond minimal separation.

In frameworks like Karate or tools like Bruno:

The unit of work is small and self-contained

Tests are:

declarative

portable

environment-parameterized

No need for:

deep component hierarchies

complex UI navigation

multi-layer architectural constructs

Why components are unnecessary here:

Because API tests:

Don’t implement business logic

Don’t manage long-lived state

Don’t benefit from internal abstraction layers

They are:

Inputs → execution → assertions

⚖️ So is IDE “doomed”?

Not doomed — but misaligned depending on the domain.

The reality is:
IDEs are optimized for:

Code construction

Refactoring

Navigation across large codebases

Managing complexity in application development

API testing is optimized for:

Deterministic execution

Portability

CI/CD integration

Minimal surface area

