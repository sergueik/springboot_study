### Info


## What are Agent Skills?

[Agent Skills](https://agentskills.io/home) are a standardized way to give AI agents new capabilities and domain expertise.

Under the Agent Skills standard, every skill includes a required `SKILL.md` file that combines structured metadata
with human-readable instructions. This allows Large Language Model (LLM) agents to reuse knowledge and procedures beyond a single conversation.

A `SKILL.md` consists of two main parts:

- **frontmatter** – written in YAML – describes the skill and tells the agent when it should be used - uses YAML
- **body** – written in [Markdown](https://en.wikipedia.org/wiki/Markdown) ; contains instructions, procedures, examples, and references that guide the agent

> YAML is used for structured metadata that the agent can efficiently discover and index.
> Markdown is used for the human-readable instructions because it is a lightweight, widely supported plain-text format that
> is easy to author, review, version-control, and diff.
> Markdown is already a de facto enterprise standard. We're not introducing a niche format; we're reusing one that our existing tools already understand.


  * VS Code has native Markdown preview.
  * IntelliJ IDEA has native Markdown support.
  * GitHub renders Markdown.
  * GitLab renders Markdown.
  * Azure DevOps renders Markdown.
  * Most code review systems treat Markdown as a first-class format.
  * It is plain UTF-8 text, so it diffs cleanly in Git.

> one is no longer asking to adopt __Markdown__  one are pointing out that they are already surrounded by it.

A `SKILL.md` file has properties that align naturally with engineering workflows:

* it needs version control;
* changes need to be reviewable;
* a history of modifications matters;
* branching and merging may matter;
* automated tooling needs to parse it;
* agents need a predictable structure.

__Agents__ load __skills__ through progressive disclosure, in three stages to minimize context usage:

|       |             |
|-------|-------------|
| **Discovery** | At startup, the agent reads only the name and description of each available skill |
| **Activation** | When a task matches a skill's description, the agent loads the full `SKILL.md` into its context |
| **Execution** | The agent follows the instructions, loads referenced files as needed, and, when applicable, executes bundled code. Most skills consist solely of documentation and supporting resources. |

Because complete instructions are loaded only when needed, an agent can have access to thousands of skills
while consuming only a small amount of context

### Example

The YAML frontmatter describes the skill metadata:

```yaml
---
name: code-reviewer
description: Reviews code for bugs, security issues, and style violations. Use when the user asks to review code or check a pull request.
---
```

The Markdown body contains the actual guidance:

````markdown
# Spring Boot Testing

This skill provides guidance for testing Spring Boot 4 applications using modern patterns and best practices.

## Core Principles

1. Test Pyramid
2. Use the narrowest test slice that provides confidence.
3. Prefer AssertJ for fluent assertions.
4. Prefer MockMvcTester and RestTestClient for modern Spring Boot testing.
...
```
Optionally, a skill may include additional files containing scripts, templates, reference material, datasets, or other supporting assets.

Recently, AI skills recognition  have become explosive 

Agent Skills have rapidly emerged as a practical way to extend Large Language Model (LLM) agents beyond the immediate prompt. Rather than repeating detailed instructions in every conversation, developers can package reusable expertise into portable skills.
Tools developed to convert other classig presentation formats: Video, PDF, etc. into Skills are highly popular.

### Info

[Agent Skills](https://agentskills.io/home) is standardized way to give AI agents new capabilities and expertise. 

Under the current standard, Agent Skills are `SKILL.md` files that combine instructions with supporting resources, enabling Large Language Model (LLM) agents to reuse procedures beyond a single conversation. There is an YAML area and markdown area in the `SKILL.md`, that is the findamental compomnent.

__Agents__ load __skills__ through progressive disclosure, in three stages:

|          |                                             |
|----------|---------------------------------------------|
|Discovery | At startup, agents load only the name and description of each available skill, just enough to know when it might be relevant|
|Activation | When a task matches a skill’s description, the agent reads the full SKILL.md instructions into context|
|Execution | The agent follows the instructions, optionally executing bundled code or loading referenced files as needed|

Tactically, full instructions load only when a task calls for them, thus agents can keep many skills on hand with only a small context footprint.

Recently, AI skills have become widelyy recognized as a highly practical way to extend Large Language Model (LLM) agents beyond the immediate prompt.

Under the Agent Skills standard, a skill at a minimum is packaged as a `SKILL.md` file: frontmatter 
```yaml
---
name: code-reviewer
description: Reviews code for bugs, security issues, and style violations. Use when the user asks to review code or check a pull request.
---

```
tells the agent when to load it, the body gives instructions, and optional files provide scripts, references, or assets. In this form, skills turn task experience into reusable software artifacts
### Usage
```sh
git clone --depth 1 https://github.com/majiayu000/claude-skill-registry/tree/main/skills
pushd claude-skill-registry
find skills -iname 'SKILL.md' | tee ../catalog.txt 
popd
```

```powershell
. ./catlog-rebuilder.ps1
```
it generates the flat HTML table with search.

### Background


"Traditional" code assistant aids focused on helping the developer write code *faster*:

* **IntelliSense** — suggests answers to the natural developer question: *"What can we do with this?"*
  A developer-oriented echo of the broader 1990s software-era message captured by Microsoft's famous campaign:
  **"Where do you want to go today?"**
* **Refactoring** — structural code changes such as extracting methods, creating classes, and reorganizing code safely.
* **Syntax highlighting** — visual structure assistance (color vision matters).
* **Static analysis** — detecting potential defects before execution.
* **Code coverage** — measuring which parts of the code are exercised by tests.

In a sharp turn, modern AI coding assistants often spend a surprising amount of time helping the developer *avoid* writing code manually at all:

* "generate this class"
* "create the API client"
* "write the unit tests"
* "convert this function"
* "explain this repository"
* "make this configuration"
The conceptual transition is actually very strong:

Traditional assistants:

"Help me write what I already know I need."

AI assistants:

"Help me produce something without manually writing every detail."

That makes the next section about SKILL.md almost inevitable, because once the assistant is producing more of the artifact, the critical question becomes:

"How does the assistant know the way this project expects things to be done?"


### See Also

  * https://openreview.net/pdf/3a0ffc73b487443feb8f2abdacbf3200299cf797.pdf
  * [Agent Skills Specification](https://agentskills.io/specification) - complete format specification for Agent Skill
  * [vibe coding cases overview](https://habr.com/ru/articles/1065582/) (in Russian)

---  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
### Usage
```sh
git clone --depth 1 https://github.com/majiayu000/claude-skill-registry/tree/main/skills
pushd claude-skill-registry
find skills -iname 'SKILL.md' | tee ../catalog.txt 
popd
```

```powershell
. ./catlog-rebuilder.ps1
```
it generates the flat HTML table with search.

### Background


"Traditional" code assistant aids focused on helping the developer write code *faster*:

* **IntelliSense** — suggests answers to the natural developer question: *"What can we do with this?"*
  A developer-oriented echo of the broader 1990s software-era message captured by Microsoft's famous campaign:
  **"Where do you want to go today?"**
* **Refactoring** — structural code changes such as extracting methods, creating classes, and reorganizing code safely.
* **Syntax highlighting** — visual structure assistance (color vision matters).
* **Static analysis** — detecting potential defects before execution.
* **Code coverage** — measuring which parts of the code are exercised by tests.

In a sharp turn, modern AI coding assistants often spend a surprising amount of time helping the developer *avoid* writing code manually at all:

* "generate this class"
* "create the API client"
* "write the unit tests"
* "convert this function"
* "explain this repository"
* "make this configuration"
The conceptual transition is actually very strong:

Traditional assistants:

"Help me write what I already know I need."

AI assistants:

"Help me produce something without manually writing every detail."

That makes the next section about SKILL.md almost inevitable, because once the assistant is producing more of the artifact, the critical question becomes:

"How does the assistant know the way this project expects things to be done?"


### See Also

  * https://openreview.net/pdf/3a0ffc73b487443feb8f2abdacbf3200299cf7o97.pdf
  * [Agent Skills Specification](https://agentskills.io/specification) - complete format specification for Agent Skill
  * [vibe coding cases overview](https://habr.com/ru/articles/1065582/) (in Russian)
  * https://www.markdownguide.org/basic-syntax/
  * https://www.markdownguide.org/extended-syntax/
---  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
