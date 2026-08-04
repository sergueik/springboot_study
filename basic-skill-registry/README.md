###

Under the current standard, Agent Skills are SKILL.md files
that combine instructions with supporting resources, enabling Large Language Model (LLM) agents to reuse procedures beyond a single conversation. 

Recently, AI skills have become a practical way to extend
Large Language Model (LLM) agents beyond the immediate
promp

Under the Agent Skills standard, a skill is packaged as a SKILL.md file: frontmatter tells the agent when to
load it, the body gives instructions, and optional files provide
scripts, references, or assets [2]. In this form, skills turn task
experience into reusable software artifacts
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

