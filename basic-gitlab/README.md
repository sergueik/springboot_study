### Info

Ths directory contains replica of [orangootan/gitlab-alpine-docker-image](https://github.com/ring0club/gitlab-alpine-docker-image) gitlab single node installation compiled from source gitlab running on alpine 3.9.5

### Usage
* build server

```sh
docker build -t gitlab-server -f Dockerfile.server .
```
* build agent

```sh
docker build -t gitlab-agent -f Dockerfile.agent .
```
this will give the following image sizes 

```text

java-executor                                      latest              248f672cf045        6 seconds ago       84.9MB
gitlab-runner                                      latest              e148ee2e88b5        38 minutes ago      72.4MB

```
as opposed to vendor ce 
```text
REPOSITORY                                         TAG                 IMAGE ID            CREATED             SIZE
gitlab/gitlab-runner                               latest              05cd232de647        3 days ago          335MB
gitlab/gitlab-ce                                   latest              5d8154a38693        5 days ago          3.66GB
java-runner                                        latest              c6ae0bef0e95        5 minutes ago       629MB
eclipse-temurin                                    11-jdk-alpine       a212a1b08af9        2 months ago        305MB
```

* build runner


```sh
docker exec -it gitlab grep 'Password:' /etc/gitlab/initial_root_password
```
### TLRD;
# Steamroller & Agile Metaphor – Collected Notes

## 1. Initial Steamroller Metaphor

**Steamroller traits:**
- Large & heavy → big organization, many dependencies
- Straight-line torque → strong strategic direction
- Not designed for turning → hard to pivot frequently
- Flattens everything → standardization, removes variability
- Leaves surface flat → predictable, stable output
- Loud, dramatic → visible transformation

**Agile traits:**
- Small, nimble → short feedback loops
- Turns constantly → inspect & adapt
- Local autonomy → teams decide how to move

**Insight:**
> Steamroller sets the road. Agile steers within the lane.

---

## 2. Principle: Steamroller at Strategy, Agile at Execution

- **Steamroller (strategy level):**
  - Clear long-term direction
  - Architectural standards
  - Compliance & security
  - Platform and tooling choices
  - Release cadence
- **Agile (execution level):**
  - Sprints = micro-steering
  - Retros = suspension system
  - Backlog refinement = obstacle detection
  - MVPs = test patches
  - Feature flags = cones on the road

**Framing:**
> Steamroller Agile: strategic rigidity with tactical adaptability.

**Dramatic expressions:**
- “A massive engine of torque, driven by quarterly goals, but steered by two-week experiments.”
- “The steamroller provides momentum. Agile provides intelligence.”

---

## 3. Puzzle / Riddle for Scrum Aces

- *Innocent question:*
  > “If Agile is about turning quickly, how do we practice Agile in a system that is structurally a steamroller — heavy, straight, and designed not to pivot?”

- *Dramatic metaphor:*
  > “Our organization is not a bicycle. It’s a steamroller: huge torque, low agility, and a job to flatten the road. Where exactly do you put the steering wheel?”

- *Trick answer:*
  > “Maybe Agile is not about turning the steamroller. Maybe it’s about teaching it to listen.”

- *Zen punch:*
  > “Agile without torque is chaos. Torque without Agile is tyranny. The art is in their uneasy marriage.”

---

## 4. Steamroller as Individual Mastery

- Bicycle developer → stops at obstacles → writes stories → repeats 10×
- Steamroller developer → anticipates obstacles → flattens them continuously → issues “already solved” by the time they’re defined

**Key lesson:**
> Skill can substitute for process. Mastery can reduce the need for frequent iteration.

**Dramatic articulation:**
- “We design processes for bicycles: turn often, balance carefully, avoid rocks. But what if the developer is a steamroller? Why zig-zag at all, if they can flatten the terrain from A to Z?”
- Short punchy:
  > “Agile waits for the issue to be defined. Steamrollers define it in advance, then flatten it.”

**Philosophical interpretation:**
- Junior dev → bicycle (needs frequent correction)
- Senior dev → truck
- Master dev → steamroller
- Master dev handles many hidden layers in one pass → “Extreme Agile inside one brain”

**Poetic versions:**
- “Agile teaches us how to turn. Mastery teaches us when turning is unnecessary.”
- “A bicycle adapts to the road. A steamroller creates the road.”

---

## 5. Persistence & Emergent Results

- Steamroller metaphor extended beyond mental foresight:
  - Not only anticipatory, but also **persistent investment over time**
  - Extra effort accumulates layer by layer until the path is smooth
  - Emergent results appear by review time (e.g., midnight)

**Refined dramatic version:**
- “Bicycles wait for each story to be defined. Steamrollers keep moving, hour by hour, layer by layer. By the time the clock strikes midnight, the path from A to Z is already flattened.”
- Punchy:
  > “A steamroller solves the obstacles while the world writes them down.”

**Insight:**
> Persistence + continuous flattening → issues solved before they’re formally recognized.

---

## 6. Summary / Lessons

1. **Steamroller = strategic, high-torque, persistent effort**
2. **Agile = tactical adaptation and feedback loops**
3. Highly skilled developers can operate as “steamrollers”:
   - Flatten multiple layers of obstacles at once
   - Produce emergent results ahead of formal process
   - Reduce the need for iterative pivots
4. The metaphor bridges:
   - Anticipatory skill
   - Persistent effort
   - Process-aware execution
5. Dramatic and puzzle versions make it memorable for Scrum teams.

---

## 7. Optional Puzzle / Slide Ideas

- **One-slide visual:**
  - Top: straight steamroller path
  - Bottom: zig-zag bicycle path
  - Annotate: “Emergent results vs iterative process”

- **Riddle format:**
  > “Agile assumes developers are bicycles — they must turn constantly to survive the terrain.
  > But what if a developer is a steamroller? Is Agile still necessary, or does mastery replace maneuvering?”

- **One-paragraph executive version:**
  > “Some developers operate like steamrollers: they see the path from A to Z, flatten obstacles proactively, and produce emergent results by the time others define the issue. Agile supports them, but their skill and persistence reduce the need for constant iteration.”

### See Also

  * https://github.com/alpinelinux/alpine-docker-gitlab/
  * https://docs.gitlab.com/runner/install/docker.html
  * https://docs.gitlab.com/install/docker/installation/

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
