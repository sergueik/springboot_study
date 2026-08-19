### Background
The [New](https://www.docker.com/blog/docker-vmm-public-beta/) complete overhaul, built for performance __Docker Desktop__ architecture specifically intended to get __Docker__ out of the WSL2/Hyper-V dependency replacing it with own [VMM](https://docs.docker.com/desktop/features/vmm) Docker's own hypervisor a new first-party virtualization layer underneath.

#### Why It Matters

To run __Linux VM__ where the `dockerd` daemon is actually hosted, __Docker Desktop__ automatically creates and manages a hypervisor and manages the complicated integration of local network and filesystem. Historically __DD__ has relied on a third-party __VMM__ for this

```code
      🧱🧱containers 🧱🧱
      ───────────────────
      🤖  Docker Engine
      ───────────────────   
      🏭 Docker VMM
      ───────────────────
       Windows Host
```
Docker Desktop no longer needs to borrow somebody else's virtualization machinery to provide the one Linux VM in which its Docker Engine lives

#### Prerequisites
At a minimum required Windows operating system version __19044__. It will *not* install on older versions like Windows 7, 8, or early builds of Windows 10.
Build __19044__ is __Windows 10__ version __21H2__, not a separate milestone called "19044."

The lifecycle table makes the choice surprisingly narrow.
That is an interesting illustration that Docker VMM is supposed to reduce the virtualization-stack dependency - after one first insatlls a fairly recent ("old" only i nMicrosoft terms) Windows build.  

Docker is removing a virtualization-stack dependency, but the prerequisite is a sufficiently recent Windows build.

So I would phrase your observation almost exactly as you did, with the reversal:

The lifecycle table makes the choice surprisingly narrow. That is an interesting illustration of how Docker VMM reduces the virtualization-stack dependency — after one first installs a fairly recent "old" Windows build, at least by Microsoft terms.

The quotation marks around "old" do a lot of work there. A Windows 10 21H2/22H2 machine isn't remotely ancient from a hardware or Unix-world perspective; it is simply historical according to Microsoft's servicing model.



Also, in August 2026 there is in fact no ordinary supported Windows 10 Home/Pro release left.
### TLDR;

At the implementation level, your observation is almost exactly right:

You have an Enumerable<Record> (or equivalent stream of records).
You have a routing/predicate/function that determines the destination.
Story #1 uses something like index < N/2 → left/right.
Story #2 uses something like client == A → left, otherwise B → right.

So the routing mechanism can be the same abstraction, with only the routing policy changing.

In pseudocode, both reduce to:

for each record:
    destination = routingPolicy(record)
    send(record, destination)

The interesting part is that the two customer memos may nevertheless describe different business requirements. That's why I think your second point — "perhaps we should ask the customer and listen to the requirements one more time" — was actually the more important observation.

There are several possibilitie

There are several possibilities hidden behind those apparently simple descriptions:

Is "half" literally half the records, half the bytes, or some other balancing criterion?
Is ordering required independently on each route?
Does client A/B routing depend on metadata that isn't present on every record?
Can records be split arbitrarily, or must some records travel together?
Is the routing decision static or configurable?
Does the customer actually want two independently deployable capabilities?
Or did two different memos simply describe two examples of the same underlying routing capability?


### See Also

  * [Windows 10 release information](https://learn.microsoft.com/en-us/windows/release-health/release-information)
  * [Windows 10 Home and Pro Relase Milestones](https://learn.microsoft.com/en-us/lifecycle/products/windows-10-home-and-pro)
 

--- 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


