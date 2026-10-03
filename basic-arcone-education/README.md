# ArcOne AI — Newbie Introduction

> **Status:** Initial orientation / learning notes
> **Purpose:** Understand what ArcOne AI is before attempting to understand its individual products, agents, connectors, and banking use cases.

## 1. What is ArcOne AI?

[ArcOne AI](https://www.arcone.com/) is an enterprise AI and revenue-intelligence company focused on highly regulated industries, particularly **banking, financial services, energy, and utilities**.

For banking, its current platform is called **ArcOne BankOS**.

The important point is that ArcOne is **not simply a chatbot and not simply an LLM application**.

Its architecture is intended to sit **above existing enterprise systems and banking cores**, connect fragmented data, establish a semantic/domain-aware representation of that data, apply AI/ML/rules/analytics, and expose the resulting intelligence through specialized AI agents and business applications.

ArcOne describes this as a **Vertical AI Orchestration System**.

---

## 2. The simplest mental model

Think of ArcOne as an additional intelligence layer placed over an existing bank:

```text
                 BANK EMPLOYEE
                       |
                       v
                Applications / LYZA
                       |
                       v
                 AGENT FABRIC
                       |
              +--------+--------+
              |                 |
          AI Agents       Workflows
              |                 |
              +--------+--------+
                       |
                       v
               INTELLIGENCE FABRIC
                       |
          +------------+-------------+
          |            |             |
         LLM           ML          Rules
          |            |             |
          +------------+-------------+
                       |
                       v
                   DATA FABRIC
                       |
             Semantic / Domain Layer
                       |
          +------------+-------------+
          |            |             |
       Core A       Core B       Enterprise Data
          |            |             |
          +------------+-------------+
                       |
                       v
              EXISTING BANK SYSTEMS
```

The fundamental architectural proposition is:

> **Do not replace the bank's existing systems merely to make them usable by AI. Put a governed, semantically meaningful intelligence layer above them.**

ArcOne explicitly says BankOS is designed to make banking cores AI-ready without re-platforming existing infrastructure.

---

# 3. Why does ArcOne need such a complicated architecture?

A bank normally does not have one clean database.

It may have:

```text
Core banking system A
Core banking system B
Payments
Cards
CRM
Billing
Pricing systems
Data warehouses
Data lakes
Risk systems
Document repositories
Enterprise applications
Legacy applications
```

Each system can have different:

```text
schemas
field names
identifiers
business terminology
data quality
security models
APIs
batch interfaces
streaming interfaces
ownership
```

For example:

```text
ACCOUNT_ID
ACCT_NO
ACCOUNT_NUMBER
CUSTOMER_ACCOUNT
```

might refer to related concepts in different systems.

An AI agent cannot safely assume that these are equivalent.

Therefore ArcOne places a **Data Fabric / semantic layer** between the enterprise systems and the AI.

---

# 4. Ocular AI

ArcOne calls its underlying data-and-AI foundation **Ocular AI**.

It consists of three major fabrics:

```text
                 OCULAR AI
                     |
       +-------------+-------------+
       |             |             |
       v             v             v
   DATA FABRIC  INTELLIGENCE   AGENT FABRIC
                  FABRIC
```

These are intended to work together rather than being unrelated products.

---

# 5. Data Fabric

The **Data Fabric** is the foundation.

Its purpose is to:

* connect enterprise systems
* integrate data
* transform and normalize data
* apply semantic meaning
* govern data
* make data usable by AI and applications

ArcOne's banking implementation includes a **Banking Domain Cartridge**.

The current BankOS material describes:

* 60+ enterprise-system connectors
* 80%+ automatic mapping of banking-core fields
* banking-specific terminology
* semantic governance

The important idea is that the AI agents should not have to understand every individual bank's historical schema independently.

Instead:

```text
               BANKING DOMAIN MODEL
                       |
        +--------------+--------------+
        |              |              |
       Core A         Core B       Warehouse
        |              |              |
     ACCOUNT_ID      ACCT_NO       ACCOUNT
```

The semantic layer provides a common interpretation.

This is arguably one of the most important architectural concepts to understand before studying the "AI agent" terminology.

---

# 6. Intelligence Fabric

The **Intelligence Fabric** sits above the data layer.

ArcOne describes it as combining:

* generative AI
* language models
* machine learning
* statistical models
* rules
* analytics

into coordinated intelligence.

Conceptually:

```text
                 BUSINESS QUESTION
                        |
                        v
                INTELLIGENCE FABRIC
                        |
       +----------------+----------------+
       |                |                |
      LLM               ML              Rules
       |                |                |
       +----------------+----------------+
                        |
                        v
                  Semantic Data
```

Therefore "AI" in ArcOne should not automatically be interpreted as:

```text
prompt -> LLM -> answer
```

It is closer to:

```text
enterprise data
      |
semantic interpretation
      |
analytics / ML / LLM / rules
      |
decision
      |
workflow / agent
      |
business action
```

---

# 7. TERRA

ArcOne describes **TERRA** as its orchestration engine.

The acronym represents:

```text
T = Trigger
E = Evaluate
R = Research
R = Recommend
A = Act
```

Conceptually:

```text
EVENT
  |
  v
TRIGGER
  |
  v
EVALUATE
  |
  v
RESEARCH
  |
  v
RECOMMEND
  |
  v
ACT
```

This is important because an "agent" is not necessarily just an intelligent conversational interface.

An agent may participate in a business process.

---

# 8. Agent Fabric

The **Agent Fabric** is the layer where the intelligence becomes usable as agents and workflows.

ArcOne describes a growing collection of specialized banking agents.

Examples include:

### Enrich360

Pricing, product, and profitability intelligence.

Typical conceptual workflow:

```text
Customer
   |
Products
   |
Pricing
   |
Revenue
   |
Cost
   |
Profitability
   |
Recommended action
```

### Experience360

Customer-experience and engagement intelligence.

Conceptually:

```text
Customer interaction
       |
Customer history
       |
Enterprise data
       |
AI interpretation
       |
Recommendation / response
```

### Exceptions360

Exception detection, prioritization, investigation, and resolution.

Conceptually:

```text
Expected process
       |
       v
Actual result
       |
       v
   Mismatch
       |
       v
   Exception
       |
       v
Investigate
       |
       v
Resolve / Escalate
```

---

# 9. LYZA

**LYZA** is ArcOne's multimodal interface into its intelligence/agent ecosystem.

ArcOne describes LYZA as supporting inputs such as:

* text
* voice
* web
* video
* documents

The important architectural distinction is:

```text
                 USER
                   |
                 LYZA
                   |
             Agent Fabric
                   |
          Intelligence Fabric
                   |
              Data Fabric
                   |
             Bank Systems
```

Therefore LYZA should not be thought of as "ArcOne's ChatGPT."

It is better understood as a **human-facing interface/conductor for the underlying agent and intelligence system**.

---

# 10. ArcOne EPM

**EPM = Enterprise Profit Maximization.**

This is the more traditional business-application side of ArcOne.

It addresses revenue-management activities such as:

* account analysis
* product management
* pricing
* repricing
* deal management
* profitability analysis
* what-if analysis

So the relationship can be pictured as:

```text
                ArcOne EPM
                    |
             Business workflows
                    |
             Ocular AI
                    |
        +-----------+-----------+
        |           |           |
       Data     Intelligence   Agents
        |
 Existing bank systems
```

---

# 11. ArcOne BankOS

The current banking packaging is **ArcOne BankOS**.

ArcOne describes BankOS as extending revenue intelligence into:

* Retail Banking
* Commercial Banking
* Global Transaction Banking
* Capital Markets
* Wealth
* Payments

The July 2026 announcement describes 60+ connectors and a growing library of 100+ AI agents/agentic workflows.

The advertised deployment model is:

```text
CONNECT
   |
   v
MAP
   |
   v
ACTIVATE
```

ArcOne currently describes a target deployment period of approximately four to six months.

These are **vendor claims**, not independent measurements, and should be treated as such in an architecture evaluation.

---

# 12. What ArcOne is NOT

It is useful to explicitly avoid several misleading mental models.

### It is not simply an LLM

It combines LLMs with ML, statistics, rules, analytics, enterprise data, and orchestration.

### It is not simply a chatbot

LYZA is a user interface into a much larger system.

### It is not simply RPA

The intended abstraction is above screen automation: enterprise data, semantics, decision intelligence, agents, workflows, and system actions.

### It is not a new banking core

ArcOne's stated architecture is designed to operate on top of existing banking cores.

### It is not merely a data warehouse

The data layer exists to supply intelligence, agents, applications, and business decisions.

---

# 13. A useful comparison with conventional enterprise architecture

A conventional application might look like:

```text
UI
 |
Application
 |
Service layer
 |
Database
```

An ArcOne-style architecture is closer to:

```text
Human / Business Application
             |
          Agents
             |
       Orchestration
             |
    AI / ML / Rules / Analytics
             |
       Semantic Data
             |
     Enterprise Integration
             |
      Existing Systems
```

The additional complexity exists because the system is attempting to turn heterogeneous enterprise data into **governed machine-actionable business intelligence**.

---

# 14. Why the domain is unusually complicated

The difficult part is not learning the names:

```text
Ocular
IntelliArc
TERRA
LYZA
EPM
Enrich360
Experience360
Exceptions360
BankOS
```

The difficult part is understanding the relationships between:

```text
banking systems
       +
enterprise integration
       +
data modeling
       +
semantic modeling
       +
AI/ML
       +
LLMs
       +
agents
       +
workflow orchestration
       +
governance
       +
auditability
       +
banking business processes
```

Therefore ArcOne should be studied **from the bottom upward**, not from the marketing names downward.

Recommended learning order:

```text
1. Banking systems
       |
2. Enterprise integration
       |
3. Data / semantic layer
       |
4. AI / ML / LLM fundamentals
       |
5. Agent architecture
       |
6. Orchestration
       |
7. Governance / audit
       |
8. ArcOne Ocular AI
       |
9. ArcOne IntelliArc
       |
10. ArcOne BankOS
       |
11. Individual products and agents
```

---

# 15. One-sentence architect definition

> **ArcOne is a vertical enterprise AI orchestration platform that creates a governed, banking-aware semantic layer over heterogeneous enterprise systems, combines AI/ML/LLMs/rules/analytics above that layer, and exposes the resulting decision intelligence through coordinated agents, workflows, and revenue-management applications.**

That sentence is deliberately much less marketing-oriented than "ArcOne is an AI platform."

It is a better starting point for architectural study.

---

# 16. Important documentation caveat

Public ArcOne material describes the architecture and capabilities but does **not** expose every implementation detail.

For example, public documentation does not by itself establish:

* which specific LLM providers are used
* the exact model architecture
* the internal database technology
* the exact message-bus implementation
* Kubernetes/container topology
* every API protocol
* internal service boundaries
* exact connector implementation
* deployment topology for a particular bank

Those details should not be invented from the product diagrams.

For architecture study, distinguish:

```text
DOCUMENTED
    |
    +-- ArcOne's published architecture
    +-- published capabilities
    +-- published integration claims
    |
    v
INFERRED
    |
    +-- likely implementation patterns
    |
    v
UNKNOWN
    |
    +-- proprietary implementation details
```

That distinction is particularly important when preparing for an enterprise architecture discussion.

## Primary references

* ArcOne AI: https://www.arcone.com/
* ArcOne BankOS: https://www.arcone.com/bankos
* ArcOne Banking: https://www.arcone.com/banking
* ArcOne July 2026 BankOS announcement: https://www.arcone.com/news/arcone-ai-launches-arcone-bankos

