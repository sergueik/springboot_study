### Overview
A JSON Schema document is just JSON data with a specific vocabulary. this is 
actually one of the strongest conceptual advantages of JSON Schema over things like Jsonnet.



### Usage
```cmd
mvn  -DskipTests spring-boot:run
```
```cmd
curl -s http://localhost:8080/assembly/transaction
```
```json
{
  "transactionId": "T-9001",
  "timestamp": "2026-05-07T12:00:00Z",
  "customer": {
    "accountNumber": "000111222",
    "currency": "USD",
    "balance": 2500.75
  },
  "account": {
    "accountNumber": "000111222",
    "currency": "USD",
    "balance": 2500.75
  },
  "amount": 125.5
}
```
```cmd
curl -s http://localhost:8080/components/transaction.json
```
reveals JSON Schema-signature DSL:
```json
{
  "transactionId": "T-9001",

  "customer": {
    "$ref": "customer.json"
  },

  "account": {
    "anyOf": [
      { "$ref": "account-basic.json" },
      { "$ref": "account-premium.json" }
    ]
  },

  "amount": 125.50
}
```
while
```cmd
curl -s http://localhost:8080/components/customer.json
```
shows "regular" JSON
```json
{
  "accountNumber": "000111222",
  "currency": "USD",
  "balance": 2500.75
}
```
In real enterprise systems, you typically see a thin but very influential "schema layer" sitting above plain JSON payloads that dominate volume

### Testing

Validation is where the real power of JSON Schema becomes visible.

The project resources contain a more realistic schema with explicit requirements on the data structure and field values:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",

  "type": "object",
  "required": ["transactionId", "customer", "account", "amount"],

  "properties": {

    "transactionId": {
      "type": "string",
      "minLength": 1
    },

    "customer": {
      "type": "object",
      "required": ["customerId", "name"],
      "properties": {
        "customerId": { "type": "string" },
        "name": { "type": "string" }
      },
      "additionalProperties": false
    },

    "account": {
      "type": "object",
      "required": ["accountType", "balance"],
      "properties": {
        "accountType": {
          "type": "string",
          "enum": ["basic", "premium"]
        },
        "balance": {
          "type": "number"
        }
      },
      "additionalProperties": false
    },

    "amount": {
      "type": "number",
      "minimum": 0
    }
  },

  "additionalProperties": false
}
```

This schema requires:

* the `amount` `value` to be positive,
* the `transactionId` to be non-empty,
* the `customer` and `account` objects to always exist,
* and forbids unexpected fields from appearing in nested objects.

To verify these constraints are enforced correctly, tests are executed with JSON payloads intentionally violating one rule at a time.
The test then confirms the validation failure is reported with a clear error message

```java
JsonSchemaFactory factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V7);
JsonSchema schema = factory.getSchema(testCase.schemaResource());
Set<ValidationMessage> errors = schema.validate(input);
if (!testCase.valid()) {

	assertThat("Expected validation failure", errors.isEmpty(), is(false));
	errorMessage = errors.stream().map(ValidationMessage::getMessage).reduce("", (a, b) -> a + "\n" + b);
	assertThat("Expected fragment not found.\n" + "Actual messages:\n" + errorMessage, errorMessage,
			matchesPattern("(?s).*" + testCase.expectedMessage() + ".*"));
}

```
When the JSON payload is valid, the test verifies that no validation errors are reported:
```java
errors = schema.validate(input);
if (testCase.valid())
	assertThat("Unexpected validation errors: " + errors, errors.isEmpty(), is(true));

```
All test inputs are externalized into `application.yaml`:
```yaml
schemaTests:
  validCases:
    - name: valid-basic-account
      schemaResource: schema/transaction.json
      payloadResource: schema/valid/account.json
      valid: true
  invalidCases:

    - name: extra-field-not-allowed
      schemaResource: schema/transaction.json
      payloadResource: schema/invalid/extra-field.json
      expectedMessage: "is not defined in the schema and the schema does not allow additional properties"
      valid: false

    - name: missing-account-balance
      schemaResource: schema/transaction.json
      payloadResource: schema/invalid/missing-account-balance.json
      expectedMessage: "required property '.*' not found"
      valid: false
```

This turns new test creation into a YAML authoring task rather than a Java coding task (adding new tests becomes mostly data authoring - not Java coding).

All validation functionality is provided by the `json-schema-validator` [library](https://github.com/java-json-tools/json-schema-validator), also [available](https://mvnrepository.com/artifact/io.rest-assured/json-schema-validator) from Maven Central

### Background Information
JSON Schema is used for:

  * contract definition (API boundary)
  * validation rules
  * documentation (OpenAPI)
  * versioning rules
  * compatibility checks
  * code generation


it is usually:

  * centrally governed
  * reused across many services
  * not duplicated per message
  * not embedded in every payload

the ratio can be:
  * 50–500 schemas in a large system
  * millions/billions of JSON documents flowing through them

The JSON Schema is *meta-data* about structure, not data

`customer-type.json`:
```json
{
  "type": "object",
  "required": ["customerId"],
  "properties": {
    "customerId": { "type": "string" },
    "balance": { "type": "number" }
  }
}
```

This is:

  * low volume
  * stable over time
  * version-controlled like code
  * often shared across team


in other words, schena serves "type system + contract layer for a distributed system", not data iself. 

There *are* systems where schema becomes heavier

### Capability Matrix a.k.a. Schema vs Jackson Capability Boundary

In reality JSON Schema only looks like JSON

|Concern |	Jackson only|	External validator needed|
|-------|---------------|----------------------------|
|Build JSON trees	|Yes	|No |
|Serialize schema JSON|	Yes	|No|
|Manipulate `$ref`|	Yes|	No|
|Walk schema nodes|	Yes|	No|
|Parse arbitrary JSON|	Yes|	No|
|Enforce JSON Schema rules|	No|	Yes|
|Draft-aware validation	|No|	Yes|
|`oneOf` / `allOf` semantics|	No|	Yes|
|`if`/`then`/`else` logic|	No|	Yes|
|format validators|	No|	Yes|

#### 1. JSON Schema constructs Jackson can handle directly

| JSON Schema Construct / Keyword | Pure Jackson (`JsonNode` / databind) Can Parse & Manipulate? | Notes |
|---|---|---|
| `$schema` | Yes | Just a string field |
| `$id` | Yes | URI/string handling only |
| `$ref` | Yes (syntactically) | Jackson sees string only |
| `$defs` / `definitions` | Yes | Ordinary object nodes |
| `type` | Yes | String/array only |
| `enum` | Yes | Array handling only |
| `const` | Yes | Value comparison possible manually |
| `properties` | Yes | Nested object trees |
| `required` | Yes | Array of strings |
| `items` | Yes | Tree structure only |
| `prefixItems` | Yes | Array traversal |
| `additionalProperties` | Yes | Boolean/object parsing |
| `patternProperties` | Yes | Regex text only |
| `minimum` / `maximum` | Yes | Numeric values only |
| `exclusiveMinimum` | Yes | Numeric/boolean parsing |
| `minLength` / `maxLength` | Yes | Integer fields |
| `minItems` / `maxItems` | Yes | Integer fields |
| `description` | Yes | Metadata only |
| `title` | Yes | Metadata only |
| `default` | Yes | Plain JSON value |
| `examples` | Yes | Array/object handling |
| `readOnly` / `writeOnly` | Yes | Boolean flags |
| `deprecated` | Yes | Boolean flag |
| `format` | Yes (as text only) | No semantic validation |
| `contentMediaType` | Yes | String only |
| `contentEncoding` | Yes | String only |
| vendor extensions (`x-*`) | Yes | Just JSON fields |
| composition containers (`allOf`, `oneOf`, etc.) | Yes (storage only) | No evaluation semantics |

---

#### 2. JSON Schema features requiring semantic engine / external tooling

| JSON Schema Feature / Semantic Behavior | Requires Additional Logic / Validator? | Why |
|---|---|---|
| Actual `$ref` resolution | Yes | URI resolution + graph traversal |
| Recursive `$ref` handling | Yes | Cycle detection / caching |
| Cross-file schema linking | Yes | Resource loader semantics |
| `allOf` evaluation | Yes | Constraint intersection logic |
| `anyOf` evaluation | Yes | Union validation logic |
| `oneOf` exclusivity semantics | Yes | Exactly-one match logic |
| `not` | Yes | Inverted validation semantics |
| `if` / `then` / `else` | Yes | Conditional evaluation engine |
| `dependentSchemas` | Yes | Dynamic rule activation |
| `dependentRequired` | Yes | Context-sensitive validation |
| `contains` | Yes | Array scanning semantics |
| `unevaluatedProperties` | Yes | Stateful evaluation tracking |
| `unevaluatedItems` | Yes | Requires annotation propagation |
| `propertyNames` | Yes | Dynamic property validation |
| `patternProperties` regex matching | Yes | Regex evaluation |
| `format` validation | Yes | Email/URI/date/etc semantics |
| Draft-specific behavior differences | Yes | Spec-version engine required |
| Annotation collection semantics | Yes | JSON Schema meta-processing |
| Dynamic anchors (`$dynamicRef`) | Yes | Runtime resolution model |
| Vocabulary support | Yes | Draft 2019-09 / 2020-12 feature model |
| Boolean schema semantics | Yes | `true` / `false` schema interpretation |
| Full numeric boundary correctness | Yes | IEEE/integer semantics |
| Unicode-aware pattern validation | Yes | Regex engine behavior |
| Output formats (`basic`, `verbose`) | Yes | Validator reporting model |
| Error path reporting | Yes | Evaluation trace machinery |
| Meta-schema validation | Yes | Validator validating validator |
| OpenAPI compatibility edge cases | Yes | OpenAPI subset divergences |
| Schema normalization / canonicalization | Yes | Graph transformation layer |
| Performance optimizations | Yes | Resolver caches / DAG handling |

---

Jackson Alone Is Sufficient For:

- Parsing JSON Schema documents
- Building schema trees
- Traversing schema nodes
- Transforming schemas
- Generating schemas
- Storing schema fragments
- Custom `$ref` graph assembly
- Lightweight DSL experimentation

Jackson Alone Is _NOT_ Sufficient For:

- Standards-compliant JSON Schema validation
- Full keyword semantics
- Recursive schema evaluation
- Draft-aware behavior
- OpenAPI-compatible validation semantics
- Validator-grade error reporting
- Complete `$ref` resolution semantics

---

- an excellent JSON AST infrastructure layer
- a good foundation for schema tooling
- lightweight and dependency-friendly


But Jackson is **not**:

- a JSON Schema validator
- a schema execution engine
- a standards-compliant semantic evaluator



NOTE: many financial payloads are fundamentally:


* contractual structures
* code/value lists
* repeated segments
* hierarchical records
* deterministic layouts


---

### JSPN Authoring Tool Chain Conceptual Difference

__JSON Schema__:

- declarative
- structural
- constraint-oriented
- Constructing blocks still valid JSON

__Jsonnet__:

- executable
- functional/programmatic
- generates JSON
- not itself JSON

modern JSON-adjacent ecosystems quietly become:

mini programming languages
dependency graphs
type systems
inheritance systems
runtime evaluators

while pretending to be “just JSON.”

And then suddenly:

editor plugins become mandatory
IntelliSense becomes mandatory
schema registries become mandatory
cloud infrastructure becomes mandatory


one does not need full schema validation to demonstrate:

compositional payload assembly
copybook-style segmentation
hierarchical output
reusable fragments
endpoint emission

Here is a small reusable payload assembly technique

JSON Schema is valid
Validation could be added later if useful

OpenAPI 3.x
Swagger tooling
contract documents
schema sections inside specs


### Milestone Release History

|Common Name  |Publication Date|Key Changes & Features|
|-------------|----------------|----------------------|
|Draft 2020-12|June 16, 2022   |Replaces `$recursiveRef` with `$dynamicRef`; introduces `prefixItems`|
|Draft 2019-09|Sept 17, 2019   |Introduced vocabularies, `unevaluatedProperties`, and the `$anchor` keyword|
|Draft 7      |March 19, 2018  |Widely supported version; added `if`/`then`/`else` conditional keywords|
|Draft 6      |April 21, 2017  |Introduced `const`, `contains`, and basic `$id` changes|
|Draft 4      |Jan 2013        |One of the most long-lived legacy versions still found in many tools|

### Python
Python has a very mature JSON Schema ecosystem. The important distinction is:

JSON parsing/manipulation → built into Python (json)
JSON Schema validation → external libraries
LLM structured output enforcement → increasingly built around JSON Schema subsets


|Package|	Primary Role	|JSON Schema Support	|Typical Enterprise Use	|Relative Popularity|
|-------|-------------------|-----------------------|------------------------|------------------|
|`Pydantic`     | Typed models + validation	        |Generates JSON Schema	| FastAPI, GenAI structured outputs, API contracts |	Extremely high|
|`jsonschema`   | Canonical JSON Schema validator	|Full validator	        | Contract validation, OpenAPI, standards compliance |Very high|
|`fastjsonschema`| High-performance validator	    |Draft 04/06/07	        | Large-scale validation, pipelines, gateways      |	High|
|`Marshmallow`  | Serialization/deserialization	    |Partial / addon-based  | Legacy APIs, Flask ecosystems, ETL               |	High but declining|

### JSON Schema and GenAI Structured Outputs

This is now one of the MOST important uses of JSON Schema

Modern LLM systems use JSON Schema to constrain generation.
Instead of:
```text
respond with JSON
```
the caller provides:
```json
{
  "type": "object",
  "properties": {
    "sentiment": {
      "type": "string",
      "enum": [
        "positive",
        "negative"
      ]
    },
    "score": {
      "type": "number"
    }
  },
  "required": [
    "sentiment",
    "score"
  ]
}
```
The model is then forced toward valid output.

JSON Schema Became Central to AI

Without schema:

* prompts become fragile prose
* parsing becomes regex hell
* failures become nondeterministic

OpenAI / Anthropic / Google Direction
All major vendors are moving toward:


schema-constrained outputs

* tool calling
* function calling
* typed responses


Internally this almost always maps to:


JSON Schema

or a close subset of it. NOTE, genAI systems use only a subset of full JSON Schema


In enterprise systems:

* 90–99% of bytes are business data
* schema layer is relatively thin compared to business payload JSON
* but architecturally critical

The schema acts like:

* compiler metadata
* COPYBOOK
* IDL
* contract
* protocol grammar

### Troubleshooting

very complex
error in version 11:
```text
org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'resourceHandlerMapping' defined in class path resource [org/springframework/web/servlet/config/annotation/DelegatingWebMvcConfiguration.class]: Bean instantiation via factory method failed; nested exception is org.springframework.beans.BeanInstantiationException: Failed to instantiate [org.springframework.web.servlet.HandlerMapping]: Factory method 'resourceHandlerMapping' threw exception; nested exception is java.lang.IllegalStateException: No ServletContext set
	at org.springframework.beans.factory.support.ConstructorResolver.instantiate(ConstructorResolver.java:658) ~[spring-beans-5.3.25.jar:5.3.25]
	at org.springframework.beans.factory.support.ConstructorResolver.instantiateUsingFactoryMethod(ConstructorResolver.java:638) ~[spring-beans-5.3.25.jar:5.3.25]
```
### See Also


  * Java - [jsonschema2pojo](https://github.com/joelittlejohn/jsonschema2pojo) - Generate Java types from JSON or JSON Schema and annotate those types for data-binding with Jackson, Gson, etc.
  * [JSON Editor](https://github.com/jdorn/json-editor) - reads a JSON Schema file(s) and uses it to generate an HTML form for editing the JSON

  * another [repo](https://github.com/json-editor/json-editor) of the same
  * [SchemaStore/schemastore](https://github.com/SchemaStore/schemastore) - collection of independent JSON schemas (under `src/schemas/json`) - [MCP Server](https://github.com/SchemaStore/SchemaStoreMcpServer) (ASPNet Core, .Net 10)
  * https://github.com/confluentinc/schema-registry - Confluent Schema Registry for Kafka
  * https://github.com/ethlo/jsons2xsd - converter from JSON-schema to XML-schema (XSD)
  * https://github.com/victools/jsonschema-generator - creating JSON Schema from Java classes
  * [awesome llm skills](https://github.com/feodal01/awesome-llm-skills) - curated list of awesome LLM and AI Agent Skills, resources and tools for customising AI Agent workflows. LLM Skills are customizable workflows that teach LLM how to perform specific tasks according to your unique requirements
  * [feodal01/schema-guided-reasoning-pydantic](https://github.com/feodal01/schema-guided-reasoning-pydantic) - LLM skill for building strict Pydantic schemas for structured output using SGR
  * https://abdullin.com/schema-guided-reasoning 
  * [Structured Decoding in vLLM: a gentle introduction](https://vllm-project.github.io/2025/01/14/struct-decode-intro.html)

 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
