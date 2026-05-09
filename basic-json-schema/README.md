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

Validation is the real power of JSON Schema belongs
In the project resources put a more realistinc schema with certain requirements on the data shape and details:

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
JsonSchema schema = factory.getSchema(testCase.getSchemaResource());
Set<ValidationMessage> errors = schema.validate(input);
if (!testCase.getValid()) {

	assertThat("Expected validation failure", errors.isEmpty(), is(false));
	errorMessage = errors.stream().map(ValidationMessage::getMessage).reduce("", (a, b) -> a + "\n" + b);
	assertThat("Expected fragment not found.\n" + "Actual messages:\n" + errorMessage, errorMessage,
			matchesPattern("(?s).*" + testCase.getExpectedMessage() + ".*"));
}

```
When the JSON payload is valid, the test verifies that no validation errors are reported:
```java
errors = schema.validate(input);
if (testCase.getValid())
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

This turns new test creation into a YAML authoring task rather than a Java coding task.

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

### See Also


  * Java - [jsonschema2pojo](https://github.com/joelittlejohn/jsonschema2pojo) - Generate Java types from JSON or JSON Schema and annotate those types for data-binding with Jackson, Gson, etc.
  * [JSON Editor](https://github.com/jdorn/json-editor) - reads a JSON Schema file(s) and uses it to generate an HTML form for editing the JSON

  * another [repo](https://github.com/json-editor/json-editor) of the same
  * [SchemaStore/schemastore](https://github.com/SchemaStore/schemastore) - collection of independent JSON schemas (under `src/schemas/json`) - [MCP Server](https://github.com/SchemaStore/SchemaStoreMcpServer) (ASPNet Core, .Net 10)
  * https://github.com/confluentinc/schema-registry - Confluent Schema Registry for Kafka
  * https://github.com/ethlo/jsons2xsd - converter from JSON-schema to XML-schema (XSD)
  * https://github.com/victools/jsonschema-generator - creating JSON Schema from Java classes
