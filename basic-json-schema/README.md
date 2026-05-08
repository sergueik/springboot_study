### Overview
A JSON Schema document is just JSON data with a specific vocabulary. this is 
actually one of the strongest conceptual advantages of JSON Schema over things like Jsonnet.

### Capability Matrix a.k.a. Schema vs Jackson Capability Boundary

In reality JSON Schema only looks like JSON

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
curl -s http://localhost:8080/components/customer.json
```

```json
{
  "accountNumber": "000111222",
  "currency": "USD",
  "balance": 2500.75
}
```
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
