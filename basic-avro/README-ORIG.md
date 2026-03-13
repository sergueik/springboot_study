# OpenAPI/JSON to Avro Converter + Java Code Generator

Outil Java complet pour Apache Avro :
- 🔄 **OpenAPI/Swagger → Avro** : Conversion de specs API en schémas Avro
- 🔄 **JSON → Avro** : Inférence de schémas depuis données JSON
- ⚡ **Avro → Java** : Génération automatique de classes Java (Maven plugin)
- 📦 **Avro → JSON → Binaire** : Génération de JSON exemple et encodage en trame binaire Avro
- 📄 **Minification** : Génération automatique d'une version one-line (`.min.avsc`) pour chaque schéma

## 🚀 Quick Start

### Prérequis
- Java 21+
- Maven 3.6+

### Installation

```bash
# Build
mvn clean package

# Génère le Fat JAR: target/json-to-avro-converter.jar
```

### Utilisation

```bash
# JSON → Avro Schema
java -jar target/json-to-avro-converter.jar data.json schema.avsc

# OpenAPI → Avro Schema (mode registry pour IBM/Confluent Schema Registry)
java -jar target/json-to-avro-converter.jar api.yaml output.avsc User --registry

# Avro → Java (automatique)
mvn compile  # Les classes sont générées dans target/generated-sources/avro/

# Avro Schema → JSON exemple
java -jar target/json-to-avro-converter.jar generate src/main/avro/User.avsc User.json User

# JSON → Trame binaire Avro
java -jar target/json-to-avro-converter.jar encode src/main/avro/User.avsc User.json User.avro User

# Génération JSON + encodage binaire en une commande
java -jar target/json-to-avro-converter.jar encode src/main/avro/User.avsc --generate User.avro User
```

## 📖 Documentation Détaillée

### OpenAPI/Swagger → Avro

**Fonctionnalités clés:**
- Support OpenAPI 3.0.x, 3.1.x, Swagger 2.0 (YAML/JSON)
- Conversion directe des types et enums
- Extraction automatique des patterns regex
- Résolution des `$ref`
- **Mode registry** (`--registry`) : Schéma unique auto-contenu compatible IBM/Confluent Schema Registry
- Génération automatique d'un fichier `.min.avsc` (JSON one-line) à côté du `.avsc`

**Exemples:**

```bash
# Mode standard (fichiers séparés, types inline)
java -jar target/json-to-avro-converter.jar api.yaml output-dir/

# Mode registry (schéma unique auto-contenu pour Schema Registry)
java -jar target/json-to-avro-converter.jar api.yaml User.avsc User --registry
```

**Mapping des types:**
- string → STRING (+ logical types: uuid, timestamp)
- integer → INT/LONG
- number → FLOAT/DOUBLE
- boolean → BOOLEAN
- object → RECORD
- array → ARRAY
- enum → ENUM

### JSON → Avro

**Détection automatique:**
- Types primitifs (string, boolean, number)
- **UUID** : Détection regex → `logicalType: uuid`
- **ENUM** : Patterns UPPER_CASE → enum Avro
- Arrays, Records imbriqués
- Champs null → union `["null", "type"]`

**Exemple:**

```bash
java -jar target/json-to-avro-converter.jar data.json schema.avsc
# → Génère schema.avsc (formaté) + schema.min.avsc (one-line)
```

### Avro Schema → JSON + Binaire

**Deux sous-commandes** pour générer du JSON et des trames binaires Avro à partir de schémas :

**`generate`** — Génère un JSON exemple (format Avro JSON encoding) à partir d'un schéma `.avsc` :
```bash
# Génère output.json avec des valeurs par défaut cohérentes
java -jar target/json-to-avro-converter.jar generate src/main/avro/User.avsc output.json User
```

**`encode`** — Encode du JSON en fichier binaire `.avro` (container format avec header + schema embarqué) :
```bash
# Depuis un fichier JSON existant
java -jar target/json-to-avro-converter.jar encode src/main/avro/User.avsc User.json User.avro User

# Auto-génération + encodage en une commande
java -jar target/json-to-avro-converter.jar encode src/main/avro/User.avsc --generate User.avro User
```

**Types supportés pour la génération JSON :**
- `string` → `"example_string"`, UUID → UUID aléatoire
- `int`/`long`/`float`/`double` → `0`
- `boolean` → `false`
- `enum` → premier symbole
- `array` → `[]`
- `record` → objet récursif
- `timestamp-millis` → timestamp courant
- unions `["null", T]` → valeur non-null wrappée (format Avro JSON encoding)

### Avro → Java (Maven Plugin)

**Structure:**
```
src/main/avro/         ← Vos schémas .avsc (versionnés)
  ├── User.avsc
  └── Order.avsc

target/generated-sources/avro/  ← Classes générées (automatique)
  └── com/shanks/model/
      ├── User.java
      └── Order.java
```

**Workflow:**

1. Créer un schéma dans `src/main/avro/User.avsc`:
```json
{
  "type": "record",
  "name": "User",
  "namespace": "com.shanks.model",
  "fields": [
    {"name": "userId", "type": {"type": "string", "logicalType": "uuid"}},
    {"name": "username", "type": "string"},
    {"name": "email", "type": "string"},
    {"name": "age", "type": ["null", "int"], "default": null}
  ]
}
```

2. Compiler (génération automatique):
```bash
mvn compile
```

3. Utiliser dans votre code:
```java
import com.shanks.model.User;

User user = User.newBuilder()
    .setUserId(UUID.randomUUID())
    .setUsername("john.doe")
    .setEmail("john@example.com")
    .build();
```

**Configuration Maven:**
Le plugin est déjà configuré dans `pom.xml` avec:
- String type (pas CharSequence)
- Champs privés + getters/setters
- Builder pattern automatique
- Support logical types (UUID, timestamp, decimal)

**Plus d'infos:** Voir [src/main/avro/README.md](src/main/avro/README.md)

## 🔀 Deux Approches Disponibles

| Approche | Branch | Cas d'usage |
|----------|--------|-------------|
| **Maven Plugin** ⭐ | `main` | Schémas stables, build automatique, IDE integration |
| **CLI Runtime** | `feat/toJsonOrAvro` | Génération dynamique, workflow OpenAPI→Avro→Java unifié |

**Branch actuelle (`main`):** Utilise `avro-maven-plugin` pour génération automatique au build.

**Branch alternative (`feat/toJsonOrAvro`):** CLI avec `--generate-java` pour génération à la demande.

## 🏗️ Architecture

```
src/main/java/com/shanks/
├── cli/              # CLI et parsing arguments
├── converter/        # Convertisseurs (JSON, OpenAPI)
├── serializer/       # Génération JSON + encodage binaire Avro
├── parser/           # Parser OpenAPI
├── mapper/           # Mapping types
├── model/            # Modèles de données
└── util/             # Détecteurs (UUID, ENUM)

src/main/avro/        # Schémas Avro versionnés
target/generated-sources/avro/  # Classes Java générées
```

**Principes:** SOLID, injection de dépendances, séparation des responsabilités.

## 🧪 Tests

```bash
mvn test  # 59 tests unitaires
```

## 🔧 Dépendances

- Apache Avro 1.11.3
- Jackson 2.16.1 (JSON)
- Swagger Parser 2.1.22 (OpenAPI)
- avro-maven-plugin 1.11.3

## 📝 Exemples Rapides

**Mode Registry vs Standard (OpenAPI):**

```yaml
# api.yaml
components:
  schemas:
    CardType:
      type: string
      enum: [DEBIT, CREDIT]
    CreditCard:
      type: object
      properties:
        type:
          $ref: '#/components/schemas/CardType'
```

```bash
# Mode registry (1 fichier auto-contenu, types imbriqués inline puis référencés par nom)
java -jar target/json-to-avro-converter.jar test-openapi.yaml CreditCard.avsc CreditCard --registry
# → Enum défini inline à la première occurrence, référencé par nom ensuite

# Mode standard (types inline dans le record)
java -jar target/json-to-avro-converter.jar test-openapi.yaml CreditCard.avsc CreditCard
# → Enum inline dans le record
```

**Détection automatique (JSON):**

```json
{
  "userId": "550e8400-e29b-41d4-a716-446655440000",
  "status": "STATUS_ACTIVE",
  "tags": ["TAG_PREMIUM"]
}
```
→ Détecte automatiquement: UUID (logical type), ENUM (patterns UPPER_CASE)

## 🐛 Troubleshooting

**NoClassDefFoundError:** Utilisez le Fat JAR `json-to-avro-converter.jar`, pas `demo-1.0-SNAPSHOT.jar`

**Classes non générées:** Exécutez `mvn clean generate-sources`

**IDE ne voit pas les classes:** Recharger le projet Maven (IntelliJ: Maven → Reload)

## 📄 License

Usage éducatif et professionnel.

---

**Version:** 1.0-SNAPSHOT | **Java:** 21+ | **Maven:** 3.6+
