### Spring Boot Validation and Async Endpoints: Historical Perspective

This project (`basic-domain-validation`) demonstrates domain-level validation in **Spring Boot 2.7.18**, focusing on **blocking, Callable, and DeferredResult endpoints** with `@Valid` model-level annotations. Interestingly, the behavior of validation in asynchronous endpoints has evolved over the history of Spring Boot, which explains some of the “surprises” we encountered.

---

### 1️⃣ Early Spring Boot and Validation

- **Spring Boot 1.x (e.g., 1.5.4.RELEASE)** relied on `javax.validation` (Hibernate Validator) for bean validation.
- `@Valid` on controller method arguments triggered **synchronous validation** before the controller logic executed.
- Async endpoints were supported via `Callable`, but the validation framework executed **before the Callable started**, so validation errors reliably produced 400 Bad Request.

---

### 2️⃣ Spring Boot 2.x (including 2.3.x → 2.7.x)

- The framework still used **`javax.validation`** and retained **predictable validation semantics**.
- `DeferredResult` and `Callable` endpoints could sometimes appear “tricky” in tests if misconfigured:
  - If `@Valid` was omitted, validation was silently bypassed.
  - If the async pipeline was manipulated incorrectly, validation could appear **corrupted** in older minor versions.
- By **SB 2.7.18**, validation works consistently:
  - **Blocking**, **Callable**, and **DeferredResult** endpoints all correctly validate `@Valid @RequestBody` payloads.
  - In tests, MockMvc sees 400 Bad Request for invalid input **even for DeferredResult** because validation is performed **synchronously before the DeferredResult is returned**.

> This explains why your current tests show 400 for invalid payloads across all three endpoint variants — a reliable, predictable behavior for a “vintage” SB 2.7.x setup.

---

### 3️⃣ The Jakarta / Spring Boot 3.x Transition

- **Spring Boot 3.x** migrated entirely to the **Jakarta namespace**:
  - `javax.validation.*` → `jakarta.validation.*`
  - `javax.servlet.*` → `jakarta.servlet.*`
- The validation API itself remained conceptually similar, but:
  - Some async edge cases changed subtly in SB 3.x due to new **internal handler adapters** and reactive improvements.
  - DeferredResult validation may appear to fail or behave differently if not updated to `jakarta.validation` and tested with **Java 17+**.
- Tests written for SB 2.7.x **will often fail or behave differently** if simply copied to 3.x without updating:
  - The async pipeline may start **before validation triggers**, or
  - Exceptions may propagate differently due to the **new exception handling adapters**.

---

### 4️⃣ Why 2.7.x is a “sweet spot”

- Using **Java 11 + SB 2.7.18** gives:
  - Minimal surprises with validation on async endpoints
  - Predictable test behavior: 400 responses for invalid input, both synchronous and deferred
  - Stable dependencies (`spring-boot-starter-validation`, `spring-boot-starter-web`) without the complexity of Jakarta migration
- This project demonstrates **a clean “vintage” validation workflow** that is easy to extend for teaching, demos, and experimentation with async endpoints.

---

### 5️⃣ Project Roadmap

- **Current branch (Java 11 + SB 2.7.18)**:
  - Working blocking / Callable / DeferredResult endpoints
  - Proper validation for invalid payloads
  - Tests reliably detect 400 Bad Request for all endpoints

- **Future branch (Java 17 + SB 3.x)**:
  - Update to `jakarta.validation.*`
  - Demonstrate migration differences and how DeferredResult / Callable endpoints behave under SB 3.x
  - Compare test results with 2.7.x for educational purp

