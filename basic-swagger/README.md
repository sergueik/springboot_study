
### Info

this directory contains trimmed code from the example [project](https://github.com/bennzhang/spring-boot-swagger2-demo)

### Usage
* run the app

```sh
mvn spring-boot:run
```
See the data and service defintion in REST call
```sh
curl -s http://localhost:8080/v2/api-docs |jq '.'
```

```json
{
  "swagger": "2.0",
  "info": {
    "description": "Spring Boot Swagger Example Api Creator",
    "version": "1.0",
    "title": "Spring Boot Swagger Example API",
    "termsOfService": "Terms of Service",
    "contact": {
      "name": "yourname",
      "url": "https://github.com/yourlink",
      "email": "youremail@yourdomain.com"
    },
    "license": {
      "name": "Apache License Version 2.0",
      "url": "https://www.apache.org/licesen.html"
    }
  },
  "host": "localhost:8080",
  "basePath": "/",
  "tags": [
    {
      "name": "person-resource",
      "description": "Shows the user info"
    }
  ],
  "paths": {
    "/rest/person/{userName}": {
      "get": {
        "tags": [
          "person-resource"
        ],
        "summary": "getPerson",
        "operationId": "getPersonUsingGET",
        "consumes": [
          "application/json"
        ],
        "produces": [
          "*/*"
        ],
        "parameters": [
          {
            "name": "userName",
            "in": "path",
            "description": "userName",
            "required": true,
            "type": "string"
          }
        ],
        "responses": {
          "200": {
            "description": "OK",
            "schema": {
              "$ref": "#/definitions/Person"
            }
          },
          "401": {
            "description": "Unauthorized"
          },
          "403": {
            "description": "Forbidden"
          },
          "404": {
            "description": "Not Found"
          }
        }
      }
    }
  },
  "definitions": {
    "Person": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "name"
        },
        "salary": {
          "type": "integer",
          "format": "int64",
          "description": "salary"
        },
        "title": {
          "type": "string",
          "description": "title"
        }
      }
    }
  }
}

```

 * alternatively explore the defined operations through Swagger UI. The default URL for the swagger-ui will be `http://localhost:8080/swagger-ui.html`

![Swagger ui](https://github.com/sergueik/springboot_study/blob/master/basic-swagger/screenshots/capture-swagger.png)


### High-Level Reasons for Using OpenAPI / Swagger in Enterprise

For a Spring Boot service exposing a limited REST surface, OpenAPI/Swagger UI provides a standards-based interactive web interface, synchronized documentation, and client code generation directly from the source. In many enterprise scenarios, this is a better engineering investment than building and maintaining a separate custom UI whose only purpose is endpoint access.

#### Why this is the preferred default
For services with only a few relevant REST endpoints, the primary needs are usually:

- discoverability of endpoints
- request and response schema visibility
- quick manual testing from a browser
- authentication token validation
- standardized API documentation
- keeping documentation synchronized with code
- enabling downstream client SDK generation
- reducing onboarding time for developers and QA
- preventing implementation/documentation drift

OpenAPI annotations in Spring Boot satisfy these needs directly from the controller and DTO layer, allowing the generated Swagger UI to become the **uniform, low-maintenance API interaction surface**.

#### Enterprise-level engineering benefits
Using OpenAPI/Swagger helps the team:

- reduce duplicated effort between backend and ad hoc frontend utilities
- avoid support burden for home-grown internal testing pages
- enforce DTO validation and response contract clarity
- standardize error response formats
- improve API governance and review quality
- accelerate integration with other internal teams
- improve auditability of public and partner-facing contracts
- reduce defects caused by outdated wiki documentation
- improve long-term maintainability through convention over customization

#### Leadership / architecture rationale
If the objective is endpoint discoverability, supportability, and reducing API misuse, Swagger UI gives a near-zero-maintenance standardized interface generated directly from the source code.

This means:

- lower long-term maintenance cost
- less documentation drift
- fewer integration defects
- faster onboarding
- easier handoff across teams
- improved best-practice enforcement

#### Important architectural distinction
Swagger UI should be considered the default solution for:

- backend developers
- QA engineers
- support engineers
- partner integrations
- admin/internal API utilities
- early-stage prototypes

A custom frontend should be introduced only when there is a true **business workflow requirement**, such as:

- multi-step forms
- reporting dashboards
- role-based business operations
- branded user experience
- customer-facing portals
- complex approval workflows

#### Recommended architectural principle
**For endpoint exploration, validation, and API documentation, custom UI should be the exception rather than the default.**

Use:
- **Swagger / OpenAPI** for API interaction and contract visibility
- **Custom frontend UI** only for real workflow or product experience needs

### Enterprise Reality: Vendor Alignment and UI Approval Constraints

There are additional practical considerations in enterprise environments that reinforce the use of OpenAPI / Swagger as the default interaction layer for REST services.

#### 1. Vendor and ecosystem alignment
A common pattern across enterprise vendors and platform providers is that OpenAPI / JSON-based REST interfaces are the *de facto standard* for API exposure and demonstration.

In practice, most vendor tooling, demos, and integration examples are built around:
- OpenAPI specifications
- Swagger UI or equivalent API explorers
- JSON request/response contracts

This reflects the industry-wide convergence on OpenAPI as the standard contract-first approach for REST services, making it the most interoperable and widely understood interface layer.

As a result, aligning internal services with OpenAPI reduces friction when:
- integrating with external vendors
- reviewing solutions during procurement
- onboarding third-party systems
- comparing APIs across platforms

#### 2. Enterprise security and UI deployment constraints
Introducing a custom web UI into an enterprise environment is not only a design decision but also a security and governance decision.

In many organizations, any new web-facing UI component may trigger:
- security review (SAST/DAST, penetration testing requirements)
- authentication and authorization review (SSO, RBAC integration)
- infrastructure approval (hosting, ingress, reverse proxy rules)
- data exposure risk assessment
- compliance validation (logging, audit, GDPR/PII handling where applicable)

As a result, a custom-built UI layered on top of a REST service is often:
- rejected unless it provides clear business value beyond API interaction
- or significantly delayed due to governance requirements

#### Architectural implication
Given these constraints, it is generally not only a design question but also an approval and operational risk question.

Therefore, it is important to explicitly signal during design discussions that:
- a custom UI implies a full enterprise web application lifecycle
- while OpenAPI/Swagger provides a pre-approved, standardized, and widely accepted interaction model

#### Summary principle
In enterprise contexts, the decision is not simply “can we build a UI”, but rather:

> “Does the UI provide enough business value to justify the additional security, governance, and maintenance overhead compared to a standardized OpenAPI interface?”

For many backend services, Swagger/OpenAPI already satisfies the required interaction and documentation needs without introducing additional approval complexity.
### See Also
  
 * [Generate Spring Boot REST Client project with Swagger](https://www.baeldung.com/spring-boot-rest-client-swagger-codegen)
 * [Spring Boot + Swagger 2](https://www.javainuse.com/spring/boot_swagger)
 * [Setting Up Swagger 2 with a Spring REST API](https://www.baeldung.com/swagger-2-documentation-for-spring-rest-api)
 * [reading Swagger spec from YAML file](https://qna.habr.com/q/1080992)(in Russian)
 * [change default Sagger UI url prefix](https://www.baeldung.com/spring-boot-custom-swagger-url)
 * https://swagger.io/docs/swagger-inspector/how-to-create-an-openapi-definition-using-swagger/
 * https://www.baeldung.com/swagger-2-documentation-for-spring-rest-api
 * https://stackoverflow.com/questions/36745620/swagger-2-spring-boot-generate-yml-file
 * https://howtodoinjava.com/swagger2/swagger-spring-mvc-rest-example/
 * Swagger Pluralsight Training
   + [Getting Started with Swagger 2 Tools](https://app.pluralsight.com/library/courses/getting-started-swagger-tools)
   + [Spring Framework: Documenting Spring Data Rest APIs with Swagger and Springfox](https://app.pluralsight.com/library/courses/spring-framework-documenting-spring-data-rest-apis-swagger-springfox)
 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
