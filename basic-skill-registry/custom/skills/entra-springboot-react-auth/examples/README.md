# Examples

This directory contains minimal, focused examples referenced by
`SKILL.md`.

Examples are intentionally not complete applications. They demonstrate
common implementation patterns and should be adapted to project-specific
requirements.

Included examples:

| File | Purpose |
|------|---------|
| application.yml | Spring OAuth2 Resource Server configuration |
| SecurityConfiguration.java | Spring Security 6 filter chain |
| MethodSecurityExample.java | Annotation-based authorization |
| JwtAuthorityMapping.java | Mapping Entra claims to authorities |
| authConfig.ts | MSAL React configuration |
| apiClient.ts | Sending bearer tokens to backend APIs |

The examples assume:

- Spring Boot 3.x
- Java 17+
- Spring Security 6
- React SPA
- Microsoft Entra ID
- OAuth2 Authorization Code Flow with PKCE
