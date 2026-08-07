---
name: entra-springboot-react-auth
description: >
  Implements Microsoft Entra ID authentication for a Java 17+ Spring Boot
  backend and a React frontend using OAuth 2.0 Authorization Code Flow with
  PKCE, OpenID Connect, Spring Security 6, and MSAL React. Guides the agent
  through application registration, backend protection, frontend login,
  token validation, and common troubleshooting.

license: CC-BY-4.0

triggers:
  - entra
  - azure ad
  - microsoft entra
  - oauth2
  - oidc
  - pkce
  - msal
  - spring security
  - spring boot security
  - jwt
  - authorization code flow
  - react authentication

tags:
  - java
  - spring
  - react
  - security
  - oauth2
  - oidc
  - msal
  - jwt
  - entra
---

# Microsoft Entra Authentication for Spring Boot + React

## Purpose

Use this skill whenever a project combines

- Java 17 or newer
- Spring Boot 3.x
- Spring Security 6
- React frontend
- Microsoft Entra ID authentication

Assume the preferred authentication model is

```
React SPA
        │
Authorization Code + PKCE
        │
Microsoft Entra ID
        │
Access Token (JWT)
        │
Spring Boot Resource Server
```

Avoid legacy implicit flow.

---

# Principles

The backend must never authenticate the user directly.

Authentication belongs to Microsoft Entra.

The frontend obtains tokens using MSAL.

The backend validates JWT access tokens.

Business logic never parses or validates JWT manually.

Always use Spring Security support.

---

# Architecture

Separate concerns.

Frontend

- MSAL React
- login
- logout
- acquireTokenSilent()
- attach Bearer token

Backend

- Resource Server
- JWT validation
- authorization
- role mapping

Microsoft Entra

- identity provider
- OpenID Connect
- OAuth2 authorization server

---

# Dependencies

Backend

```
spring-boot-starter-security
spring-boot-starter-oauth2-resource-server
spring-security-oauth2-jose
```

If the application itself performs login (server-side MVC), also include

```
spring-boot-starter-oauth2-client
```

React

```
@azure/msal-browser
@azure/msal-react
```

---

# Application Registration

Verify

- Tenant ID
- Client ID
- Redirect URI
- Logout URI
- SPA enabled
- Authorization Code Flow with PKCE

Avoid client secrets for SPAs.

---

# Backend Configuration

Prefer configuration over code.

Typical properties include

```
spring.security.oauth2.resourceserver.jwt.issuer-uri=...
```

or

```
spring.security.oauth2.resourceserver.jwt.jwk-set-uri=...
```

Do not hardcode endpoints.

---

# Security Configuration

Prefer SecurityFilterChain.

Example

```java
@Bean
SecurityFilterChain security(HttpSecurity http) throws Exception {

    http
        .authorizeHttpRequests(auth -> auth
            .requestMatchers(
                "/",
                "/index.html",
                "/public/**")
            .permitAll()
            .anyRequest()
            .authenticated()
        )
        .oauth2ResourceServer(oauth ->
            oauth.jwt(Customizer.withDefaults())
        );

    return http.build();
}
```

Avoid deprecated WebSecurityConfigurerAdapter.

---

# Method Security

Prefer annotation-based authorization.

Enable

```java
@EnableMethodSecurity
```

Examples

```java
@PreAuthorize("hasAuthority('APPROLE_Admin')")
```

```java
@PreAuthorize("hasRole('Admin')")
```

```java
@PreAuthorize("hasAuthority('SCOPE_api.read')")
```

Keep authorization close to business logic.

---

# React Integration

Use

```
MsalProvider
```

Protect routes.

Acquire tokens silently whenever possible.

Send

```
Authorization: Bearer <token>
```

Never store tokens in localStorage unless explicitly required.

---

# CORS

Configure CORS explicitly.

Allow

- frontend origin
- Authorization header
- OPTIONS
- credentials only when required

Never disable CORS globally.

---

# Claims

Common claims include

```
sub
oid
tid
preferred_username
name
scp
roles
aud
iss
```

Use Spring Security converters instead of manual parsing.

---

# Roles

Prefer application roles over hardcoded usernames.

Map

```
roles
```

or

```
scp
```

into GrantedAuthority.

---

# Troubleshooting

401

Usually indicates

- invalid audience
- expired token
- wrong issuer
- missing Authorization header

403

Usually indicates

- authenticated user
- insufficient authorities

AADSTS errors

Usually indicate

- redirect URI mismatch
- incorrect client ID
- consent required
- wrong tenant

---

# Security Guidance

Never

- disable JWT validation
- disable signature verification
- accept unsigned tokens
- embed secrets into React

Always

- validate issuer
- validate audience
- validate expiration
- use HTTPS
- use PKCE

---

# Agent Checklist

Before generating code verify

- Java version
- Spring Boot version
- Spring Security version
- React version
- MSAL version
- Tenant ID available
- Client ID available
- Redirect URI known

Determine

- SPA only?
- MVC login?
- Resource server?
- Microsoft Graph required?
- App roles?
- Group claims?
- Multi-tenant?

Generate code that matches those decisions.

Prefer Spring Security idioms over handwritten authentication logic.

Follow Microsoft Learn recommendations unless the project explicitly requires otherwise.


