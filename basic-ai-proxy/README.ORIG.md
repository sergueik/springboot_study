# ai-proxy — Universal AI Gateway

It's time to document this as a proper document. Below: purpose, alternatives, configs, OpenCode integration, request examples, and an answer on whether settings survive a restart.

## Why ai-proxy exists

`ai-proxy` is a lightweight self-hosted reverse proxy that provides a single access point to multiple AI providers (Gemini, OpenAI, Anthropic, and any other with an HTTP REST API), solving three problems:

- **Bypassing network restrictions** — the request leaves from a server in an allowed jurisdiction/network, instead of directly from the client machine, which may be blocked by a corporate firewall or the provider's geo-restrictions.
- **Key pass-through** — the proxy does not store or know the API keys; the client sends them as usual (`Authorization`, `x-api-key`, `?key=`), just through a different domain/address.
- **Geo-routing** — the same Docker image can be deployed on servers in different countries, and each instance becomes "regional" via environment variables, with no code changes.

## Alternatives

| Product | Type | Difference from ai-proxy |
| :-------------------------------- | :----------------------- | :----------------------------------------------------------- |
| LiteLLM Proxy | Open-source, self-hosted | A full enterprise AI gateway: unified OpenAI format for 100+ models, budgets, virtual keys, cost tracking, guardrails ([litellm](https://github.com/BerriAI/litellm)). Heavier and more feature-rich, but harder to deploy and requires a Python stack. |
| OpenRouter | SaaS (cloud) | A unified API to 300+ models from 60+ providers, but keys and billing go through OpenRouter itself rather than pass-through of your own keys ([dibi8](https://dibi8.com/resources/llm-frameworks/openrouter-unified-llm-api-gateway/)). |
| Kong AI Gateway / Bifrost | Enterprise self-hosted | Full-featured API gateways with plugins, metrics, guardrails — overkill for personal/team use, targeting enterprise scale ([futureagi](https://futureagi.com/blog/best-kong-ai-gateway-alternatives-2026/)). |
| Nginx/Envoy as a raw reverse proxy | Infrastructure level | Can do the same thing (path-based proxying), but without a built-in auth model (IP allowlist, admin token) and without convenient JSON-in-env configuration — you'd have to write that yourself. |

**The key difference of ai-proxy** is minimalism: a single Java-level logic layer, configuration entirely through environment variables with no image rebuild, no key storage, no database, no UI. This is a deliberate trade-off — simplicity and control at the cost of features like cost tracking, budgets, and guardrails that LiteLLM offers.

## Example `PROVIDERS_JSON`

```json
{
  "gemini": {
    "baseUrl": "https://generativelanguage.googleapis.com"
  },
  "openai": {
    "baseUrl": "https://api.openai.com"
  },
  "anthropic": {
    "baseUrl": "https://api.anthropic.com"
  }
}
```

Each top-level key (`gemini`, `openai`, `anthropic`) becomes the first path segment in your proxy's URL: `/gemini/...`, `/openai/...`, `/anthropic/...`.

## OpenCode configuration

In OpenCode you only set `baseURL` — keys are entered the standard way (`opencode auth login` or `{env:...}`), and OpenCode itself sends them in the headers as usual; the proxy just forwards them unchanged.

```json
{
  "$schema": "https://opencode.ai/config.json",
  "provider": {
    "google": {
      "options": { "baseURL": "https://ai-proxy-pog5.onrender.com/gemini/v1beta" }
    },
    "openai": {
      "options": { "baseURL": "https://ai-proxy-pog5.onrender.com/openai/v1" }
    },
    "anthropic": {
      "options": { "baseURL": "https://ai-proxy-pog5.onrender.com/anthropic/v1" }
    }
  }
}
```

## Example requests to AI through the proxy

**Gemini:**

```bash
curl --request POST \
  --url 'https://ai-proxy-pog5.onrender.com/gemini/v1beta/models/gemini-2.5-flash:generateContent?key=YOUR_GEMINI_KEY' \
  --header 'Content-Type: application/json' \
  --data '{"contents":[{"parts":[{"text":"Explain how AI works in a few words"}]}]}'
```

**OpenAI:**

```bash
curl --request POST \
  --url https://ai-proxy-pog5.onrender.com/openai/v1/chat/completions \
  --header 'Authorization: Bearer YOUR_OPENAI_KEY' \
  --header 'Content-Type: application/json' \
  --data '{"model":"gpt-4o-mini","messages":[{"role":"user","content":"hi"}]}'
```

**Anthropic:**

```bash
curl --request POST \
  --url https://ai-proxy-pog5.onrender.com/anthropic/v1/messages \
  --header 'x-api-key: YOUR_ANTHROPIC_KEY' \
  --header 'anthropic-version: 2023-06-01' \
  --header 'Content-Type: application/json' \
  --data '{"model":"claude-3-5-sonnet-20241022","max_tokens":100,"messages":[{"role":"user","content":"hi"}]}'
```

## Example requests for changing settings on the fly

**Allow the current IP for 12 hours (dynamic allowlist):**

```bash
curl --request POST \
  --url https://ai-proxy-pog5.onrender.com/admin/allow-ip \
  --header 'X-Admin-Token: YOUR_ADMIN_TOKEN'
```

**Adding/changing providers** is not done via a request to the proxy itself, but through Render Dashboard → Environment → `PROVIDERS_JSON` → Save → Restart. ai-proxy has no HTTP endpoint for editing the provider list — this setting is only read at process startup (see `ProviderConfigLoader`), so "on the fly" here means "without a git commit and without rebuilding the image," but not "without a restart."


## Running with Docker

### Using docker-compose

If you have a `docker-compose.yml` file (I've already created one for you), you can run `ai-proxy` with the following command:

```bash
docker-compose up -d
```

This command will build the image (if it hasn't already been built) and start the container in the background. The application will be accessible at `http://localhost:8080`.

To stop the container:

```bash
docker-compose down
```

### Using docker run

You can also run the Docker image directly using the `docker run` command. Replace `malexple/ai-proxy:latest` with the latest image tag.

```bash
docker run -d -p 8080:8080 \
-e "PORT=8080" \
-e "PROVIDERS_JSON={\"gemini\":{\"baseUrl\":\"https://generativelanguage.googleapis.com\"},\"openai\":{\"baseUrl\":\"https://api.openai.com\"}}" \
--name ai-proxy malexple/ai-proxy:latest
```

In this command:
- `-d` starts the container in the background.
- `-p 8080:8080` maps port 8080 on your host to port 8080 in the container.
- `-e "PORT=8080"` sets the `PORT` environment variable inside the container. - `-e "PROVIDERS_JSON=..."` sets the `PROVIDERS_JSON` environment variable for the proxy configuration. You will need to replace the JSON contents with your actual configuration.
- `--name ai-proxy` names the container for easier management.
- `malexple/ai-proxy:latest` is the name and tag of the Docker image to run.

To stop the container:

```bash
docker stop ai-proxy
```

To remove the container:

```bash
docker rm ai-proxy
```

## Do settings survive a restart?

There are two different types of settings with different fates here:

**Survive a restart:**

- `PROVIDERS_JSON`, `ALLOWED_IPS`, `ADMIN_TOKEN` — these are environment variables, stored in the Render Dashboard (or `.env`/docker run on a VDI) separately from the container process. On restart/redeploy, they are read again from the same place.

**Do not survive a restart:**

- IP addresses dynamically registered via `/admin/allow-ip` — they live **only in the process's memory** (a `ConcurrentHashMap` in `IpAllowlistService`). If the container restarts (for example, Render's free plan "sleeps" on idle and spins up again, or you trigger a Manual Deploy), this list is completely cleared, and you need to register the IP again via the same `/admin/allow-ip`.

This is a deliberate choice: the dynamic list is temporary and disposable by design, so as not to introduce persistent storage (a database/file) where an in-memory TTL is enough. If it matters to you that this list survives a restart, the only way is to move it to persistent storage (a file on a persistent disk or an external database), but that is an added complexity that's only worth doing if restarts are frequent for you and this genuinely gets in the way.
