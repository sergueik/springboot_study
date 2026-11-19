---
name: convert-documents-to-markdown
description: Use when needing to convert a file (PDF, DOCX, PPTX, XLSX, XLS, image, audio, HTML, CSV, JSON, XML, ZIP, EPub, Outlook message) or a URL (web page, YouTube video) into Markdown. Triggers on tasks like "convert this document", "extract text from PDF", "turn this file into markdown", or any need to read/process a supported document format as text.
---

# convert-documents-to-markdown

## Overview

Converts documents, office files, and URLs to Markdown using a Dockerized
[MarkItDown](https://github.com/microsoft/markitdown) CLI.

**Project location:** `~/Projects/markitdown_docker/`
**Script:** `~/Projects/markitdown_docker/markitdown.sh`

Always run the script from the project directory, or use its absolute path. Plugins are always
active; LLM Vision OCR activates automatically when configured in `docker-compose.yml`.

## Supported formats

PDF, DOCX, PPTX, XLSX, XLS, PNG/JPG and other images, WAV/MP3 audio, HTML, CSV, JSON, XML, ZIP,
YouTube URLs, EPub, Outlook messages (.msg).

## Quick reference

```bash
MARKITDOWN=~/Projects/markitdown_docker/markitdown.sh
```

| Goal | Command |
|---|---|
| Convert file, print to stdout | `$MARKITDOWN /data/input.pdf` |
| Convert file, save output | `$MARKITDOWN /data/input.pdf -o /data/output.md` |
| Redirect stdout to file | `$MARKITDOWN /data/input.pdf > output.md` |
| Convert a URL | `$MARKITDOWN https://example.com/page > output.md` |
| Convert YouTube video | `$MARKITDOWN https://www.youtube.com/watch?v=ID > output.md` |
| List installed plugins | `$MARKITDOWN --list-plugins` |
| Show help | `$MARKITDOWN --help` |

## File path rule

The container mounts `~/Projects/markitdown_docker/` as `/data`. To convert any file on the host,
copy or symlink it there, then reference it as `/data/filename`:

```bash
# File at /home/user/docs/report.pdf
cp /home/user/docs/report.pdf ~/Projects/markitdown_docker/
~/Projects/markitdown_docker/markitdown.sh /data/report.pdf -o /data/report.md
```

Files already inside `~/Projects/markitdown_docker/` are directly accessible as `/data/filename`.

## Output file ownership

When using `-o`, the output file automatically inherits the ownership and permissions of the input
file. When redirecting stdout (`>`), the host shell creates the file under the current user.

## All options

| Option | Description |
|---|---|
| `[file_or_url]` | `/data/`-prefixed path or a URL |
| `-o FILE` | Write output to FILE instead of stdout |
| `-d` | Use Azure Document Intelligence |
| `-e ENDPOINT` | Azure Document Intelligence endpoint URL |
| `--list-plugins` | List installed plugins and exit |
| `--help` | Show CLI help |

> `--use-plugins` is always injected automatically. `--llm-client` and `--llm-model` are injected
> when `OPENAI_API_KEY` and `LLM_MODEL` are set in `docker-compose.yml`.

## Common mistakes

- **Forgetting `/data/` prefix** — the container cannot see host paths; always use `/data/filename`
- **Using relative paths** — use `/data/file.pdf`, not `./file.pdf` or `file.pdf`
- **Running from wrong directory** — always run `./markitdown.sh` from the project root where
  `docker-compose.yml` lives, otherwise Docker Compose won't find the service
