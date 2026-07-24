### Info

replica of the [mermaid-live-editor](https://github.com/mermaid-js/mermaid-live-editor) container for Edit, preview and share mermaid charts/diagrams taken
at `efafad1e8709854e77bea2d6f1abf212ed7482a9` to use node 18

### Usage
```sh
docker build -t mermaid-live -f Dockerfile .
```
```sh
docker run --name mermaid-live -p 8080:80  -d mermaid-live
```
```sh
docker ps 
```
```text
```
CONTAINER ID        IMAGE               COMMAND                  CREATED             STATUS              PORTS                  NAMES
85762a3f9290        mermaid-live                "/docker-entrypoint.…"   4 minutes ago       Up 4 minutes        0.0.0.0:8080->80/tcp   mermaid-live
```sh
curl -I http://192.168.99.100:8080/
```
```text
HTTP/1.1 200 OK
Server: nginx/1.30.3
Date: Fri, 24 Jul 2026 12:32:42 GMT
Content-Type: text/html
Content-Length: 2550
Last-Modified: Fri, 24 Jul 2026 05:51:14 GMT
Connection: keep-alive
ETag: "6a62fd52-9f6"
Accept-Ranges: bytes
```
open in the browser
![editor](screenshots/capture-editor.png)
### Background

Live https://mermaid.live/ to learn


There official __Mermaid Live Editor__  Docker image is hosted on [GitHub Container Registry](ghcr.io/mermaid-js/mermaid-live-editor/mermaid-live-editor) (__GHCR__) 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
