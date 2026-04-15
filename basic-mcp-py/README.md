### Info

### Usage

#### Docker

```sh
IMAGE=python:3.11-slim
docker pull $IMAGE
```

```sh

docker-compose up --build -d
```
```sh
docker-compose ps
```
```text
   Name            Command             State               Ports
-------------------------------------------------------------------------
mcp-client   python mcp_client.py   Exit 0
mcp-server   python mcp_server.py   Up (healthy)   0.0.0.0:9000->9000/tcp
```

```sh
docker-compose logs -f mcp-client | cut -f 2 -d '|'
```
```txt
Attaching to mcp-client
 2026-04-15 22:11:08,588 | INFO | SEND [initialize] id=1 payload={'jsonrpc': '2.0', 'id': 1, 'method': 'initialize'}
 2026-04-15 22:11:08,590 | INFO | RECV id=1 time=0.72ms response={'jsonrpc': '2.0', 'id': 1, 'result': {'status': 'ok', 'server': 'mcp-lab'}}
 {'jsonrpc': '2.0', 'id': 1, 'result': {'status': 'ok', 'server': 'mcp-lab'}}
 2026-04-15 22:11:08,591 | INFO | SEND [tools/list] id=2 payload={'jsonrpc': '2.0', 'id': 2, 'method': 'tools/list'}
 2026-04-15 22:11:08,592 | INFO | RECV id=2 time=0.46ms response={'jsonrpc': '2.0', 'id': 2, 'result': {'tools': [{'name': 'echo', 'description': 'Echo input text'}, {'name': 'uppercase', 'description': 'Uppercase text'}]}}
 2026-04-15 22:11:08,592 | INFO | Available tools: ['echo', 'uppercase']
 2026-04-15 22:11:08,592 | INFO | SEND [tools/call] id=3 payload={'jsonrpc': '2.0', 'id': 3, 'method': 'tools/call', 'params': {'name': 'echo', 'arguments': {'text': 'hello from docker network'}}}
 2026-04-15 22:11:08,593 | INFO | RECV id=3 time=0.32ms response={'jsonrpc': '2.0', 'id': 3, 'result': {'content': 'hello from docker network'}}
 {'jsonrpc': '2.0', 'id': 3, 'result': {'content': 'hello from docker network'}}
 2026-04-15 22:11:08,594 | INFO | echo result: None
mcp-client exited with code 0
```

#### Windows

Launch two cmd shells with the commands

```cmd
chcp 65001
set PYTHONIOENCODING=utf-8
set MCP_BIND=127.0.0.1
set MCP_PORT=9000
pushd app1
python.exe mcp_server.py
```
```cmd
chcp 65001
set PYTHONIOENCODING=utf-8
set MCP_BIND=127.0.0.1
set MCP_PORT=9000
pushd app2
python.exe mcp_client.py
```
the server console log will show
```text
[server] listening on 127.0.0.1:9000
[server] client: ('127.0.0.1', 65098)
```

the client console log will show
```text
2026-04-15 16:18:59,716 | INFO | SEND [initialize] id=1 payload={'jsonrpc': '2.0', 'id': 1, 'method': 'initialize'}
2026-04-15 16:18:59,717 | INFO | RECV id=1 time=1.01ms response={'jsonrpc': '2.0', 'id': 1, 'result': {'status': 'ok', 'server': 'mcp-lab'}}
{'jsonrpc': '2.0', 'id': 1, 'result': {'status': 'ok', 'server': 'mcp-lab'}}
2026-04-15 16:18:59,718 | INFO | SEND [tools/list] id=2 payload={'jsonrpc': '2.0', 'id': 2, 'method': 'tools/list'}
2026-04-15 16:18:59,718 | INFO | RECV id=2 time=0.00ms response={'jsonrpc': '2.0', 'id': 2, 'result': {'tools': [{'name': 'echo', 'description': 'Echo input text'}, {'name': 'uppercase', 'description': 'Uppercase text'}]}}
2026-04-15 16:18:59,718 | INFO | Available tools: ['echo', 'uppercase']
2026-04-15 16:18:59,718 | INFO | SEND [tools/call] id=3 payload={'jsonrpc': '2.0', 'id': 3, 'method': 'tools/call', 'params': {'name': 'echo', 'arguments': {'text': 'hello from docker network'}}}
2026-04-15 16:18:59,719 | INFO | RECV id=3 time=0.00ms response={'jsonrpc': '2.0', 'id': 3, 'result': {'content': 'hello from docker network'}}
{'jsonrpc': '2.0', 'id': 3, 'result': {'content': 'hello from docker network'}}

```
### Note on Python iterables

Python does not perform any “generator → set conversion” in set comprehensions.

A set comprehension like:
```python
{tool['name'] for tool in tools}
```
directly builds a set by iterating over the input, and is eager, not lazy.

Only explicit generator expressions like `(x for x in y)` are lazy.
### See Also 

  * https://github.com/forrestchang/andrej-karpathy-skills
