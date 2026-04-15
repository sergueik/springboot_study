### Info



```sh
IMAGE=python:3.11-slim
docker pull $IMAGE
```

```sh

docker-compose up --build -d
```

```sh
docker-compose logs -f mcp-client | cut -f 2 -d '|'
```
```
Attaching to mcp-client
 {'jsonrpc': '2.0', 'id': 1, 'result': {'status': 'ok', 'server': 'mcp-lab'}}
 {'jsonrpc': '2.0', 'id': 2, 'result': {'tools': [{'name': 'echo', 'description': 'Echo input text'}, {'name': 'uppercase', 'description': 'Uppercase text'}]}}
 {'jsonrpc': '2.0', 'id': 3, 'result': {'content': 'hello from docker network'}}
```
