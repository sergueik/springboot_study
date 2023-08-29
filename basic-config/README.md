### Usage
```sh
curl -s http://localhost:8085/configs/xxx/list?newer=1690604340
```
```JSON
{"base:c":1690604632}
```
```sh
curl -s http://localhost:8085/configs/xxx/list
```
```JSON
{"a":1690604332,"b":1690604335,"base:c":1690604632}
```

```sh
curl -s  http://localhost:8085/configs/foo/load
```

```JSON
{"result":{},"status":"ok"}
```

```sh
curl -s http://localhost:8085/configs/foo/load?newer=1690604340
```
```JSON
{"result":"error message","status":"error"}
```
