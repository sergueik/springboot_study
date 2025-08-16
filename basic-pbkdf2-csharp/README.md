### Info

### Testing

* add secret data

```sh
docker-compose run -it app2 sh
```
on vault node
```sh
export VAULT_ADDR=http://127.0.0.1:8200
VAULT_TOKEN=dmF1bHQgdG9rZW4=
vault kv put secret/app1/config password=secret
```
```text
===== Secret Path =====
secret/data/app1/config

======= Metadata =======
Key                Value
---                -----
created_time       2025-08-16T04:17:22.074577753Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            1
```
```sh
vault kv get secret/app1/config
```
```text
===== Secret Path =====
secret/data/app1/config

======= Metadata =======
Key                Value
---                -----
created_time       2025-08-16T04:17:22.074577753Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            1

====== Data ======
Key         Value
---         -----
password    secret
```
alernatively
```sh
docker-compose exec -e VAULT_ADDR=http://127.0.0.1:8200 -e VAULT_TOKEN=dmF1bHQgdG9rZW4=  -it app2 vault kv destroy -versions=1 secret/app1/config
```
```sh
docker-compose exec -e VAULT_ADDR=http://127.0.0.1:8200 -e VAULT_TOKEN=dmF1bHQgdG9rZW4=  -it app2 vault kv put secret/app1/config password=secret
```
```text
===== Secret Path =====
secret/data/app1/config

======= Metadata =======
Key                Value
---                -----
created_time       2025-08-16T04:37:04.145584516Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            1


```

```sh
docker-compose exec -e VAULT_ADDR=http://127.0.0.1:8200 -e VAULT_TOKEN=dmF1bHQgdG9rZW4= -it app2 vault kv get secret/app1/config
```
```text
===== Secret Path =====
secret/data/app1/config

======= Metadata =======
Key                Value
---                -----
created_time       2025-08-16T04:17:22.074577753Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            1

====== Data ======
Key         Value
---         -----
password    secret
```
in the browser, navigate to `http://192.168.99.100:8080/password`,  where `192.168.99.100` is `$(docker-machine ip`.

it will display
```json
{
  "result": "secret"
}
```
*  check aspnet.core server logs:
```sh
docker-compose logs -f app1
```
```text
app1  | GET /password
app1  | info: Microsoft.AspNetCore.Routing.EndpointMiddleware[0]
app1  |       Executing endpoint 'HTTP: GET /password'
app1  | getting secret path app1/config with vault token dmF1bHQgdG9rZW4=
app1  | Key: password, Value: secret
app1  | info: Microsoft.AspNetCore.Http.Result.JsonResult[1]
app1  |       Executing JsonResult, writing value of type '<>f__AnonymousType0`1[[System.String, System.Private.CoreLib, Version=6.0.0.0, Culture=neutral, PublicKeyToken=7cec85d7bea7798e]]'.
app1  | info: Microsoft.AspNetCore.Routing.EndpointMiddleware[1]
app1  |       Executed endpoint 'HTTP: GET /password'
app1  | info: Microsoft.AspNetCore.Hosting.Diagnostics[2]
app1  |       Request finished HTTP/1.1 GET http://192.168.99.100:8080/password
```

the error
```text
app1  |       Request starting HTTP/1.1 GET http://192.168.99.100:8080/password - -
app1  | GET /password
app1  | info: Microsoft.AspNetCore.Routing.EndpointMiddleware[0]
app1  |       Executing endpoint 'HTTP: GET /password'
app1  | getting secret path app1/config with vault token dmF1bHQgdG9rZW4=
app1  | Message: {"errors":[]}
app1  |
app1  | Stack Trace:
app1  |    at VaultSharp.Core.Polymath.MakeRequestAsync[TResponse](String resourcePath, HttpMethod httpMethod, Object requestData, IDictionary`2 headers, Boolean rawResponse, Action`1 postResponseAction)
app1  |    at VaultSharp.Core.Polymath.MakeVaultApiRequest[TResponse](String resourcePath, HttpMethod httpMethod, Object requestData, Boolean rawResponse, Action`1 postResponseAction, String wrapTimeToLive, Boolean unauthenticated)
app1  |    at VaultSharp.Core.Polymath.MakeVaultApiRequest[TResponse](String mountPoint, String path, HttpMethod httpMethod, Object requestData, Boolean rawResponse, Action`1 postResponseAction, String wrapTimeToLive, Boolean unauthenticated)
app1  |    at VaultSharp.V1.SecretsEngines.KeyValue.V2.KeyValueSecretsEngineV2Provider.ReadSecretAsync(String path, Nullable`1 version, String mountPoint, String wrapTimeToLive)
app1  |    at CryptoService.Server.<>c.<<Main>b__6_1>d.MoveNext() in /src/Server/Server.cs:line 47
app1  | info: Microsoft.AspNetCore.Http.Result.JsonResult[1]
app1  |       Executing JsonResult, writing value of type '<>f__AnonymousType0`1[[System.String, System.Private.CoreLib, Version=6.0.0.0, Culture=neutral, PublicKeyToken=7cec85d7bea7798e]]'.
app1  | info: Microsoft.AspNetCore.Routing.EndpointMiddleware[1]
app1  |       Executed endpoint 'HTTP: GET /password'
app1  | info: Microsoft.AspNetCore.Hosting.Diagnostics[2]
app1  |       Request finished HTTP/1.1 GET http://192.168.99.100:8080/password - - - 200 - application/json;+charset=utf-8 999.6727ms

```
indicates the secret was not added to vault
```sh
docker-compose exec -e VAULT_ADDR=http://127.0.0.1:8200 -e VAULT_TOKEN=dmF1bHQgdG9rZW4=  -it app2 vault kv  get secret/app1/config

```
No value found at secret/data/app1/config```text
```
* run encryption and decription, providing password explicitly

```sh
http://192.168.99.100:8080/encrypt?password=secret&value=test
```
```json
{ 
   "result": "jLMUcMfHHDctVdW06D1W9st6iIJIv1Obcd7jymV/y4yqo9Phspn0KxbVskVRHJaw",
   "original ":"test"
}
```
```
http://192.168.99.100:8080/decrypt?password=secret&value=jLMUcMfHHDctVdW06D1W9st6iIJIv1Obcd7jymV%2Fy4yqo9Phspn0KxbVskVRHJaw
```json
{
  "result": "test",
  "original": "jLMUcMfHHDctVdW06D1W9st6iIJIv1Obcd7jymV/y4yqo9Phspn0KxbVskVRHJaw"
}

```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
