### Info
https://github.com/judsonc/react-upload-uppy
https://uppy.io/
https://github.com/transloadit/uppy with over 10K commits

### Usage


```sh
docker pull node:18.1.0-alpine
docker pull maven:3.9.5-eclipse-temurin-11-alpine
docker pull eclipse-temurin:11-jre-alpine
```

```sh
IMAGE=uppy-tus-react
docker build -t $IMAGE -f Dockerfile .
```

run both frontend and backend on port `8080`:
```sh
IMAGE=uppy-tus-react
CONTAINER=example
docker container rm $CONTAINER
docker run -d -p 8080:8080 --name $CONTAINER $IMAGE
```

```
docker logs $CONRAINER
```

```text
LoggingFilter      : Before request [POST /api/upload, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"0", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36", upload-length:"447", sec-ch-ua:""Google Chrome";v="149", "Chromium";v="149", "Not)A;Brand";v="24"", upload-metadata:"relativePath bnVsbA==,name dGVzdC50eHQ=,type dGV4dC9wbGFpbg==,filetype dGV4dC9wbGFpbg==,filename dGVzdC50eHQ=", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://192.168.99.100:8080", sec-fetch-site:"cross-site", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://192.168.99.100:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9"]]
2026-06-18 15:18:50.314  INFO 34304 --- [nio-8080-exec-6] m.d.t.s.c.CreationPostRequestHandler     : Created upload with ID 21b73f31-647a-45be-b010-6701031015da at 1781810330301 for ip address 0:0:0:0:0:0:0:1 with location /api/upload/21b73f31-647a-45be-b010-6701031015da
2026-06-18 15:18:50.315  INFO 34304 --- [nio-8080-exec-6] e.controller.TusFileUploadController     : upload not started
2026-06-18 15:18:50.316 DEBUG 34304 --- [nio-8080-exec-6] o.s.w.f.CommonsRequestLoggingFilter      : After request [POST /api/upload, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"0", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36", upload-length:"447", sec-ch-ua:""Google Chrome";v="149", "Chromium";v="149", "Not)A;Brand";v="24"", upload-metadata:"relativePath bnVsbA==,name dGVzdC50eHQ=,type dGV4dC9wbGFpbg==,filetype dGV4dC9wbGFpbg==,filename dGVzdC50eHQ=", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://192.168.99.100:8080", sec-fetch-site:"cross-site", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://192.168.99.100:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9"]]
2026-06-18 15:18:50.324 DEBUG 34304 --- [nio-8080-exec-3] o.s.w.f.CommonsRequestLoggingFilter      : Before request [OPTIONS /api/upload/21b73f31-647a-45be-b010-6701031015da, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", accept:"*/*", access-control-request-method:"PATCH", access-control-request-headers:"content-type,tus-resumable,upload-offset", origin:"http://192.168.99.100:8080", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36", sec-fetch-mode:"cors", sec-fetch-site:"cross-site", sec-fetch-dest:"empty", referer:"http://192.168.99.100:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9"]]
2026-06-18 15:18:50.327 DEBUG 34304 --- [nio-8080-exec-3] o.s.w.f.CommonsRequestLoggingFilter      : After request [OPTIONS /api/upload/21b73f31-647a-45be-b010-6701031015da, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", accept:"*/*", access-control-request-method:"PATCH", access-control-request-headers:"content-type,tus-resumable,upload-offset", origin:"http://192.168.99.100:8080", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36", sec-fetch-mode:"cors", sec-fetch-site:"cross-site", sec-fetch-dest:"empty", referer:"http://192.168.99.100:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9"]]
2026-06-18 15:18:50.332 DEBUG 34304 --- [io-8080-exec-10] o.s.w.f.CommonsRequestLoggingFilter      : Before request [PATCH /api/upload/21b73f31-647a-45be-b010-6701031015da, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"447", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36", sec-ch-ua:""Google Chrome";v="149", "Chromium";v="149", "Not)A;Brand";v="24"", upload-offset:"0", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://192.168.99.100:8080", sec-fetch-site:"cross-site", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://192.168.99.100:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]
2026-06-18 15:18:50.365  INFO 34304 --- [io-8080-exec-10] m.d.t.s.core.CorePatchRequestHandler     : Upload with ID 21b73f31-647a-45be-b010-6701031015da at location /api/upload/21b73f31-647a-45be-b010-6701031015da finished successfully
2026-06-18 15:18:50.385  INFO 34304 --- [io-8080-exec-10] e.controller.TusFileUploadController     : upload complete
2026-06-18 15:18:50.398  INFO 34304 --- [io-8080-exec-10] e.controller.TusFileUploadController     : info: id: 21b73f31-647a-45be-b010-6701031015da filename: test.txt local path: C:\Users\kouzm\AppData\Local\Temp\tus\uploads\21b73f31-647a-45be-b010-6701031015da\data
2026-06-18 15:18:50.398 DEBUG 34304 --- [io-8080-exec-10] o.s.w.f.CommonsRequestLoggingFilter      : After request [PATCH /api/upload/21b73f31-647a-45be-b010-6701031015da, client=0:0:0:0:0:0:0:1, headers=[host:"localhost:8080", connection:"keep-alive", content-length:"447", sec-ch-ua-platform:""Windows"", tus-resumable:"1.0.0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36", sec-ch-ua:""Google Chrome";v="149", "Chromium";v="149", "Not)A;Brand";v="24"", upload-offset:"0", sec-ch-ua-mobile:"?0", accept:"*/*", origin:"http://192.168.99.100:8080", sec-fetch-site:"cross-site", sec-fetch-mode:"cors", sec-fetch-dest:"empty", referer:"http://192.168.99.100:8080/", accept-encoding:"gzip, deflate, br, zstd", accept-language:"en-US,en;q=0.9", Content-Type:"application/offset+octet-stream;charset=UTF-8"]]

...
2026-06-18 19:31:53.328 DEBUG 1 --- [nio-8080-exec-8] o.s.w.f.CommonsRequestLoggingFilter      : Before request [POST /api/uploads/finalize, client=192.168.99.1, headers=[host:"192.168.99.100:8080", connection:"keep-alive", content-length:"0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36", body:"{"uploadId":"69df7e54-70cb-4033-bd07-d6896b94263a"}", accept:"*/*", origin:"http://192.168.99.100:8080", referer:"http://192.168.99.100:8080/", accept-encoding:"gzip, deflate", accept-language:"en-US,en;q=0.9", Content-Type:"application/json;charset=UTF-8"]]
2026-06-18 20:01:32.595 DEBUG 1 --- [nio-8080-exec-8] o.s.w.f.CommonsRequestLoggingFilter      : Before request [POST /api/uploads/finalize, client=192.168.99.1, headers=[host:"192.168.99.100:8080", connection:"keep-alive", content-length:"0", user-agent:"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Safari/537.36", body:"{"uploadId":"30dacb9b-1ca6-49cb-9b5f-fc1af9d740df"}", accept:"*/*", origin:"http://192.168.99.100:8080", referer:"http://192.168.99.100:8080/", accept-encoding:"gzip, deflate", accept-language:"en-US,en;q=0.9", Content-Type:"application/json;charset=UTF-8"]]
2026-06-18 20:01:32.718  WARN 1 --- [nio-8080-exec-8] .w.s.m.s.DefaultHandlerExceptionResolver : Resolved [org.springframework.http.converter.HttpMessageNotReadableException: Required request body is missing: public org.springframework.http.ResponseEntity<?> example.controller.FinalizeController.uploadJson(example.dto.FinalizeRequest) throws java.lang.Exception]

```

```sh
curl -X POST -d '{"uploadId":"30dacb9b-1ca6-49cb-9b5f-fc1af9d740df"}' -H 'Content-Type: application/json' http://192.168.99.100:8080/api/uploads/finalize
```
```json
{
  "filename": "example.bin",
  "uploadId": "30dacb9b-1ca6-49cb-9b5f-fc1af9d740df",
  "status": "OK"
}
```

```sh
dd if=/dev/urandom of=test.bin bs=1M count=50
```
