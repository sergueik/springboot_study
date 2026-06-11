
### Info

This directory contains moderately modified Springboot Commandline runner posting JSON containing file contents encoded as base64

### Usage
```sh
dd if=/dev/urandom of=test.bin bs=10K count=1
```
```sh
mvn clean package
```
run ../basic-upload app
```
java -jar target\example.commandlinerunner-resttemplate.jar -filename test.bin
```
```text
Uploading: test.bin
{"id":0,"foo":"alpha","bar":"beta","filename":"test.bin","contentType":"application/octet-stream","contentBase64":"ZivWnZYQVgBeBLVM5T10FVnravxHB8W7D5Pta7K1FmiqjpWFluHpGVXmTlfdSifPG69TAidDVtxlKijHJM7W9J33/tvq+6JZ6xTotvM4M4Xx4+SKG9gNYuY1LBqompb7CaZl3nqHRqPluc0hVs86HUPB9kT+GKVzv+yrHDelhZEqCRQ+f8Yyd065DW66FtArkEo9A7gD2NKwaqkVgBnV0rg6Orbi3VS9pIAEUrZYEAL4RkzNuwW4hwjapRAah4z5iHcNNnICaAK3cUGusHRXe42ArXq7OOThtWEHjsJurJJl3DqFkcOpp7jQkM4o5QmeabxeVg/zYQaNzRhI1g3o55pSB7SvPRrQkI7US6O8qUtRMZKN5VzHTkqPFtpfJYbCWCvprh02PwpX5/jIEZ4WutDjcQr2PAQZJj4RMycdAJG9ylRW4bSUV8OgMNWQjcXvO7HjC6XGPxKAxMPv1mfa+CHyMBwzbzcyFkux/DTIBHNci7nyrGTndT2twtNlf5CaK51uSvtuLeZx4ZI994SotFF83kw+g2KkpJijV9KPmIeVSY+nUFuWhL8rOCu9YHrKgk4vL8PdZkXxUkzaHvq6ISmJAGM4D+fiLHxL7BZ6Wz1vOQT93KFreOxsBjhunydFlAp89ttUh8xhAH0+a5a1dHCbnzZEy9uDXtA26/m29O47uq5TCqTmHL+azDlvLlgztmYtN7qx1teRNTGubaG0v15r8EbWDNK0Hg56vIS8lsEoqS0q//5I/R3h3Qkx8egmxGPT7XGtmwbzmz66UlbQwBSvNAFxJLr8P23NdUtv1eZV/PDAEfUsHzhfo0soQ1qkf1jSLkmlz5ITALxUApcvh9aF9Wzjtxz37IRqF1No7j7JkVmD4r5EdOHC0Mi3cM4GuHH8LiCdOxCgZjLuySdzFK96tjYEs4gDlYxuPdN8ZFfIS8LASi9tLDyyrnXBC01tIQax3dGueW8NmlNlbW37UcD4YM563MBxqWSsAd4IcVo7lKnHURPEGsW4soxCNXpUCg8tYJJ3J/jBubwIP0GkFmebDLUz5fYmLvOpVzdl6lcSa0IqZhoCong/tV13fDBoD10ljBEciNylvcE+OplIVUAGjnCUZL/DIOcnfkB6UW4e93o6Yyi0SBxVRPAl1hAmnkQUaT5+QMzn2+Ukt2AAvIhdJxGm6lx44P3MZYzNclU8ieljvXgv3WybNoTgr/yfHtDOVoss6NOTGMuSJ0uDSuJO+...

2026-06-11 19:27:20.881  INFO 25480 --- [           main] example.Application                      : Response: "received: foo \u003d alpha bar \u003d beta filename \u003d test.bin data: 10240 bytes"
```
> NOTE: there is a bug somewhere in the logging leading to actual content of the file being logged. It is console only - the server accepts and responds with a success message.
### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

