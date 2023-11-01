### Usage
 * build applications:

```sh
export IMAGE=jasypt-go
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile .
export NAME=jasypt-go
docker container rm $NAME
docker run --name=$NAME -it $IMAGE go run jasypt.go password test
```

### Verify

* navigate to directory the __c# Jasypt__ is checked out from [powershell_samples](https://github.com/sergueik/powershell_samples/tree/master/csharp/jasypt-csharp) and run
```powershell
 .\jasypt.ps1 -value 'VIYeyCyD9/55Hhd1IajXmA==' -operation decrypt  -password password
```
```text
test
```
### See Also
  * `PBEWithMD5AndDES ` [golang example](https://github.com/LucasSloan/passwordbasedencryption/blob/master/pbewithmd5anddes.go)
  * [another](https://github.com/andrewstuart/go-jasypt/tree/master) `PBEWithMD5AndDES` implementation in golang
  * [example](https://github.com/Mystery00/go-jasypt) of `PBEWithHMACSHA512AndAES_256` - somewhat excessive breakdown into chained classes (~ 13 source files) with only two needed `pbe_hmacsha512_aes256.go` and `pbe_md5_des.go`, rest are iterfaces and proxy classes - need flatten  before being useful
  * [command line client for the same](https://github.com/brunofjesus/jasypt-cli)
  * [example](https://github.com/wispeeer/jasypt-go) of an Jasypt non compatible project
