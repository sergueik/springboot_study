### Info 


https://cryptography.io/en/latest/hazmat/primitives/key-derivation-functions/

 * Installing: 
### NOTE 
This works on Python __2.7__ but fails on Python __3.8.2__ with 

### Usage

* build image in stages
```sh
export IMAGE1=basic-jasypt-python-aes-2
docker image build -t $IMAGE1 -f Dockerfile.build .
```

```sh
export IMAGE=basic-jasypt-python-aes
docker image build -t $IMAGE -f Dockerfile  .
```
```sh
export NAME=basic-jasypt-python-aes
docker run --name $NAME -it $IMAGE sh
```

in the container

```sh
python app.py --value test --password password
```
```text
bda9aI5SDLaKkRVBobkm2w==
```
```sh
python app.py --operation decrypt --value bda9aI5SDLaKkRVBobkm2w== --password password
```
```text
test
```
the `MD5` `DES` encryption appears to be compatible with Perl, C#, Java: 

encrypt sample test with .Net app:
```powershell
Program\bin\Debug\Program.exe -value:something -password:password
```


```sh
python app.py --operation decrypt --value 'LWOxm4m1O+f/Zrn6WDfrPiHmZl+IugiE' --password password
```
```text
something
```
### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
docker image prune -f
```
### See Also 
  * https://cryptography.io/en/latest/hazmat/primitives/key-derivation-functions/
  * https://github.com/pyca/cryptography/issues/1958
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
