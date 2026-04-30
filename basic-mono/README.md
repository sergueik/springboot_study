### Usage

```sh
IMAGE=basic-mono
docker pull ubuntu:22.04
docker build -f Dockerfile -t $IMAGE .
```
followed by
```sh
docker run -it -p 4050:4050 $IMAGE
```


### Troubleshooting
`mono-complete` prompts to select `tzdata`:
```text
onfiguring tzdata
------------------

Please select the geographic area in which you live. Subsequent configuration
questions will narrow this down by presenting a list of cities, representing
the time zones in which they are located.

  1. Africa      4. Australia  7. Atlantic  10. Pacific  13. Legacy
  2. America     5. Arctic     8. Europe    11. US
  3. Antarctica  6. Asia       9. Indian    12. Etc
```
-  solved by setting environment
```
  GET https://api.nuget.org/v3-flatcontainer/nunit.extension.nunitv2driver/3.6.0/nunit.extension.nunitv2driver.3.6.0.nupkg
WARNING: Install failed. Rolling back...
Executing nuget actions took 1.89 sec
Error downloading 'NUnit.ConsoleRunner.3.8.0' from 'https://api.nuget.org/v3-flatcontainer/nunit.consolerunner/3.8.0/nunit.consolerunner.3.8.0.nupkg'.
  crypt32.dll assembly:<unknown assembly> type:<unknown type> member:(null)
  
```

Error downloading 'NUnit.ConsoleRunner.3.12.0' from 'https://api.nuget.org/v3-flatcontainer/nunit.consolerunner/3.12.0/nunit.consolerunner.3.12.0.nupkg'.
  crypt32.dll assembly:<unknown assembly> type:<unknown type> member:(null)
