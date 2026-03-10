### Info

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

### Usage

### Prep
```sh
pushd src/main/java
cp -R dev/danvega/dvaas/ example
rm -fr dev/
grep -ilr 'dev.danvega.dvaas' .
grep -ilr 'dev.danvega.dvaas' . | xargs -IX sed -i 's|dev.danvega.dvaas|example|g' X
popd
```
```
```sh
mvn -DskipTests package
```
### TODO

prune massively redundant code in 'example.tools' package space

