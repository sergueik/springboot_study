###  Info


This directory contains docker build flow to construct  __CobolToJson__ - *Convert Cobol Data Files to JSON*
 - with all its dependencies from the source. To enforce portability, all processes run in Docker containers, multi-step

### Background

* Build Stages:


![build stage diagram](https://github.com/sergueik/springboot_study/blob/master/basic-cobol2json-cb2xml-jrecord-build/screenshots/build-stages.png)


* Docker Build:

![build flow diagram](https://github.com/sergueik/springboot_study/blob/master/basic-cobol2json-cb2xml-jrecord-build/screenshots/flow-diagram.png)

### Usage

* get images
```sh
docker pull maven:3.9.5-eclipse-temurin-11-alpine
docker pull eclipse-temurin:11-jre-alpine
```
* build 1st dependency
```
export REPO=https://github.com/bmTas/cb2xml
export COMMIT=97f8dc8
export VERSION=1.01.08
cp avoid_c2bxml_deps.txt avoid_deps.txt
docker build -t cb2xml --build-arg REPO=$REPO --build-arg COMMIT=$COMMIT --build-arg VERSION=$VERSION -f Dockerfile.BUILD-DEPENDENCY .
```
* build 2nd dependdncy
```
export REPO=https://github.com/bmTas/JRecord
export COMMIT=f50ece71
export VERSION=0.93.3
cp avoid_jrecord_deps.txt avoid_deps.txt
docker build -t jrecord --build-arg REPO=$REPO --build-arg COMMIT=$COMMIT --build-arg VERSION=$VERSION -f Dockerfile.BUILD-DEPENDENCY .
```
build app
```sh
export REPO=https://github.com/bmTas/CobolToJson
export COMMIT=99b0aa2
docker build -t cobol2json --build-arg REPO=$REPO --build-arg COMMIT=$COMMIT -f Dockerfile.BUILD-APP .
```
* build the app runtime container

```sh
docker build -t app -f Dockerfile.APP .
```
* confirm the app can run
```sh
docker run app
```
```text

  Program options:

          -cobol	- Cobol  copybook used to "interpret" the data (you must supply either a cobol or cb2xml copybook
          -cb2xml	- Cb2xml copybook used to "interpret" the data

          -input	- Input file
          -output	- Output file
          -font  	- Characterset used in the Cobol data file (e.g. IBM037 for US-EBCIDIC)
          -pretty  	- Wether to pretty print the JSON

          -dropCopybookName	- (true/false) wether to drop the cobol copybook name from the start of the Json Tags

          -tagFormat     	- How Cobol Variable names are reformated to Xml tags:
                Asis       - Use the Cobol Variable name
                Underscore - Convert - to _,         COBOL-VAR-NAME ==> COBOL_VAR_NAME
                CamelCase  - Convert to Camel Case,  COBOL-VAR-NAME ==> cobolVarName

          -fileOrganisation	- "file organisation" of the Cobol data file
		Text    	- Standard Windows/Unix text file (single byte characterset)
		FixedWidth	- File where lines (records) are the same length no \n
		UnicodeText    	- Standard Windows/Unix Unicode text file
		FixedText    	- Fixed width Text (possibly unicode)
		Mainframe_VB	- Mainframe VB, file consists of <record-length><record-data>
		GNUCobol_VB	- GNU Cobol VB, file consists of <record-length><record-data>

          -dialect	- Cobol Dialect
		Mainframe	- Mainframe cobol
		Futjitsu	- Fujitsu PC cobol
		GNUCobol	- GNU Cobol (little endian, ie intel)
		GNUCobolBE	- GNU Cobol (big endian, ie IBM, Sun(oracle))

          -split	- Split Copybook Option
		None	- No Split
		01	- Split on 01
		Highest	- On Highest Repeating

          -recordSelection	- Record Selection, can be used multiple times
                            	  format: -recordSelection RecordName field=value

          -recordParent   	- Record Parent, can be used multiple times
                            	  format: -recordParent    RecordName ParentRecord


```
* run example
```sh
docker run -w /app/Example -v $(pwd)/Example:/app/Example app -cobol cobol/DTAR020a.cbl -fileOrganisation FixedWidth -font cp037 -input in/DTAR020.bin -output DTAR020.json
```
```sh
ls -l Example/DTAR020.json
```
```text
-rw-r--r-- 1 root root 90174 Jan 18 20:57 Example/DTAR020.json```
```
```sh
cat Example/DTAR020.json | jq '.sss[0:3]'
```
```json
[
  {
    "DTAR020-KCODE-STORE-KEY": {
      "DTAR020-KEYCODE-NO": "69684558",
      "DTAR020-STORE-NO": 20
    },
    "DTAR020-DATE": 40118,
    "DTAR020-DEPT-NO": 280,
    "DTAR020-QTY-SOLD": 1,
    "DTAR020-SALE-PRICE": 19
  },
  {
    "DTAR020-KCODE-STORE-KEY": {
      "DTAR020-KEYCODE-NO": "69684558",
      "DTAR020-STORE-NO": 20
    },
    "DTAR020-DATE": 40118,
    "DTAR020-DEPT-NO": 280,
    "DTAR020-QTY-SOLD": -1,
    "DTAR020-SALE-PRICE": -19
  },
  {
    "DTAR020-KCODE-STORE-KEY": {
      "DTAR020-KEYCODE-NO": "69684558",
      "DTAR020-STORE-NO": 20
    },
    "DTAR020-DATE": 40118,
    "DTAR020-DEPT-NO": 280,
    "DTAR020-QTY-SOLD": 1,
    "DTAR020-SALE-PRICE": 5.01
  }
]
```
### Inventory

```sh
docker image ls
```
```text
REPOSITORY        TAG                               IMAGE ID       CREATED          SIZE
app               latest                            8bbbff74016c   19 minutes ago   171MB
cobol2json        latest                            bf44405afa07   20 minutes ago   338MB
jrecord           latest                            3470d9d371da   22 minutes ago   381MB
cb2xml            latest                            5e21966a2d42   24 minutes ago   364MB
eclipse-temurin   11-jre-alpine                     f135099692b9   2 months ago     169MB
maven             3.9.5-eclipse-temurin-11-alpine   37ef041f8432   2 years ago      305MB
```

### Docker-specific Note

The container is executed with an explicit working directory (`-w /app/Example`) and a narrowly scoped bind mount (`-v $(pwd)/Example:/app/Example`). This design choice is intentional and technically sound.

The application JAR is baked into the image at build time and resides in the image filesystem. Docker’s overlay filesystem semantics dictate that **a bind mount completely masks the target directory inside the container**. If the entire `/app` directory (or `/app/Example`) were mounted from the host without care, the pre-copied JAR inside the image would disappear at runtime, replaced by the host directory contents. This is a common and subtle Docker pitfall.

By mounting **only the data subdirectory** and keeping the executable artifacts inside the image layer, the container preserves:
- a deterministic runtime environment,
- a stable executable JAR,
- and a clean separation between *code* (image) and *data* (volume).

This approach prevents accidental shadowing of the application binary while still allowing input/output files to flow naturally between host and container.

The result confirms correct behavior:
- The JSON output is generated inside the mounted directory and persists on the host.
- File ownership reflects container execution (`root:root`), which is expected unless user remapping is configured.
- The output size (~90 KB) is consistent with full record expansion rather than truncated or schema-only output.

The JSON content itself demonstrates correctness at multiple levels:
- EBCDIC input (`cp037`) is decoded accurately.
- Fixed-width parsing preserves signed numeric semantics (negative quantities and prices are correctly represented).
- COBOL group fields are mapped into nested JSON objects rather than flattened or lossy structures.
- Decimal handling is preserved (`5.01`), indicating correct PIC and scale interpretation rather than integer coercion.

Using `jq` to slice and inspect the first records provides a strong sanity check: the data is structurally valid, semantically faithful to the COBOL copybook, and immediately consumable by downstream tooling without post-processing.

This execution pattern also scales well to automation. Because the image encapsulates the executable and runtime dependencies, automated pipelines can safely:
- pull the image,
- mount only input/output directories,
- run deterministic conversions,
- and archive results.

For automated image acquisition, the same model supports scripted downloads via `docker pull` (or registry mirroring in air-gapped environments), without requiring any changes to runtime invocation. The image remains immutable; only data varies.

In short, this setup is good because it respects Docker filesystem semantics, preserves image integrity, produces verifiable results, and is automation-friendly by design.

### Troubleshooting
            
```text
[ERROR] Some problems were encountered while processing the POMs:
[FATAL] Non-readable POM /build/src/Source/pom.xml: /build/src/Source/pom.xml (No such file or directory) @
 @
```
examine the project
```sh
git clone --depth 50 https://github.com/bmTas/JRecord $TEMP/project; pushd $TEMP/project; find . -name pom.xml; popd
```
fix the loose maven command in the `Dockerfile`
```text
[ERROR] Failed to execute goal on project cobolToJson: Could not resolve dependencies for project net.sf:cobolToJson:jar:0.93.4: The following artifacts could not be resolved: net.sf.jrecord:JRecord:jar:0.93.4 (absent): Could not find artifact net.sf.jrecord:JRecord:jar:0.93.4 in jitpack.io (https://jitpack.io) -> [Help 1]
```
examine the bad commit and use the one commit earlier
```
https://github.com/bmTas/JRecord/commit/5eee51b478cf8d9480014273e80ed02b68198586
```
failure to run the fat jar:
```sh
docker run -it cobol2json sh
```
```text
Error: Unable to initialize main class net.sf.cobolToJson.Data2Json
Caused by: java.lang.NoClassDefFoundError: net/sf/JRecord/Common/RecordException

```
```sh
docker run -it --entrypoint='' cobol2json sh
```

```sh
unzip -l src/target/cobolToJson-0.93.3.jar |grep jar
```
```text
Archive:  src/target/cobolToJson-0.93.3.jar
```
> NOTE: no dependency jars in the fat jar
```txt
```

```sh
docker run -v $(pwd):/app app -cobol Example/cobol/DTAR020a.cbl -fileOrganisation FixedWidth -font cp037 -input Example/in/DTAR020.bin -output DTAR020.json
```
```text
Error: Unable to access jarfile /app/cobolToJson.jar
```
one should not map the developer machine directory to workdir

#### Maven Offline Dependency Installation and Fat JAR Integrity

This project is built in a fully offline and restricted environment. Maven does not treat a dependency as available simply because a JAR file exists on disk. For Maven (via the Aether resolver), an artifact is considered installed only
when its full repository metadata is present and consistent.

A valid local Maven artifact must include, at minimum, the following files in the local repository layout:

```text
~/.m2/repository/<groupId path>/<artifactId>/<version>/
  <artifactId>-<version>.jar
  <artifactId>-<version>.pom
  &#95;remote.repositories
  <artifactId>-<version>.jar.sha1   (optional but commonly present)
```

If any of these files are missing, Maven may attempt to resolve the artifact from a remote repository, fail with `ArtifactNotFoundException`, or produce an incomplete build output. Copying only the JAR file is therefore insufficient; the POM and repository metadata are mandatory.

During `mvn dependency:go-offline`, `mvn package`, or shaded (fat) JAR builds,
Maven performs strict dependency validation.
It reads the application `pom.xml`, resolves all declared dependencies, validates each dependency as a complete Maven artifact, and fails immediately if any artifact metadata is incomplete. For this reason, all dependencies must be explicitly installed into a local Maven repository before the application build executes.

In unrestricted environments this is typically done with:

```sh
mvn -DskipTests install
```

In this project, upstream source repositories cannot be collected from internet. Instead, dependencies are built in isolated Docker builder images, and the resulting local Maven repository directories are copied verbatim into the application build image. Copying the entire artifac

### See Also
 
 * [cb2xml](https://github.com/bmTas/cb2xml)
 * [JRecord](https://github.com/bmTas/JRecord)
 * [CobolToJson](https://github.com/bmTas/CobolToJson)
 * [Sourceforge download](https://sourceforge.net/projects/coboltojson/) - __CobolToJson__
*Convert Cobol Data Files to JSON*
