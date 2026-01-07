### Info

This directory contains a poc project to illustrate:

1. Using [Aqua Security](https://www.aquasec.com/about-us/) [Trivy](https://trivy.dev/) in default scanner mode
2. No need to run any containers
3. Extracting artifacts safely from images
4. Verifying Java JAR signatures independently in a step bundled after __Trivy__

The cryptographic integrity is verified separatly, it proves the image was produced by the vendor and not tampered with

  | Requirement  | Supported? |How |
  | -------------------------------------------------- | ------------------------ | ---------------------------------------------------------- |
  | Scan container images for CVEs                     | ✅	                      | MicroScanner (deprecated) / Trivy / Aqua Enterprise        |
  | Customize policies (what gets checked)             | ❌ in MicroScanner alone | ✅ in Aqua Enterprise with custom checks                   |
  | Enforce “JAR must be signed by authority” policy   | ❌ out-of-the-box        | ✅ via custom script in Enterprise policy (or CI pipeline) |


![verify](screenshots/capture-macedonian-snooting-scene.png)

recommendation is to use Trivy Instead of legacy MicroScanner (when not on Aqua Enterprise)

with pseudo code that runs custom JAR signing check:
```sh
trivy image myapp:latest || exit 1
./verify-jar-signature.sh /path/to/file.jar
```

the __CI CD__ platform heuristics of combining the shell calls needs to be followed

#### Why Trivy Doesn't Do Image Scan Directly

__Trivy__ is designed for:

* CVE scanning (OS + language deps)
* Misconfiguration checks (Dockerfile, K8s, IaC, any other latest known msconfigurations classified by Aquasec)
* Secret scanning
* License checks

__Trivy__ does *not* parse JAR signing metadata and has no built-in rule like:

"Fail if JAR is not signed by CA X"

So anything claiming "Trivy-native JAR signature rule" is misleading.

But Trivy does integrate extremely well with external policy and CI logic, which is what DevSecOps actually want.
---
### Usage

  *  build a dummy jar
```sh
echo "hello ops" > target/hello.txt
jar cf target/app.jar -C target .
```
  
  * create a demo CA
```sh
keytool -genkeypair -alias demo-ca -keyalg RSA -keysize 4096 -validity 3650 -dname "CN=DemoRootCA, OU=Ops, O=ExampleCorp, L=NA, C=US" -ext bc:c -keystore demo-ca.jks -storepass changeit -keypass changeit
```
  * export CA certificate
```sh
keytool -exportcert -alias demo-ca -keystore demo-ca.jks -storepass changeit -rfc -file demo-ca.crt 
```

```txt
Certificate stored in file <demo-ca.crt>
```
  * create a signing certificate (issued by the CA)
```sh
keytool -genkeypair -alias app-signer -keyalg RSA -keysize 2048 -validity 365 -dname "CN=DemoAppSigner, O=ExampleCorp, C=US" -keystore app-signer.jks -storepass changeit -keypass changeit
```
  * Create CSR 
```sh
keytool -certreq -alias app-signer -keystore app-signer.jks -storepass changeit -file app-signer.csr
```
  * sign CSR with CA: 
```sh
keytool -gencert -alias demo-ca -keystore demo-ca.jks -storepass changeit -infile app-signer.csr -outfile app-signer.crt -validity 365 -ext KU=digitalSignature -ext EKU=codeSigning
```
  * Import CA and signed cert into signer keystore
```sh
keytool -importcert -alias demo-ca -keystore app-signer.jks -storepass changeit -file demo-ca.crt -noprompt
```
```txt
Certificate was added to keystore
```
keytool -importcert -alias app-signer -keystore app-signer.jks -storepass changeit -file app-signer.crt
```
```
Certificate reply was installed in keystore
```
  * sign a jar
```
jarsigner -keystore app-signer.jks -storepass changeit target\app.jar app-signer
```
```
jar signed.

Warning:
No -tsa or -tsacert is provided and this jar is not timestamped. Without a timestamp, users may not be able to validate this jar after the signer certificate's expiration date (2027-01-07).

The signer certificate will expire on 2027-01-07.

```
  * Verify
```
jarsigner -verify -certs -verbose target\app.jar
```
expect to see in the output 
```
smk   1234 Mon Jan 01 ...
      X.509, CN=DemoAppSigner
      X.509, CN=DemoRootCA
```
actually see
```

s        138 Wed Jan 07 16:01:22 EST 2026 META-INF/MANIFEST.MF

      >>> Signer
      X.509, CN=DemoAppSigner, O=ExampleCorp, C=US
      [certificate is valid from 1/7/26, 3:59 PM to 1/7/27, 3:59 PM]
      X.509, CN=DemoRootCA, OU=Ops, O=ExampleCorp, L=NA, C=US
      [certificate is valid from 1/7/26, 3:57 PM to 1/5/36, 3:57 PM]
      [Invalid certificate chain: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target]

         300 Wed Jan 07 16:01:22 EST 2026 META-INF/APP-SIGN.SF
        2964 Wed Jan 07 16:01:22 EST 2026 META-INF/APP-SIGN.RSA
           0 Wed Jan 07 15:53:10 EST 2026 META-INF/
sm        14 Wed Jan 07 15:52:34 EST 2026 hello.txt

      >>> Signer
      X.509, CN=DemoAppSigner, O=ExampleCorp, C=US
      [certificate is valid from 1/7/26, 3:59 PM to 1/7/27, 3:59 PM]
      X.509, CN=DemoRootCA, OU=Ops, O=ExampleCorp, L=NA, C=US
      [certificate is valid from 1/7/26, 3:57 PM to 1/5/36, 3:57 PM]
      [Invalid certificate chain: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target]


  s = signature was verified
  m = entry is listed in manifest
  k = at least one certificate was found in keystore

- Signed by "CN=DemoAppSigner, O=ExampleCorp, C=US"
    Digest algorithm: SHA-256
    Signature algorithm: SHA256withRSA, 2048-bit key

jar verified.

Warning:
This jar contains entries whose certificate chain is invalid. Reason: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target
This jar contains signatures that do not include a timestamp. Without a timestamp, users may not be able to validate this jar after any of the signer certificates expire (as early as 2027-01-07).

The signer certificate will expire on 2027-01-07.


```
  * on Windows Git MinGW environment, need to set JAVA_HOME differently than in vanilla Powershell/ CMD and update PATH accodringly.
```sh
which  java
```
if the output is (and it often is)
```
/c/Program Files/Common Files/Oracle/Java/javapath/java
```
then, assuming JDK 11 is installed under 'C:\JAVA\' directory
```sh
export JAVA_HOME=/c/java/jdk-11.0.12/
export PATH=$PATH:$JAVA_HOME/bin
```
```
jarsigner -verify -certs -verbose target/app.jar

s        138 Wed Jan 07 16:01:22 EST 2026 META-INF/MANIFEST.MF

      >>> Signer
      X.509, CN=DemoAppSigner, O=ExampleCorp, C=US
      [certificate is valid from 1/7/26, 3:59 PM to 1/7/27, 3:59 PM]
      X.509, CN=DemoRootCA, OU=Ops, O=ExampleCorp, L=NA, C=US
      [certificate is valid from 1/7/26, 3:57 PM to 1/5/36, 3:57 PM]
      [Invalid certificate chain: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target]

         300 Wed Jan 07 16:01:22 EST 2026 META-INF/APP-SIGN.SF
        2964 Wed Jan 07 16:01:22 EST 2026 META-INF/APP-SIGN.RSA
           0 Wed Jan 07 15:53:10 EST 2026 META-INF/
sm        14 Wed Jan 07 15:52:34 EST 2026 hello.txt

      >>> Signer
      X.509, CN=DemoAppSigner, O=ExampleCorp, C=US
      [certificate is valid from 1/7/26, 3:59 PM to 1/7/27, 3:59 PM]
      X.509, CN=DemoRootCA, OU=Ops, O=ExampleCorp, L=NA, C=US
      [certificate is valid from 1/7/26, 3:57 PM to 1/5/36, 3:57 PM]
      [Invalid certificate chain: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target]


  s = signature was verified
  m = entry is listed in manifest
  k = at least one certificate was found in keystore

- Signed by "CN=DemoAppSigner, O=ExampleCorp, C=US"
    Digest algorithm: SHA-256
    Signature algorithm: SHA256withRSA, 2048-bit key

jar verified.

Warning:
This jar contains entries whose certificate chain is invalid. Reason: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target
This jar contains signatures that do not include a timestamp. Without a timestamp, users may not be able to validate this jar after any of the signer certificates expire (as early as 2027-01-07).

The signer certificate will expire on 2027-01-07.
```
The `keytool` and `jarsigner` are present in the typical JDK install

* package the Docker image
> NOTE the image is obsolete and will be flagged as such. It also is no longer download from Docker hub but may be present in the local Docker image cache
```sh
docker pull eclipse-temurin:11-jre-alpine
```
```sh
docker build -t example_image -f Dockerfile .
```

```sh
./verify.sh example_image 'app.jar' "CN=DemoAppSigner, O=ExampleCorp, C=US"
```
```
▶ Image: example_image
▶ JAR path: /app/app.jar
▶ Trusted CA: CN=ExpectedAppSigner, O=ExampleCorp, C=US

▶ Downloading Trivy...
aquasecurity/trivy info checking GitHub for latest tag
aquasecurity/trivy info found version: 0.68.2 for v0.68.2/windows/64bit
Archive:  trivy_0.68.2_windows-64bit.zip
  inflating: LICENSE
  inflating: README.md
  inflating: contrib/asff.tpl
  inflating: contrib/gitlab-codequality.tpl
  inflating: contrib/gitlab.tpl
  inflating: contrib/html.tpl
  inflating: contrib/junit.tpl
  inflating: trivy.exe
aquasecurity/trivy info installed /tmp/tmp.LVVRUIb39m/trivy.exe
▶ Running Trivy scan...

Report Summary

┌─────────────────────────┬────────┬─────────────────┬─────────┐
│         Target          │  Type  │ Vulnerabilities │ Secrets │
├─────────────────────────┼────────┼─────────────────┼─────────┤
│ example (alpine 3.22.2) │ alpine │       14        │    -    │
└─────────────────────────┴────────┴─────────────────┴─────────┘
Legend:
- '-': Not scanned
- '0': Clean (no security findings detected)


example (alpine 3.22.2)
=======================
Total: 14 (HIGH: 14, CRITICAL: 0)

┌──────────────────┬────────────────┬──────────┬────────┬───────────────────┬───────────────┬──────────────────────────────────────────────────────────────┐
│     Library      │ Vulnerability  │ Severity │ Status │ Installed Version │ Fixed Version │                            Title                             │
├──────────────────┼────────────────┼──────────┼────────┼───────────────────┼───────────────┼──────────────────────────────────────────────────────────────┤
│ gnupg            │ CVE-2025-68973 │ HIGH     │ fixed  │ 2.4.7-r0          │ 2.4.9-r0      │ GnuPG: GnuPG: Information disclosure and potential arbitrary │
│                  │                │          │        │                   │               │ code execution via out-of-bounds write...                    │
...
└──────────────────┴────────────────┴──────────┴────────┴───────────────────┴───────────────┴──────────────────────────────────────────────────────────────┘
✔ Trivy scan passed

▶ Creating container (no execution)...
▶ Extracting JAR from image...
▶ Verifying JAR signature...
✔ JAR signature valid and trusted

✅ IMAGE PASSED ALL CHECKS

```

### Problematic Jar Detection

the script discovers when the JAR is not signed
to verify, rebuild the dummy jar and repackage the container without signing the jar.

```sh

./verify.sh example_image 'app.jar' "CN=DemoAppSigner, O=ExampleCorp, C=US"
```
```text
✖ JAR is NOT signed

```
or signed with an unexpected CA:
```sh
./verify.sh example_image 'app.jar' "CN=ExpectedAppSigner, O=ExampleCorp, C=US"
```
```text
▶ Image: example_image
▶ JAR path: app.jar
▶ Trusted CA: CN=ExpectedAppSigner, O=ExampleCorp, C=US
...
▶ Verifying JAR signature...
✖ JAR not signed by trusted authority CN=ExpectedAppSigner, O=ExampleCorp, C=US
```
### Advanced Usage (WIP)

  * Fingerprint pinning (SHA256:)
  * Timestamp authority (-tsa)
  * Multiple trusted signers
  * Enforce -strict verification
  * Cosign → container signature comparison

### See Also:

  * signing JAR Files [basic](https://docs.oracle.com/javase/tutorial/deployment/jar/signing.html)
  * [Certificate Types](https://www.cyber.mil/eca/certificate-types/)
  * AquaSec [resource center](https://www.aquasec.com/cloud-native-academy/) for *everything cloud native*
  * Aqua MicroScanner: Free Image Vulnerability Scanning Plugin for Jenkins [blog](https://www.aquasec.com/blog/aqua-microscanner-free-image-vulnerability-scanning-plug-in-for-jenkins)
  * Aqua MicroScanner Jenkins Plug-In for Scanning Docker Builds [video](https://www.youtube.com/watch?v=cuBvHAuXq1o)
  * [aqua Jenkins MicroScanner Plugin](https://github.com/jenkinsci/aqua-microscanner-plugin/blob/master/README.md)

  * Macedonian shooting [Wiki](https://en.wikipedia.org/wiki/Dual_wield)
---
### Misc

```sh
IMAGE=screenshots/capture-macedoniab-snooting-scene.png
DATA=$(base64 -w 0 $IMAGE)
echo "data:image/jpeg;base64,$DATA)" >> README.md
```

__Aqua MicroScanner__ was a small, free plugin/tool for scanning container images (e.g., from Jenkins) against vulnerability databases (CVE-style checks). It doesn’t support deep custom logic for artifact content such as enforcing that a JAR is signed by a specific CA.

In fact, __MicroScanner__ has been deprecated and __Aqua__ recommends switching t use __Trivy__ (__Aqua__ open source vulnerability scanner) instead

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
