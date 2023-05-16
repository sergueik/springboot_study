### Info

Pure Perl module with dependencies installed on top of Alpine image
and with modified and trimmed Crypt::PBE installed into workdir for debugging

### Usage

#### Build

```sh
IMAGE=basic-perl-crypt-jasypt
docker build -t $IMAGE -f Dockerfile .
```
if the build ends with error

```text
Step 5/5 : COPY test.pl /root/
COPY failed: file not found in build context or excluded by .dockerignore: stat test.pl: file does not exist
```

copy the `jasypt.pl` locally with the name `test.pl`
if seeing the error with `Crypt::PBE`, comment the line, where the cpan commandis invoked, rebuild the image and install interatively as explained below in the __TODO__ section

* NOTE: build will be time comsuming

#### Verify Application

```sh
NAME=example-perl-jasypt
docker container rm $NAME
docker run --name $NAME -it $IMAGE sh
```
* run test in the container
```sh
VALUE=$(perl test.pl -value message -secret apple -operation encrypt)
echo $VALUE
```
this will print some base64 encoded binary string, different each time
```text
x5p9WNNzxLAqGwt7zDkx1A==
```
```sh
perl test.pl -value "$VALUE" -password apple
```
this will successfully decrypt it
```text
message
```
### Debugging

```sh
perl test.pl -password secret -value test
```
```text
/SsSdC91HQuA0NRml8JD1g==
```

```sh
perl test.pl -password secret -value test -debug
```
```text

password: secret
value: test
Encrypting
Salt (random): fcf78385db4f64b3
$VAR1 = [
          253,
          43,
          18,
          116,
          47,
          117,
          29,
          11
        ];
Salt (fixed): fd2b12742f751d0b
DK: 29395b04f92c9d2a57e85c0eb06566bb
Salt: fd2b12742f751d0b
Encrypted: 80d0d46697c24
/SsSdC91HQuA0NRml8JD1g==
Decrypting
Decrypting
Salt: fd2b12742f751d0b
Encrypted: 80d0d46697c243d6
test

```
```sh
perl test.pl -password secret -value '/SsSdC91HQuA0NRml8JD1g==' -debug -operation decrypt
```
```text
password: secret
value: /SsSdC91HQuA0NRml8JD1g==
Decrypting
/SsSdC91HQuA0NRml8JD1g==
Salt: fd2b12742f751d0b
Encrypted: 80d0d46697c243d6
test
```
### TODO

the same build fails on a clean machine with:
```text
t/60-CLI.t           (Wstat: 256 Tests: 0 Failed: 0)
#7 24.60   Non-zero exit status: 1
#7 24.60   Parse errors: No plan found in TAP output
#7 24.60 Files=10, Tests=87,  4 wallclock secs ( 0.14 usr  0.02 sys +  2.72 cusr  0.22 csys =  3.10 CPU)
#7 24.60 Result: FAIL
#7 24.61 Failed 1/10 test programs. 0/87 subtests failed.
#7 24.61 make: *** [Makefile:916: test_dynamic] Error 1
#7 24.61   GDT/Crypt-PBE-0.102.tar.gz
#7 24.61   /usr/bin/make test -- NOT OK
#7 24.61 //hint// to see the cpan-testers results for installing this module, try:
#7 24.61   reports GDT/Crypt-PBE-0.102.tar.gz
------
process "/bin/sh -c cpan -i 'Crypt::PBE'" did not complete successfully: exit code: 1

```
to recover try
```sh
 docker system prune -f
```
this will report
```text
Deleted build cache objects:
rbqirr85u8sbh77cj7iidq6ti
zmk5fst4bpwiofuvzdprw4wwv
5b635q2yc3zs6gshhdjln82em
68b2irmjp0j5qvvg2ts5gff41
9o4rpfp1bahn6w4vjr5jrerdt
2bhuk4y123czi4fmcfykkj1co
ww0nfj0f7a94s7osdsz7sz022

Total reclaimed space: 258.6MB

```
* comment the
```sh
RUN cpan -i 'Crypt::PBE'
```
line

* install interactively
```sh
NAME=example-perl-jasypt
docker run --name $NAME -it $IMAGE sh
```
```sh
cpan -i 'Crypt::PBE'
```

apparently this succeeds:
```text
Running make test
PERL_DL_NONLAZY=1 "/usr/bin/perl" "-MExtUtils::Command::MM" "-MTest::Harness" "-e" "undef *Test::Harness::Switches; test_harness(0, 'blib/lib', 'blib/arch')" t/*.t
t/00-load.t ............ 1/? # Crypt::PBE 0.102, Perl 5.026003, /usr/bin/perl
t/00-load.t ............ ok
t/10-PBKDF1.t .......... ok
t/11-PBKDF1-alias.t .... ok
t/20-PBKDF2.t .......... ok
t/21-PBKDF2-RFC6070.t .. ok
t/22-PBKDF2-alias.t .... ok
t/30-PBES1.t ........... ok
t/40-PBES2.t ........... ok
t/50-PBE.t ............. ok
t/60-CLI.t ............. ok
All tests successful.
Files=10, Tests=98,  2 wallclock secs ( 0.10 usr  0.02 sys +  1.86 cusr  0.15 csys =  2.13 CPU)
Result: PASS
  GDT/Crypt-PBE-0.102.tar.gz
  /usr/bin/make test -- OK
Running make install
Manifying 1 pod document
Manifying 6 pod documents
Installing /usr/local/share/perl5/site_perl/Crypt/PBE.pm
Installing /usr/local/share/perl5/site_perl/Crypt/PBE/CLI.pm
Installing /usr/local/share/perl5/site_perl/Crypt/PBE/PBES1.pm
Installing /usr/local/share/perl5/site_perl/Crypt/PBE/PBKDF2.pm
Installing /usr/local/share/perl5/site_perl/Crypt/PBE/PBKDF1.pm
Installing /usr/local/share/perl5/site_perl/Crypt/PBE/PBES2.pm
Installing /usr/local/share/man/man1/pkcs5-tool.1
Installing /usr/local/share/man/man3/Crypt::PBE.3pm
Installing /usr/local/share/man/man3/Crypt::PBE::PBES1.3pm
Installing /usr/local/share/man/man3/Crypt::PBE::PBKDF1.3pm
Installing /usr/local/share/man/man3/Crypt::PBE::PBKDF2.3pm
Installing /usr/local/share/man/man3/Crypt::PBE::PBES2.3pm
Installing /usr/local/share/man/man3/Crypt::PBE::CLI.3pm
Installing /usr/local/bin/pkcs5-tool
Appending installation info to /usr/lib/perl5/core_perl/perllocal.pod
  GDT/Crypt-PBE-0.102.tar.gz
  /usr/bin/make install  -- OK

```
### Debugging the `PBEWithHmacSHA512AndAES_256`

* modify with debug messages and include `Crypt/PBE/PBES2.pm`, `Crypt/PBE/PBKDF2.pm`
and modify the `Crypt/PBE.pm` to recognize `PBEWithHmacSHA512AndAES_256`

* run
```sh
perl test.pl  -operation encrypt -password secret -value data -debug 2>&1 |t
ee a.log

```
```text
PBEWithMD5AndDES
PBEWithHmacSHA512AndAES_256
Salt (random): e7a0f23ff31287202a24e4714109bd75
$VAR1 = [
          4,
          50,
          207,
          58,
          65,
          5,
          42,
          101,
          146,
          144,
          12,
          46,
          108,
          11,
          92,
          121
        ];
Salt (fixed): 0432cf3a41052a6592900c2e6c0b5c79
key: fedbb6eb4b3128bef07fc97215342452e1327b24e7b1fa4288506544f4d1f4bd
Salt: 0432cf3a41052a6592900c2e6c0b5c79
Iv: 646ebc65e7176836b07bac13f4192b41
Encrypted: c93fc19c6f235990d9503cee5a96a58a
Salt: 0432cf3a41052a6592900c2e6c0b5c79
Iv: 646ebc65e7176836b07bac13f4192b41
Encrypted: c93fc19c6f235990d9503cee5a96a58a
key: fedbb6eb4b3128bef07fc97215342452e1327b24e7b1fa4288506544f4d1f4bd
$VAR1 = \{
            'PBEWithMD5AndDES' => {
                                    'encryption' => 'des',
                                    'hash' => 'md5'
                                  }
          };
$VAR1 = \{
            'PBEWithHmacSHA512AndAES_256' => {
                                               'hmac' => 'hmac-sha512',
                                               'encryption' => 'aes-256'
                                             }
          };
password: secret
value: data
Encrypting
BDLPOkEFKmWSkAwubAtceWRuvGXnF2g2sHusE/QZK0HJP8GcbyNZkNlQPO5alqWK

Decrypting
data

```
### See Also

  * `Crypt::PBE` [module](https://metacpan.org/pod/Crypt::PBE)
  * [discussion](https://www.perlmonks.org/?dislaytype=print;node_id=845861;replies=1) about implementing Jasypt in Perl
  * [packing](https://perldoc.perl.org/perlpacktut) - ranked overly complicated
  * [stackoverflow packing example](https://stackoverflow.com/questions/2427527/how-can-i-convert-a-48-hex-string-to-bytes-using-perl)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
