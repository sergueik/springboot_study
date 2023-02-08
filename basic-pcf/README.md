### Info
This directory contains the media for local PCF development
the media downloads directory https://network.pivotal.io/products/pcfdev
The download contains a 35Gb VM image and a 7Gb iso image `cfdev-efi-v2.iso`
```sh
tar xzvf ../pcfdev-v1.3.1-windows.tgz
```
```text
./
./bin/
./state/
./services/
./services/deploy-rabbitmq
./services/cf.yml
./services/deploy-apps-manager.ps1
./services/rabbitmq.yml
./services/deploy-cf
./services/deploy-redis.ps1
./services/deploy-redis
./services/mysql.yml
./services/redis.yml
./services/deploy-mysql.ps1
./services/deploy-cf.ps1
./services/deploy-scs
./services/deploy-scs.ps1
./services/spring-cloud-services.yml
./services/deploy-apps-manager
./services/deploy-mysql
./services/deploy-rabbitmq.ps1
./state/id_rsa
./state/bosh/
./state/metadata.yml
./state/vpnkit/
tar: Ignoring unknown extended header keyword 'LIBARCHIVE.creationtime'
tar: Ignoring unknown extended header keyword 'SCHILY.dev'
tar: Ignoring unknown extended header keyword 'SCHILY.ino'
tar: Ignoring unknown extended header keyword 'SCHILY.nlink'
./state/disk.vhdx
./state/bosh/director.yml
./state/bosh/cloud-config.yml
./state/bosh/state-temp.json
./state/bosh/state.json
./state/bosh/ca.crt
./state/bosh/ops-manager-dns-runtime.yml
./state/bosh/env.yml
./state/bosh/jumpbox.key
./bin/cfdev-efi-v2.iso
./bin/bosh.exe
./bin/vpnkit.exe
./bin/winsw.exe


```
PCF Dev runs on top of the CF Dev CF CLI plugin, but the latter is problematic to find: The `https://github.com/cloudfoundry-incubator/cfdev` gets redirected to `https://github.com/cloudfoundry-attic/cfdev`, which release directory links are on inreachable host: `https://d3p1cc0zb2wjno.cloudfront.net/cfdev/cfdev-v0.0.16-rc.5-windows.exe`

```text
Could not resolve host: d3p1cc0zb2wjno.cloudfront.net
```
The commandline client can be downloaded grom `https://packages.cloudfoundry.org/stable?release=windows64&version=8.5.0&source=github-rel`

one still needs to find the location to download the CF DEV plugin

### See Also
   * [installing on Windows 7](https://docs.pivotal.io/pcf-dev/install-windows-legacy.html) without HyperV
   * [installing on Windows 10](https://docs.pivotal.io/pcf-dev/install-windows.html) with HyperV
  * [plugin directory](https://plugins.cloudfoundry.org)
  * [stackoverflow](https://stackoverflow.com/questions/70989962/how-to-get-a-local-cloud-foundry-instance)
  * CF CLI Documentation in [github repository](https://github.com/cloudfoundry/cli)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
