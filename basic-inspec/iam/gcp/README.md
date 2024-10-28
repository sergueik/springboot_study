
### info


This example shows the implementation of an InSpec profile.

```
set CHEF_LICENSE_KEY=free-f...
```
```
inspec exec . -t gcp:// --attrs attributes.yml
```
```
inspec exec . -t gcp:// --input-file attributes.yml
```



```
The remote source https://github.com/inspec/inspec-gcp/archive/master.tar.gz no longer has the requested content:

Request Content Hash: b7cdc10d372868e154f8831c73dfd437db89c2cc155b14c3e3e878e64a5a84f5
Actual Content Hash: 32e8d3e8506a9848571ac9548f1d7f59288d1616a4940ffc631a2ad557d34af2

For URL, supermarket, compliance, and other sources that do not
provide versioned artifacts, this likely means that the remote source
has changed since your lockfile was generated.

```