
### info


This example shows the implementation of an InSpec profile.

See also https://github.com/inspec/inspec-gcp/issues/77
https://docs.chef.io/inspec/profiles/controls/

Add a Chef license key by using the CHEF_LICENSE_KEY environment variable.

```
set CHEF_LICENSE_KEY=free-f...
```

```
terraform apply -var "bucket_name=static_website_9384" 
```
```
inspec exec . -t gcp:// --attrs attributes.yml
```
```
inspec exec . -t gcp:// --input-file attributes.yml
```

remove the `inspec.lock` to avoid

```
The remote source https://github.com/inspec/inspec-gcp/archive/master.tar.gz no longer has the requested content:

Request Content Hash: b7cdc10d372868e154f8831c73dfd437db89c2cc155b14c3e3e878e64a5a84f5
Actual Content Hash: 32e8d3e8506a9848571ac9548f1d7f59288d1616a4940ffc631a2ad557d34af2

```