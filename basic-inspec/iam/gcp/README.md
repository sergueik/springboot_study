
### info


This example shows the implementation of an InSpec profile.

#### Install


on Linux install inspec via `https://omnitruck.chef.io/install.sh` (command from [option 1 (Package installer)](https://mitre-inspec-developer.netlify.app/installation/LinuxInstall.html#option-1-package-installer):
```sh
curl https://omnitruck.chef.io/install.sh | sudo bash -s -- -P inspec
```

this will create `/opt/inspec/bin/inspec`
which  will be useful.
internally uses
`/opt/inspec/embedded/lib/ruby/gems/3.1.0/gems/inspec-bin-6.8.1/lib/inspec-bin`


The command
`sudo gem install inspec`  nor `em install inspec --user-instal` does not create the binary

`inspec-bin` needs to be also installed. this is coverd in [https://github.com/inspec/inspec/issues/4138(https://github.com/inspec/inspec/issues/4138) 
```
set CHEF_LICENSE_KEY=free-f...
export GOOGLE_APPLICATION_CREDENTIALS="/path/to/service-account.json"
SET GOOGLE_APPLICATION_CREDENTIALS="/path/to/service-account.json"
```
```
inspec exec . -t gcp:// --attrs attributes.yml
```
```
inspec exec . -t gcp:// --input-file attributes.yml
```

```
inspec exec . -t gcp:// --log-level debug
```		

```
The remote source https://github.com/inspec/inspec-gcp/archive/master.tar.gz no longer has the requested content:

Request Content Hash: b7cdc10d372868e154f8831c73dfd437db89c2cc155b14c3e3e878e64a5a84f5
Actual Content Hash: 32e8d3e8506a9848571ac9548f1d7f59288d1616a4940ffc631a2ad557d34af2

For URL, supermarket, compliance, and other sources that do not
provide versioned artifacts, this likely means that the remote source
has changed since your lockfile was generated.

```


#### Reset Cloud Shell
you can reset your Google Cloud Shell instance to get a clean environment. Here’s how you can do that:

Open Google Cloud Shell: Go to the Google Cloud Console and click on the Cloud Shell icon in the top right corner.

Reset Cloud Shell:

Click on the three vertical dots (⋮) in the top-right corner of the Cloud Shell window.
Select "Reset Cloud Shell" from the dropdown menu.
Confirm the Reset: A prompt will ask you to confirm the reset. This will remove all files and settings in your current Cloud Shell session, restoring it to its initial state.

Important Notes:
Data Loss: Resetting Cloud Shell will delete all files and directories in your home directory (~). Make sure to back up any important data before proceeding.
Persistent Disk: If you have any persistent disks or other resources associated with your Google Cloud project, those will not be affected by the reset.

#### notes on dependencies

gem uninstall thor -v 1.3.1
ERROR:  While executing gem ... (Gem::InstallError)
    thor is not installed in GEM_HOME, try:
        gem uninstall -i /usr/local/lib/ruby/gems/3.2.0 thor
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/uninstaller.rb:120:in `uninstall'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/commands/uninstall_command.rb:202:in `uninstall'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/commands/uninstall_command.rb:188:in `uninstall_gem'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/commands/uninstall_command.rb:183:in `block in uninstall_specific'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/commands/uninstall_command.rb:175:in `each'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/commands/uninstall_command.rb:175:in `uninstall_specific'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/commands/uninstall_command.rb:136:in `execute'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/command.rb:326:in `invoke_with_build_args'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/command_manager.rb:253:in `invoke_command'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/command_manager.rb:194:in `process_args'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/command_manager.rb:152:in `run'
        /usr/local/lib/ruby/site_ruby/3.2.0/rubygems/gem_runner.rb:56:in `run'
        /usr/local/bin/gem:12:in `<main>'
kouzmine_serguei@cloudshell:~/kitchen-terraform-project (spheric-alcove-430818-f9)$ sudo  gem uninstall -i /usr/local/lib/ruby/gems/3.2.0 thor
WARN: Unresolved or ambiguous specs during Gem::Specification.reset:
      psych (>= 4.0.0)
      Available/installed versions of this gem:
      - 5.1.2
      - 5.0.1
WARN: Clearing out unresolved specs. Try 'gem cleanup <gem>'
Please report a bug if this causes problems.

Select gem to uninstall:
 1. thor-1.2.2
 2. thor-1.3.1
 3. All versions
> 2



kitchen converg
-----> Starting Test Kitchen (v3.7.0)
>>>>>> ------Exception-------
>>>>>> Class: Kitchen::ClientError
>>>>>> Message: Could not load the 'inspec' verifier from the load path. Did you mean: busser, dummy, shell, terraform ? Please ensure that your verifier is installed as a gem or included in your Gemfile if using Bundler.
>>>>>> ----------------------
>>>>>> Please see .kitchen/logs/kitchen.log for more details
>>>>>> Also try running `kitchen diagnose --all` for configuration



gem install kitchen-inspec


gem list kitchen-google


```

  ×  google-storage-bucket: Check Google Cloud Storage Bucket
     ×  Control Source Code Error default/test.rb:14 
     undefined method `google_storage_bucket' for #<Inspec::Rule:0x00007ea0f1df4ec0 @impact=1.0, @title="Check Google Cloud Storage Bucket", @descriptions={:default=>"Ensure the Google Cloud Storage bucket exists and has the expected properties."}, @refs=[], @tags={}, @resource_dsl=#<Module:0x00007ea0f2ff5160>, @__code=nil, @__block=#<Proc:0x00007ea0f3f03b98 default/test.rb:14>, @__source_location={:ref=>"default/test.rb", :line=>14}, @__rule_id="google-storage-bucket", @__profile_id="tests from default.", @__checks=[["describe", ["Control Source Code Error"], #<Proc:0x00007ea0f3f84fb8 /home/kouzmine_serguei/.gems/gems/inspec-core-6.8.1/lib/inspec/rule.rb:454>]], @__skip_rule={}, @__merge_count=0, @__merge_changes=[], @__skip_only_if_eval=false, @__na_rule={}, @__waiver_data=nil, @__file="default/test.rb", @__group_title=nil>

```