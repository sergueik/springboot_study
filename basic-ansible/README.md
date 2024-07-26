### Info
This directory contains Ansible examples from the training.

### Runnning Playbooks on localhost with no ssh or sudo

* include in the `yaml`
```yaml
- hosts: all
  connection: local
```
or through command line option
```sh
ansible-playbook --ask-become-pass -i inventory --connection local when_fact.yaml --tags jq
```

### Runnning Playbooks on Docker instance

The `Docker` directory contains steps to arrange a quick sane Docker environment with `Ansible` inventory for testing


### Testing Modules


Using example module from [cytopia/ansible-modules](https://github.com/cytopia/ansible-modules/blob/master/modules/diff.py)


* testing locally
```sh
ANSIBLE_LIBRARY=./library ansible -vvv -m diff_module -a "source=hello target=hola source_type=string target_type=string" localhost
```
```text
ansible 2.9.27
  config file = /etc/ansible/ansible.cfg
  configured module search path = [u'/home/sergueik/src/springboot_study/basic-ansible/library']
  ansible python module location = /usr/lib/python2.7/dist-packages/ansible
  executable location = /usr/bin/ansible
  python version = 2.7.17 (default, Mar  8 2023, 18:40:28) [GCC 7.5.0]
Using /etc/ansible/ansible.cfg as config file
host_list declined parsing /etc/ansible/hosts as it did not pass its verify_file() method
script declined parsing /etc/ansible/hosts as it did not pass its verify_file() method
auto declined parsing /etc/ansible/hosts as it did not pass its verify_file() method
Parsed /etc/ansible/hosts inventory source with ini plugin
Skipping callback 'actionable', as we already have a stdout callback.
Skipping callback 'counter_enabled', as we already have a stdout callback.
Skipping callback 'debug', as we already have a stdout callback.
Skipping callback 'dense', as we already have a stdout callback.
Skipping callback 'dense', as we already have a stdout callback.
Skipping callback 'full_skip', as we already have a stdout callback.
Skipping callback 'json', as we already have a stdout callback.
Skipping callback 'minimal', as we already have a stdout callback.
Skipping callback 'null', as we already have a stdout callback.
Skipping callback 'oneline', as we already have a stdout callback.
Skipping callback 'selective', as we already have a stdout callback.
Skipping callback 'skippy', as we already have a stdout callback.
Skipping callback 'stderr', as we already have a stdout callback.
Skipping callback 'unixy', as we already have a stdout callback.
Skipping callback 'yaml', as we already have a stdout callback.
META: ran handlers
<127.0.0.1> ESTABLISH LOCAL CONNECTION FOR USER: sergueik
<127.0.0.1> EXEC /bin/sh -c 'echo ~sergueik && sleep 0'
<127.0.0.1> EXEC /bin/sh -c '( umask 77 && mkdir -p "` echo /home/sergueik/.ansible/tmp `"&& mkdir "` echo /home/sergueik/.ansible/tmp/ansible-tmp-1721843153.23-4468-162855540315784 `" && echo ansible-tmp-1721843153.23-4468-162855540315784="` echo /home/sergueik/.ansible/tmp/ansible-tmp-1721843153.23-4468-162855540315784 `" ) && sleep 0'
Using module file /home/sergueik/src/springboot_study/basic-ansible/library/diff_module.py
<127.0.0.1> PUT /home/sergueik/.ansible/tmp/ansible-local-4462giapAv/tmpT2ohcB TO /home/sergueik/.ansible/tmp/ansible-tmp-1721843153.23-4468-162855540315784/AnsiballZ_diff_module.py
<127.0.0.1> EXEC /bin/sh -c 'chmod u+x /home/sergueik/.ansible/tmp/ansible-tmp-1721843153.23-4468-162855540315784/ /home/sergueik/.ansible/tmp/ansible-tmp-1721843153.23-4468-162855540315784/AnsiballZ_diff_module.py && sleep 0'
<127.0.0.1> EXEC /bin/sh -c '/usr/bin/python2 /home/sergueik/.ansible/tmp/ansible-tmp-1721843153.23-4468-162855540315784/AnsiballZ_diff_module.py && sleep 0'
<127.0.0.1> EXEC /bin/sh -c 'rm -f -r /home/sergueik/.ansible/tmp/ansible-tmp-1721843153.23-4468-162855540315784/ > /dev/null 2>&1 && sleep 0'
localhost | CHANGED => {
    "changed": true,
    "diff": {
        "after": "hello",
        "before": "hola"
    },
    "invocation": {
        "module_args": {
            "source": "hello",
            "source_type": "string",
            "target": "hola",
            "target_type": "string"
        }
    }
}
META: ran handlers
META: ran handlers

```
pass arguments through JSON file
```
cat /tmp/args.json
```
```json
{
     "ANSIBLE_MODULE_ARGS": {
        "source": "hello",
        "target": "hola"
     }
}
```
 
```sh
python library/diff_module.py /tmp/args.json
```
```json
{
  "invocation": {
    "module_args": {
      "source": "hello",
      "target_type": "string",
      "target": "hola",
      "source_type": "string"
    }
  },
  "diff": {
    "after": "hello",
    "before": "hola"
  },
  "changed": true
}





```

* testing in playbook

```YAML
- hosts: localhost
  gather_facts: no
  become: false
  tasks:
    - name: test
      register: result
      diff_module:
        source: 'hello'
        target: 'goodbye'
    - debug: var=result  
```
```sh
ansible-playbook ./module_test.yaml
```
```text
[WARNING]: provided hosts list is empty, only localhost is available. Note that
the implicit localhost does not match 'all'



PLAY [localhost] ***************************************************************

TASK [test] ********************************************************************
changed: [localhost]

TASK [debug] *******************************************************************
ok: [localhost] => {
    "result": {
        "changed": true,
        "diff": {
            "after": "hello",
            "before": "goodbye"
        },
        "failed": false
    }
}

PLAY RECAP *********************************************************************
localhost                  : ok=2    changed=1    unreachable=0    failed=0    skipped=0    rescued=0    ignored=0


```
reading the well formed module documentation
```sh
ANSIBLE_LIBRARY=./library ansible-doc diff_module
```
### See Also

  * [ansible modules](https://docs.ansible.com/ansible/2.9/modules/modules_by_category.html)
  * https://spacelift.io/blog/ansible-modules  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
