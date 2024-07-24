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
ANSIBLE_LIBRARY=./library ansible -m diff -a "source=hello target=hola source_type=string target_type=string" localhost
```
pass arguments file
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
python library/diff.py /tmp/args.json
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
### See Also

  * [ansible modules](https://docs.ansible.com/ansible/2.9/modules/modules_by_category.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
