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


### See Also

  * [ansible modules](https://docs.ansible.com/ansible/2.9/modules/modules_by_category.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
