### Info

This directory contains a replica of [Docker Container Ansible project example](https://github.com/Ilhicas/ansible-docker-example) with intent to combine it with [testinfa]() and execute the latter over Docker CLI.

### Usage

```sh
sudo -H pip install docker-py --upgrade
```
```sh
docker pull python:2.7.17-alpine3.9
```
```sh
ansible-playbook -i inventory.yml playbook.yml
```

```su
sudo -H pip install testinfra --upgrade
```
### TODO

Add and run testinfra locally using Docker networking

### See Also

 * [Ansible Docker plugins](https://docs.ansible.com/ansible/2.5/scenario_guides/guide_docker.html)



 * [Using Testinfra with Ansible](https://opensource.com/article/19/5/using-testinfra-ansible-verify-server-state)
 * [Infrastructure testing with Testinfra](https://philpep.org/blog/infrastructure-testing-with-testinfra/)
 * [Testinfra framework to  write infrastructure integration tests in Python](https://github.com/philpep/testinfra) - can also install via pip3

  * [testinfra example](https://github.com/philpep/test-dr	iven-infrastructure-example), uses Vagrant together with Docker ansible and Testinfra
  * 2-container [configuration](https://github.com/tuxpiper/ansible-testinfra-docker) to run testinfra Ansible playbook testing
  * https://developer.ibm.com/tutorials/cl-provision-docker-containers-ansible/
  * [feeding Docker containers with TestInfra](https://medium.com/@colinwren/configuration-testing-your-docker-containers-with-testinfra-58e79ae85be0)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
