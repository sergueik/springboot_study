### Info
This direcrory contains a replica of [Docker Container Ansnible project example](https://github.com/Ilhicas/ansible-docker-example) with intent to combine it with [testinfa]() and execute the latter over Docker CLI.
### Usage
```sh
sudo -H pip install docker-py --upgrade
docker pull python:2.7.17-alpine3.9
```
### See Also

 * [Ansible Docker plugins](https://docs.ansible.com/ansible/2.5/scenario_guides/guide_docker.html)



 * [Using Testinfra with Ansible](https://opensource.com/article/19/5/using-testinfra-ansible-verify-server-state)
 * [Infrastructure testing with Testinfra](https://philpep.org/blog/infrastructure-testing-with-testinfra/)
 * [Testinfra framework to  write infrastructure integration tests in Python](https://github.com/philpep/testinfra) - can also install via pip3

  * [testinfra example](https://github.com/philpep/test-driven-infrastructure-example), uses Vagrant together with Docker ansible and Testinfra
  * Two container [configuration](https://github.com/tuxpiper/ansible-testinfra-docker) to run testinfra
  * https://developer.ibm.com/tutorials/cl-provision-docker-containers-ansible/

