### Info

this directory contains steps to arrange a quick sane Docker environment with `Ansible` inventory for testing

### Usage
* step 1 - launch container based on image which has python
```sh
IMAGE=python:3.8.2-alpine
NAME=dummy
docker run --name $NAME -d $IMAGE sh -c 'while true; do sleep 1 ; done'
```
* step 2 - generate dummy inventory. Note to be in `docker` directory

```sh
../generate_docker_inventory.sh $NAME | tee docker-inventory
```
note the `ansible.cfg` that points to the `docker-inventory` is aready present
* step 3 confirm to run under custom config

```sh
ansible-config dump  --o
```
```text
DEFAULT_HOST_LIST(/home/sergueik/src/puppetmaster_vagrant/ansible/docker/ansible
DEFAULT_TRANSPORT(/home/sergueik/src/puppetmaster_vagrant/ansible/docker/ansible
```
* step 4 run ansible modules on dummy
```sh
ansible all -m ping
```
```text
172.17.0.2 | SUCCESS => {
    "ansible_facts": {
        "discovered_interpreter_python": "/usr/local/bin/python"
    },
    "changed": false,
    "ping": "pong"
}

```
```sh
ansible all -m command -a 'hostname'
```
```text
172.17.0.2 | CHANGED | rc=0 >>
dedea1f250fd
```

```sh
ansible all -m setup  -m debug -a msg="{{vars['ansible_host']}}"
```
```text
172.17.0.2 | SUCCESS => {
    "msg": "dummy"
}

```
NOTE: if no Python is installed in the container, can still use Ansible `raw` module
```sh
ansible all -m raw -a 'ip addr li eth0'
```
```text
```
* step 5 run some playbooks on `dummy` host
```sh
ansible-playbook ../roles/docker_role_test.yaml
```
```text
fatal: [172.17.0.2]: FAILED! => {
    "assertion": "ansible_memtotal_mb >= (expected_mem_mb | int)",
    "changed": false,
    "evaluated_to": false,
    "msg": "memory is low: found 3814 need 10240"
}

```
NOTE: the successul run lists the node `ansible_facts` vars:
```sh
ansible-playbook -e "expected_mem_gb=1" ../roles/docker_role_test.yaml
```
```text
ok: [172.17.0.2]

TASK [spec : memory check] *****************************************************
ok: [172.17.0.2] => {
    "changed": false,
    "msg": "All assertions passed"
}

TASK [spec : facts check] ******************************************************
ok: [172.17.0.2] => {
    "msg": {
        "all_ipv4_addresses": [
            "172.17.0.2"
        ],

...
```
* cleanup

```sh
docker stop $NAME
docker container rm $NAME
```
if there is no other containers running can simply

```sh
docker container rm -f $(docker container ps -qa)
```
```sh
docker image prune -f
```
### Note

Alternatively one can download and install `docker.py` and use it as a dynamic inventory. This is documented [here](https://docs.ansible.com/ansible/latest/collections/community/docker/docker_containers_inventory.html)

*  download from github

```sh
curl -o docker-inventory.py https://raw.githubusercontent.com/ryankwilliams/docker-inventory/master/src/docker_inventory.py
```
* make executable
```sh
chmod +x docker-inventory.py
```

Alternatively
install [docker-inventory]https://github.com/ryankwilliams/docker-inventory() script, upgrade ansible and docker per its `requirements.txt`
```
sudo -H pip install -r requirements.txt --upgrade
```
this is failing on Ubunty Bionic  because the repository installed ansible is somewhat archaic, and runs on Python 2.7 while installing the `requirements.txt` leads to error:
```text

Collecting ansible>=2.1.0.0 (from -r requirements.txt (line 1))
  Using cached https://files.pythonhosted.org/packages/fd/f8/071905c6a67592d0852a9f340f6ab9226861eeeb97fdf4068642b22edcf3/ansible-4.10.0.tar.gz
    Complete output from command python setup.py egg_info:


  ### ERROR ###

  Upgrading directly from ansible-2.9 or less to ansible-2.10 or greater with pip is known to cause problems.  Please uninstall the old version found at:

  /usr/lib/python2.7/dist-packages/ansible/__init__.pyc

  and install the new version:

      pip uninstall ansible
      pip install ansible

  If you have a broken installation, perhaps because ansible-core was installed before ansible was upgraded, try this to resolve it:

      pip install --force-reinstall ansible ansible-core

  If ansible is installed in a different location than you will be installing it now (for example, if the old version is installed by a system package manager to /usr/lib/python3.8/site-packages/ansible but you are installing the new version into ~/.local/lib/python3.8/site-packages/ansible with `pip install --user ansible`) or you want to install anyways and cleanup any breakage afterwards, then you may set the ANSIBLE_SKIP_CONFLICT_CHECK environment variable to ignore this check:

      ANSIBLE_SKIP_CONFLICT_CHECK=1 pip install --user ansible

  ### END ERROR ###



```

To install recent version of __Ansible__ on  legacy __Ubuntu__ __18.04__ follow [this](https://www.cyberciti.biz/faq/how-to-install-and-configure-latest-version-of-ansible-on-ubuntu-linux/)
```sh
apt -y install software-properties-common
apt-add-repository ppa:ansible/ansible
apt install ansible
```
```sh
ansible --version
```
```text
ansible 2.9.27
```

NOTE: Ansible __2.9.27__ still uses Python __2.7.17__, but since everything else works, so will be


Then to enable `docker.py`, `fixed_docker.py` install `docker-py` in Python 3 globally:
```sh
sudo -H pip3 install docker-py --upgrade
```
*  verify  the `fixed_docker.py` works:

```sh
./fixed_docker.py  | jq '.'
```
```json
{
  "myhosts": [
    "dummy"
  ],
  "running": [
    "dummy"
  ]
}
```
* update `ansible.cfg` 

```text
-inventory=docker-inventory
+# do dynamic inventory
+inventory=fixed-docker.py
```
* verify the operation

```
NAME=dummy
for CNT in 1 2 3 ; do
docker ansible 'all:!stopped' -m pingrun --name "${NAME}_${CNT}" -d $IMAGE sh -c 'while true; do sleep 1 ; done'; done
```
```sh
ansible 'all:!stopped' -m ping 2>/dev/null
```
```text
dummy_2 | SUCCESS => {
    "ansible_facts": {
        "discovered_interpreter_python": "/usr/local/bin/python"
    },
    "changed": false,
    "ping": "pong"
}
dummy_1 | SUCCESS => {
    "ansible_facts": {
        "discovered_interpreter_python": "/usr/local/bin/python"
    },
    "changed": false,
    "ping": "pong"
}
dummy_3 | SUCCESS => {
    "ansible_facts": {
        "discovered_interpreter_python": "/usr/local/bin/python"
    },
    "changed": false,
    "ping": "pong"
}

```
### See Also

  * [ansible modules](https://docs.ansible.com/ansible/2.9/modules/modules_by_category.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
