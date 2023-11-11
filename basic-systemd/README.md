### Info

Example of Docker container with `systemd` as main Docker process. Based on example from [Getting Started with Podman](https://app.pluralsight.com/courses/af3a57c3-dd9f-4ca5-8407-f4ee506ae1c0/table-of-contents). NOTE: not working, unable to connect from Xenial host using ssh.


### Usage

* Focal appears to require key based auth, need to be careful to not break the regular user keys
```sh
mkdir .ssh
ssh-keygen -f $(pwd)/.ssh/id_rsa  -N ''
```

```sh
docker pull ubuntu:focal
IMAGE=basic-ubuntu-ssh
docker build -t $IMAGE -f Dockerfile.focal .
```
```sh
NAME=basic-ubuntu-ssh
docker run -d -p 2222:22 --name $NAME $IMAGE
```
* for testing, create `app` user and  put the keys to that user `.ssh` (in a separate console)
```sh
sudo adduser app
sudo mkdir ~app/.ssh
sudo cp -R $(pwd)/.ssh/* ~app/.ssh
sudo chown -R app:app ~app/.ssh
sudo chmod 700 ~app/.ssh/id_rsa
sudo chmod 700 ~app/.ssh
```
```sh
sudo su - app
```
the as `app` user:
```sh
ssh -vvv -p 2222 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null app@localhost
```
```text
OpenSSH_7.6p1 Ubuntu-4ubuntu0.7, OpenSSL 1.0.2n  7 Dec 2017
debug1: Reading configuration data /etc/ssh/ssh_config
debug1: /etc/ssh/ssh_config line 19: Applying options for *
debug2: resolving "localhost" port 2222
debug2: ssh_connect_direct: needpriv 0
debug1: Connecting to localhost [127.0.0.1] port 2222.
debug1: Connection established.
debug1: identity file /home/app/.ssh/id_rsa type 0
debug1: key_load_public: No such file or directory
debug1: identity file /home/app/.ssh/id_rsa-cert type -1
debug1: key_load_public: No such file or directory
debug1: identity file /home/app/.ssh/id_dsa type -1
debug1: key_load_public: No such file or directory
debug1: identity file /home/app/.ssh/id_dsa-cert type -1
debug1: key_load_public: No such file or directory
debug1: identity file /home/app/.ssh/id_ecdsa type -1
debug1: key_load_public: No such file or directory
debug1: identity file /home/app/.ssh/id_ecdsa-cert type -1
debug1: key_load_public: No such file or directory
debug1: identity file /home/app/.ssh/id_ed25519 type -1
debug1: key_load_public: No such file or directory
debug1: identity file /home/app/.ssh/id_ed25519-cert type -1
debug1: Local version string SSH-2.0-OpenSSH_7.6p1 Ubuntu-4ubuntu0.7
ssh_exchange_identification: Connection closed by remote host
```

when the developer host is also Focal, the logs becomes
```text
OpenSSH_8.9p1 Ubuntu-3ubuntu0.4, OpenSSL 3.0.2 15 Mar 2022
debug1: Reading configuration data /etc/ssh/ssh_config
debug1: /etc/ssh/ssh_config line 19: include /etc/ssh/ssh_config.d/*.conf matched no files
debug1: /etc/ssh/ssh_config line 21: Applying options for *
debug2: resolving "localhost" port 2222
debug3: resolve_host: lookup localhost:2222
debug3: ssh_connect_direct: entering
debug1: Connecting to localhost [127.0.0.1] port 2222.
debug3: set_sock_tos: set socket 3 IP_TOS 0x10
debug1: Connection established.
debug1: identity file /home/app/.ssh/id_rsa type 0
debug1: identity file /home/app/.ssh/id_rsa-cert type -1
debug1: identity file /home/app/.ssh/id_ecdsa type -1
debug1: identity file /home/app/.ssh/id_ecdsa-cert type -1
debug1: identity file /home/app/.ssh/id_ecdsa_sk type -1
debug1: identity file /home/app/.ssh/id_ecdsa_sk-cert type -1
debug1: identity file /home/app/.ssh/id_ed25519 type -1
debug1: identity file /home/app/.ssh/id_ed25519-cert type -1
debug1: identity file /home/app/.ssh/id_ed25519_sk type -1
debug1: identity file /home/app/.ssh/id_ed25519_sk-cert type -1
debug1: identity file /home/app/.ssh/id_xmss type -1
debug1: identity file /home/app/.ssh/id_xmss-cert type -1
debug1: identity file /home/app/.ssh/id_dsa type -1
debug1: identity file /home/app/.ssh/id_dsa-cert type -1
debug1: Local version string SSH-2.0-OpenSSH_8.9p1 Ubuntu-3ubuntu0.4
kex_exchange_identification: Connection closed by remote host
Connection closed by 127.0.0.1 port 2222
```
- still a failure

connecting from a different machine also fails, but at the network level:
```sh
ssh -vvv -p 2222 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null app@192.168.0.151
```
```text
OpenSSH_9.4p1, OpenSSL 3.1.2 1 Aug 2023
debug1: Reading configuration data /etc/ssh/ssh_config
debug2: resolve_canonicalize: hostname 192.168.0.151 is address
debug3: ssh_connect_direct: entering
debug1: Connecting to 192.168.0.151 [192.168.0.151] port 2222.
debug3: set_sock_tos: set socket 4 IP_TOS 0x48
debug1: connect to address 192.168.0.151 port 2222: Connection refused
ssh: connect to host 192.168.0.151 port 2222: Connection refused
```


### Notes

  * Example uses Ubuntu __20.04__ LTS Focal, the systemd startup issues observed on __18.04__ LTS  Xenial.
  * [alpine](https://en.wikipedia.org/wiki/Alpine_Linux) uses [openrc](https://en.wikipedia.org/wiki/OpenRC) instead of systemd

  * when developer host is on Xenial the container appears to run __ipv6__, instead of __ipv4__
```sh
sudo netstat -antp | grep 2222
```
```text
tcp6       0      0 :::2222                 :::*                    LISTEN      
5991/docker-proxy
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
