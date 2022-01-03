### Info

This is a replica of the source of 3 node k8 cluster on [bento/ubuntu-18.05](https://app.vagrantup.com/bento/boxes/ubuntu-18.04) base box 
described in [blog](https://devopscube.com/kubernetes-cluster-vagrant/). witched to [Ubuntu bionic 64 base box](https://app.vagrantup.com/ubuntu/boxes/bionic64) to save some host disk space

### Usage
```sh
vagrant up
```
```sh
vagrant ssh master

```
```sh
kubectl get node
``
```sh
kubectl get pod --namespace kube-system
```
```sh
kubectl apply -f /vagrant/deployment.nginx.yaml
```


```sh
kubectl expose deployment nginx-app --type=NodePort --port=80
```
```sh
kubectl get service
```
this will show
```text
NAME         TYPE        CLUSTER-IP       EXTERNAL-IP   PORT(S)        AGE
kubernetes   ClusterIP   10.96.0.1        <none>        443/TCP        125m
nginx-app    NodePort    10.105.189.180   <none>        80:30879/TCP   34s
```

test connection
```sh
curl localhost:30879
```
this will print HTML page with
```text
If you see this page, the nginx web server is successfully installed and
working. Further configuration is required.
```

### Cleanup
```sh
kubectl delete -f /vagrant/deployment.nginx.yaml
kubectl delete service nginx-app
```
exit vagrant master and halt the cluster
```sh
vagrant halt
```
### Notes

Host disk usage:
```
4.2G    ~/VirtualBox VMs/basic-k8-vagrant_master_1641230975031_61735/
2.3G    ~/VirtualBox VMs/basic-k8-vagrant_node01_1641231679619_37949/

```
### See Also
   * https://habr.com/ru/post/599039/ (Russian translation)
