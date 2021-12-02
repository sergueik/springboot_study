### Install

* follow the [steps](https://phoenixnap.com/kb/install-minikube-on-ubuntu) listed gfor bionic

```sh
wget https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64
```

```sh
sudo cp minikube-linux-amd64 /usr/local/bin/minikube
sudo chmod 755 /usr/local/bin/minikube
```
```sh
curl -LO https://storage.googleapis.com/kubernetes-release/release/`curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt`/bin/linux/amd64/kubectl
```

```sh
sudo mv ./kubectl /usr/local/bin/kubectl
sudo chmod 755 /usr/local/bin/kubectl
```
* start the VM
```sh
minikube start --driver=virtualbox
```
of (on Linux host)
```sh
minikube start --driver=docker
```
NOTE: a lot quicker on Windows / SSD than on Linux/CF
NOTE: to use `--driver=none` one appears to have to install minikube as root

*  smoke test 
```sh
kubectl create deployment nginx --image=nginx:alpine
kubectl expose deployment nginx --type=NodePort --port=80
curl $(minikube service nginx --url)
kubectl delete service nginx
kubectl delete deployment nginx
```
```
<h1>Welcome to nginx!</h1>
```

* test basic deployment
```
kubectl apply -f deployment.nginx.yaml 
```
```text
deployment.apps/nginx-app created
```

```sh
kubectl get pod | grep nginx-app
```
```text
NAME                         READY   STATUS    RESTARTS   AGE
nginx-app-64d5fbc4b6-lc84r   1/1     Running   0          5s
```
(the suffix will vary)
```sh
kubectl expose deployment nginx-app --type=NodePort --port=80
```
```sh
curl $(minikube service nginx-app --url)
```
```html
<h1>Welcome to nginx!</h1>
```
```sh
kubectl delete service nginx-app
kubectl delete deployment nginx-app
```

### Deploying Locally Built Images

* this is work in progrss. current status is `ErrImagePull`/`ImagePullBackOff`:
```sh
cd ../basic-perl-cgi
NAME=basic-perl-cgi-container
docker build -t $NAME -f Dockerfile .
docker image ls | grep $NAME
kubectl apply -f deployment.perl-cgi.yaml 
kubectl get pod
```
```txt
NAME                        READY   STATUS             RESTARTS   AGE
perl-app-79f77c8665-45zjs   0/1     ImagePullBackOff   0          3m58s
```
```sh
kubectl delete deployment perl-app
```
### See Also

 * https://github.com/DanWahlin/DockerAndKubernetesCourseCode/tree/main/samples
 * https://v1-18.docs.kubernetes.io/docs/setup/learning-environment/minikube/
