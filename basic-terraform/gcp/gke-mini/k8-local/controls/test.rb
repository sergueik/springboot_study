control 'k8s-deployment-validation' do
  # YAML definition for the deployment and service
  deployment_yaml = <<-YAML
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-app
spec:
  replicas: 2
  selector:
    matchLabels:
      app: my-app
  template:
    metadata:
      labels:
        app: my-app
    spec:
      containers:
        - name: my-app-container
          image: nginx:1.19
          ports:
            - containerPort: 80
  YAML

  service_yaml = <<-YAML
apiVersion: v1
kind: Service
metadata:
  name: my-service
spec:
  selector:
    app: my-app
  ports:
    - protocol: TCP
      port: 80
      targetPort: 80
      nodePort: 30007
  type: NodePort
  YAML

  # Deploy the Kubernetes resources (Deployment and Service)
  describe command("echo '#{deployment_yaml}' | kubectl apply -f -") do
    its('exit_status') { should eq 0 }
  end

  describe command("echo '#{service_yaml}' | kubectl apply -f -") do
    its('exit_status') { should eq 0 }
  end

  # Validate the Kubernetes Deployment (Check if it's running correctly)
  describe command('kubectl get deployment my-app -o json') do
    its('stdout') { should match /"replicas": 2/ }
  end

  # Validate the Kubernetes Service is of type NodePort and has NodePort configured
  describe command('kubectl get service my-service -o json') do
    its('stdout') { should match /"type": "NodePort"/ }
    its('stdout') { should match /"nodePort": 30007/ }
  end

  # Failing Control: Check if the Pod is running, expecting it to fail
  describe command('kubectl get pods -l app=my-app -o json') do
    its('stdout') { should_not match /Running/ } # This should fail intentionally
  end

  # Passing Control: Check if the Pod is running (for a successful case)
  describe command('kubectl get pods -l app=my-app -o json') do
    its('stdout') { should match /Running/ } # This should pass
  end

  # Validate the Node (Check for specific taint or label)
  describe command('kubectl describe node my-node') do
    its('stdout') { should match /NoSchedule/ } # Check if the node has a taint for scheduling
  end

  # Teardown the resources after the test to ensure they don't persist
  describe command("echo '#{deployment_yaml}' | kubectl delete -f -") do
    its('exit_status') { should eq 0 }
  end

  describe command("echo '#{service_yaml}' | kubectl delete -f -") do
    its('exit_status') { should eq 0 }
  end
end

