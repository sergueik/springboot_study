control 'k8s-cluster-setup' do
  impact 1.0
  title 'Ensure Kubernetes Cluster Setup is Correct'
  desc 'Check service account authentication, RBAC permissions, and cluster readiness'

  # Load attributes from the attributes.yml
  input_project_id = attribute('project_id')
  input_cluster_name = attribute('cluster_name')
  input_zone = attribute('zone')
  input_user_email = attribute('user_email')
  input_service_account_key = attribute('service_account_key')
  input_kubeconfig_path = attribute('kubeconfig_path')  # New input for KUBECONFIG path

  # Set environment variables and perform authentication
    ENV['GOOGLE_APPLICATION_CREDENTIALS'] = input_service_account_key
    ENV['KUBECONFIG'] = input_kubeconfig_path

    describe file(input_service_account_key) do
      it { should exist }
      it { should be_file }
    end

    # Setup Kubernetes credentials using the service account
    describe command("gcloud auth activate-service-account --key-file=$GOOGLE_APPLICATION_CREDENTIALS") do
      its('exit_status') { should eq 0 }
    end

    # Assign IAM roles
    describe command("gcloud projects add-iam-policy-binding #{input_project_id} --member=\"user:#{input_user_email}\" --role=\"roles/container.viewer\"") do
      its('exit_status') { should eq 0 }
    end

    describe command("gcloud projects add-iam-policy-binding #{input_project_id} --member=\"user:#{input_user_email}\" --role=\"roles/container.developer\"") do
      its('exit_status') { should eq 0 }
    end

    # Get credentials for the Kubernetes cluster
    describe command("gcloud container clusters get-credentials #{input_cluster_name} --zone #{input_zone} --project #{input_project_id}") do
      its('exit_status') { should eq 0 }
    end

    # Optionally configure kubectl for the user
    describe command("kubectl config set-context --current --user=#{input_user_email}") do
      its('exit_status') { should eq 0 }
    end

    # Ensure the necessary RBAC role exists
    describe command("kubectl create clusterrolebinding my-user-binding --clusterrole=cluster-admin --user=#{input_user_email}") do
      its('exit_status') { should eq 0 }
    end
  
  # Test assertions after setup
  describe kubernetes_pod(name: 'my-pod') do
    it { should be_running }
    its('status') { should eq 'Running' }
  end

  describe kubernetes_service(name: 'my-service') do
    it { should exist }
    it { should have_port(80) }
    its('type') { should eq 'NodePort' }
  end

  describe kubernetes_deployment(name: 'my-deployment') do
    it { should exist }
    it { should have_replicas(3) }
  end

  # Failing test: Check for a service that does not exist
  describe kubernetes_service(name: 'nonexistent-service') do
    it { should_not exist }
  end

  # Cleanup: Delete the RBAC binding
  describe command('kubectl delete clusterrolebinding my-user-binding') do
    its('exit_status') { should eq 0 }
  end
end
