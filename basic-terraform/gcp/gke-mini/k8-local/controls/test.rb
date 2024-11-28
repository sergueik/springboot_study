# k8s_cluster_setup.rb
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

  # Setup kubeconfig and RBAC permissions before tests
  before do
    # Check if the service account key exists to authenticate
    if File.exist?(input_service_account_key)
      # Set the environment variable for authentication using the service account key
      ENV['GOOGLE_APPLICATION_CREDENTIALS'] = input_service_account_key

      # Authenticate using the service account credentials
      system("gcloud auth activate-service-account --key-file=$GOOGLE_APPLICATION_CREDENTIALS")

      # Assign the necessary IAM roles
      system("gcloud projects add-iam-policy-binding #{input_project_id} --member=\"user:#{input_user_email}\" --role=\"roles/container.viewer\"")
      system("gcloud projects add-iam-policy-binding #{input_project_id} --member=\"user:#{input_user_email}\" --role=\"roles/container.developer\"")

      # Get the credentials for the Kubernetes cluster
      system("gcloud container clusters get-credentials #{input_cluster_name} --zone #{input_zone} --project #{input_project_id}")

      # Explicitly set the KUBECONFIG environment variable for kubectl and InSpec
      ENV['KUBECONFIG'] = "~/.kube/config"
      
      # Optionally, configure kubectl to point to the correct user context
      system("kubectl config set-context --current --user=#{input_user_email}")

      # Create RBAC permissions if needed (cluster-admin role)
      system("kubectl create clusterrolebinding my-user-binding --clusterrole=cluster-admin --user=#{input_user_email}")
    else
      skip "Service account credentials file not found at #{input_service_account_key}, skipping setup."
    end
  end

  # Example test: Check if the pod is running
  describe kubernetes_pod(name: 'my-pod') do
    it { should be_running }
    its('status') { should eq 'Running' }
  end

  # Example test: Verify the service configuration
  describe kubernetes_service(name: 'my-service') do
    it { should exist }
    it { should have_port(80) }
    its('type') { should eq 'NodePort' }
  end

  # Example test: Verify deployment exists and has correct number of replicas
  describe kubernetes_deployment(name: 'my-deployment') do
    it { should exist }
    it { should have_replicas(3) }
  end

  # Cleanup: Remove the RBAC binding after tests
  after do
    # Clean up RBAC bindings if they were created
    if system('kubectl get clusterrolebinding my-user-binding')
      system('kubectl delete clusterrolebinding my-user-binding')
    end
  end
end
