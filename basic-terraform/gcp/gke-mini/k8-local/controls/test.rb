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

  # Debugging: Print the attributes
  describe 'Debug: Check attributes' do
    it 'outputs the project_id' do
      puts "Project ID: #{input_project_id}"
    end
    it 'outputs the cluster_name' do
      puts "Cluster Name: #{input_cluster_name}"
    end
    it 'outputs the zone' do
      puts "Zone: #{input_zone}"
    end
    it 'outputs the user_email' do
      puts "User Email: #{input_user_email}"
    end
    it 'outputs the service_account_key' do
      puts "Service Account Key: #{input_service_account_key}"
    end
    it 'outputs the kubeconfig_path' do
      puts "Kubeconfig Path: #{input_kubeconfig_path}"
    end
  end

  # Ensure all required attributes are set
  if input_project_id == Inspec::Input::NO_VALUE_SET
    raise 'Attribute project_id is not set in attributes.yml'
  end
  if input_cluster_name == Inspec::Input::NO_VALUE_SET
    raise 'Attribute cluster_name is not set in attributes.yml'
  end
  if input_zone == Inspec::Input::NO_VALUE_SET
    raise 'Attribute zone is not set in attributes.yml'
  end
  if input_user_email == Inspec::Input::NO_VALUE_SET
    raise 'Attribute user_email is not set in attributes.yml'
  end
  if input_service_account_key == Inspec::Input::NO_VALUE_SET
    raise 'Attribute service_account_key is not set in attributes.yml'
  end
  if input_kubeconfig_path == Inspec::Input::NO_VALUE_SET
    raise 'Attribute kubeconfig_path is not set in attributes.yml'
  end

  # Test for missing service account key file
  only_if { File.exist?(input_service_account_key) }

  describe file(input_service_account_key) do
    it { should exist }
    it { should be_file }
  end

  # Setting the KUBECONFIG environment variable
  ENV['KUBECONFIG'] = input_kubeconfig_path

  describe 'run gcloud auth activate service account' do
    subject { command("gcloud auth activate-service-account --key-file=$GOOGLE_APPLICATION_CREDENTIALS") }
    its('exit_status') { should eq 0 }
  end

  describe 'run gcloud projects add-iam-policy-binding for container.viewer' do
    subject { command("gcloud projects add-iam-policy-binding #{input_project_id} --member=\"user:#{input_user_email}\" --role=\"roles/container.viewer\"") }
    its('exit_status') { should eq 0 }
  end

  describe 'run gcloud projects add-iam-policy-binding for container.developer' do
    subject { command("gcloud projects add-iam-policy-binding #{input_project_id} --member=\"user:#{input_user_email}\" --role=\"roles/container.developer\"") }
    its('exit_status') { should eq 0 }
  end

  describe 'run gcloud container clusters get-credentials' do
    subject { command("gcloud container clusters get-credentials #{input_cluster_name} --zone #{input_zone} --project #{input_project_id}") }
    its('exit_status') { should eq 0 }
  end

  describe 'run kubectl get nodes' do
    subject { command('kubectl get nodes') }
    its('exit_status') { should eq 0 }
  end
end
