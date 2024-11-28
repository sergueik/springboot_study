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

# Ensure Kubernetes Cluster Setup is Correct
describe 'running gcloud auth activate-service-account' do
  subject { command("gcloud auth activate-service-account --key-file=#{input('service_account_key')}") }
  its('exit_status') { should eq 0 }
end

# Ensure gcloud container cluster credentials are configured correctly
describe 'running gcloud container clusters get-credentials' do
  subject { command("gcloud container clusters get-credentials #{input('cluster_name')} --zone #{input('zone')} --project #{input('project_id')}") }
  its('exit_status') { should eq 0 }
end

# Ensure the Kubeconfig path exists
describe 'ensuring Kubeconfig path is set' do
  subject { file(input('kubeconfig_path')) }
  it { should exist }
  it { should be_file }
end

# Check for kubeconfig contents (e.g., validation of credentials)
describe 'validating Kubeconfig contents' do
  subject { command("kubectl config view --kubeconfig=#{input('kubeconfig_path')}") }
  its('exit_status') { should eq 0 }
  # its('stdout') { should match /clusters/ }
end

# Validate a Kubernetes pod (you can add your specific validation here)
describe 'validating Kubernetes pod' do
  subject { command("kubectl get pod --namespace=default") }
  its('exit_status') { should eq 0 }
  # its('stdout') { should match /pod/ }
end

# Validate a Kubernetes deployment (you can add your specific validation here)
describe 'validating Kubernetes deployment' do
  subject { command("kubectl get deployment --namespace=default") }
  its('exit_status') { should eq 0 }
  # its('stdout') { should match /deployment/ }
end

# Validate a Kubernetes service (you can add your specific validation here)
describe 'validating Kubernetes service' do
  subject { command("kubectl get service --namespace=default") }
  its('exit_status') { should eq 0 }
  # its('stdout') { should match /service/ }
end

# Validate Kubernetes PVC (you can add your specific validation here)
describe 'validating Kubernetes PVC' do
  subject { command("kubectl get pvc --namespace=default") }
  its('exit_status') { should eq 0 }
  # its('stdout') { should match /pvc/ }
end

# Check for Kubernetes node status
describe 'validating Kubernetes node status' do
  subject { command("kubectl get nodes") }
  its('exit_status') { should eq 0 }
  #its('stdout') { should match /Ready/ }
end
describe 'activate service account' do
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

  describe 'validating Kubernetes node status' do
    subject { command("kubectl get nodes -o jsonpath='{.items[*].status.conditions[?(@.type==\"Ready\")].status}'") }
    its('exit_status') { should eq 0 }
    its('stdout') { should match /True/ }  # Expecting the node to be in 'Ready' status
  end
  describe 'validating Kubernetes node OS images' do
    nodes = command("kubectl get nodes -o jsonpath='{.items[*].status.nodeInfo.osImage}'").stdout.strip.split(' ')
  
    nodes.each do |node_os_image|
      describe "checking node OS image: #{node_os_image}" do
        it { expect(node_os_image).to match(/Container-Optimized OS/) }
        it 'outputs node OS image as JSON' do
          node_info_json = { "osImage" => node_os_image, "expected_osImage" => "Container-Optimized OS from Google" }.to_json
          puts "Node Info JSON: #{node_info_json}"
          expect(JSON.parse(node_info_json)["osImage"]).to eq("Container-Optimized OS from Google")
        end
      end
    end  
    
    describe 'validating Kubernetes node info' do
      # Run the kubectl command and capture the output
      subject do
        command("kubectl get nodes -o jsonpath='{.items[*].status.nodeInfo}'")
      end
    
      its('exit_status') { should eq 0 }
    
      it 'Parse the output as JSON' do
        parsed_json = JSON.parse("[#{subject.stdout}]") # Wrap the output in an array to parse multiple items as an array of nodes
      
        # Iterate over each node's info and check for osImage
        parsed_json.each do |node_info|
          describe "node OS image for #{node_info['osImage']}" do
            it 'should be Container-Optimized OS from Google' do
              expect(node_info['osImage']).to match(/Container-Optimized OS/)
            end
          end
        end
      
        # Output the parsed JSON for debugging or inspection
        it 'outputs node info as JSON' do
          puts "Node Info JSON: #{parsed_json.to_json}"
        end
      end
    end
  end    
end