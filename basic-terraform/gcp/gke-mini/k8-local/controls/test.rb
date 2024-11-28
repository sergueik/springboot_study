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
    
    describe 'validating Kubernetes node status' do
      # Run the kubectl command to get node details in JSON format
      subject do
        command('kubectl get nodes -o jsonpath="{.items[*].status.nodeInfo}"')
      end
    
      # Ensure the command executed successfully
      its('exit_status') { should eq 0 }
    
      # Validate that the command outputs the necessary field (nodeInfo)
      its('stdout') { should match /osImage/ }
    
      it 'should have Container-Optimized OS image' do
        # Load the stdout (JSON string) and parse it
        parsed_json = JSON.parse(subject.stdout)
    
        # Extract the 'osImage' using dig and validate it
        os_image = parsed_json.dig(0, 'osImage') # Assuming the JSON contains an array and we want the first item
    
        expect(os_image).to eq('Container-Optimized OS from Google')  # Expected value for osImage
      end
    end
  end

  describe 'validating Kubernetes node status' do
    # Run the kubectl command to get node details in JSON format
    subject do
      command('kubectl get nodes -o jsonpath="{.items[*].status.nodeInfo}"')
    end
  
    # Ensure the command executed successfully
    its('exit_status') { should eq 0 }
  
    # Validate that the command outputs the necessary field (nodeInfo)
    its('stdout') { should match /osImage/ }
  
  
    # Check the structure and print the parsed JSON for debugging purposes
    describe 'parsed JSON structure' do
      # Load the stdout (JSON string) and parse it
      it 'should have an array of nodeInfo' do
        parsed_json = JSON.parse(subject.stdout)
        expect(parsed_json).to be_an(Array)
      end
    
        # Inspect the contents of the first element (if any)
        it 'should have nodeInfo containing osImage' do
          parsed_json = JSON.parse(subject.stdout)
          node_info = parsed_json.first  # Get the first nodeInfo entry
          expect(node_info).to have_key('osImage')
        end
    end
  
    # Extract and validate the 'osImage'
    describe 'validating osImage' do
      it 'should have the correct osImage' do
        parsed_json = JSON.parse(subject.stdout)
        node_info = parsed_json.first  # Get the first nodeInfo entry
        os_image = node_info['osImage']  # Extract the osImage value
        expect(os_image).to eq('Container-Optimized OS from Google')  # Expected value for osImage
      end
    end
  end
end
