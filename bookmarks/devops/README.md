### Udemy Course Catalog

  * https://www.udemy.com/topic/devops/#!&p=67
  * https://www.edureka.co/devops-certification-training 500

    + Git
    + Jenkins
    + Docker
    + Ansible
    + Terraform
    + Kubernetes
    + Prometheus
    + Grafana

      - Overview of DevOps
      - Version Control with Git
      - Git, Jenkins and Maven Integration
      - Continuous Integration using Jenkins
      - Configuration Management using Ansible
      - Containerization using Docker Part - I
      - Containerization using Docker Part - II
      - Orchestration using Kubernetes Part - I
      - Orchestration using Kubernetes Part - II
      - Monitoring using Prometheus and Grafana
      - Provisioning using Terraform Part - I
      - Provisioning using Terraform Part - II
      - Selenium (Self-Paced)
      - Nagios (Self-Paced)
      - DevOps on Cloud (Self-Paced)
      - AWS EC2 and IAM (Self-Paced)

    + coupon code "SAVE20" and Get FLAT 20% OFF

  * https://www.fau.edu/online/online-programs/programs/big-data-analytics-graduate-certificate/
  * https://www.coursera.org/learn/aws-cicd-pipelines-and-deployment-strategies
  * https://www.coursera.org/specializations/exam-prep-dop-c02-aws-certified-devops-engineer--professional
  * https://www.coursera.org/learn/foundations-of-business-intelligence
  * https://www.coursera.org/learn/motivate-people-teams (?)
  * https://www.coursera.org/professional-certificates/google-business-intelligence
   + https://www.coursera.org/learn/the-path-to-insights-data-models-and-pipelines
   + https://www.coursera.org/learn/decisions-decisions-dashboards-and-reports
   + https://www.coursera.org/learn/leadershipskills/home/welcome
#### DevOps IaC ( Terraform) - Config management ( Ansible )
  * In which phase of the CI/CD pipeline can Infrastructure as Code (IaC) be integrated?
    +  build phase, where IaC scripts can be used to automatically provision the necessary infrastructure for the application which ensures that the required infrastructure is in place before deploying the application.
  * describe the basic structure of a Terraform configuration `.tf` file
    +  three main sections: variables, resources, and outputs
  * explain the concept of "declarative" and "imperative" in the context of Terraform
    + declarative approach means defining the desired state of the infrastructure in a configuration file without specifying the exact sequence of steps needed to achieve that state. Terraform then determines the necessary actions to reach the desired state and performs them automatically. This approach focuses on describing what the infrastructure should look like rather than how to achieve it
  * what will happen if you lose the state file in Terraform, and how can we recreate the state file?
    +  lose the state file, Terraform will be unable to manage the existing resources and may result in conflicts and errors. To recreate the state file, you can use the `terraform import` command for each resource.
  * What differentiates Terraform from Ansible.
     + the primary difference between Terraform and Ansibleis: Terraform is an Infrastructure as Code (IaC) tool primarily focused on provisioning and managing infrastructure resources. It uses declarative language to define the desired state of the infrastructure and handles resource creation, modification, and deletion. Ansible, on the other hand, is a configuration management tool that focuses on automating the deployment and management of software applications on existing infrastructure
  * What are Ansible roles ?
     + Ansible roles are predefined sets of tasks, variables, and files that encapsulate a specific functionality or configuration. They provide a structured and reusable way to organize playbook logic, making it easier to manage complex deployments. Roles can be easily shared and reused across different projects, promoting consistency and reducing duplication of effort.
  * Key features of Ansible
     + Infrastructure as Code (IaC): Ansible allows you to define infrastructure configurations as code, making it easy to manage and automate infrastructure deployment.
     + Agentless Architecture: Ansible uses SSH or WinRM to connect to target systems, eliminating the need for any additional agents or daemons to be installed.
     + Playbooks: Ansible uses Playbooks, which are YAML files that describe the desired state of systems.
     + Idempotent Execution: Ansible ensures idempotent execution, meaning that applying the same configuration multiple times has the same result as applying it once.

#### DevOps Containerization ( DOCKER - KUBERNETES )
  *  basic architecture of Docker
    + core components of Docker are Docker Daemon, Docker CLI, and Docker Image Registry
  * Docker image
    + template that contains an application and its dependencies. It can be created using a Dockerfile, which specifies the instructions to build the image.
  * how Docker manage networking
    +  has built-in virtual network called `docker0` and can create custom user-defined bridge networks using the 'docker network' command. Containers within the same network can communicate with each other using their container names.
  * Docker container life cycle
    + Created
    + Running
    + Paused
    + Stopped
    + Deleted
  * difference between CMD and ENTRYPOINT
    + `CMD` to specify the default command to run when a container is started
    + `ENTRYPOINT` to define the main command and its arguments that are always executed when the container is run
  *  troubleshoot a Docker container run on a remote server you do not have direct access to
    +  `docker logs <container_id>` to view the container's logs and identify potential errors or issues.
    + `docker stats <container_id>` provides real-time resource usage statistics for the container, such as CPU, memory, and network usage
    + `docker exec -it <container_id>` bash allows you to access the container's shell interactively, which can be helpful for debugging purposes. However, in this scenario, you do not have direct access to the remote server, so running an interactive shell is not a feasible option.
    + `ocker inspect <container_id>` provides a wealth of detailed information about the container, including its configuration, networking details, and more. While this command can be useful for gathering general information about the container, it does not directly assist in diagnosing the issue within the container's runtime behavior.
  * orchestration
    +  is the process of automating and coordinating the deployment, management, and scaling of containerized applications. It helps streamline the DevOps workflow by enabling teams to efficiently manage infrastructure and application deployments.
  * Container orchestration
    + is the process of automating the deployment, scaling, and management of containers. We need it to ensure fault tolerance and high availability.
  * basic architecture of Kubernetes
    +  master node and multiple worker nodes. The master node manages the cluster and schedules containers, while the worker nodes run the containers. Pods are the logical groups of containers that share networking and storage resources.
    + architecture of Kubernetes consists of three main components: Control Plane, Nodes, and Kubernetes API Server. The Control Plane includes the Master Node components like Scheduler, Controller Manager, and etcd. The Nodes host the containers, and the Kubernetes API Server handles API requests and interacts with the Control Plane.
    + Control Plane: The Control Plane, also known as the Master Node components, is responsible for managing the cluster's state and making global decisions about the cluster. It includes the following components:

      - Scheduler: The Scheduler is responsible for assigning newly created pods to appropriate worker nodes based on resource requirements and constraints.
      - Controller Manager: The Controller Manager includes various controllers that handle tasks such as node scaling, replication, and monitoring the desired state of resources.
      - etcd: etcd is a distributed key-value store that holds the configuration and state data of the entire cluster. It serves as the cluster's data store and provides high availability.

    + Nodes: Nodes are the worker machines where containers are deployed and run. Each node runs a container runtime, such as Docker or Containerd, to manage the containers. The main components on each node are:

     - Kubelet: The Kubelet is an agent running on each node that communicates with the Control Plane and ensures that containers are running as expected on the node.
     - Container Runtime: The Container Runtime is responsible for pulling container images and running containers on the node.

    + Kubernetes API Server: The Kubernetes API Server is the central management point for the entire cluster. It exposes the Kubernetes API, which is used by various components to interact with the cluster. The API server handles requests from the command-line interface (kubectl), the web UI (Dashboard), and other automation tools.
  * What is a Pod
    +  Pod in Kubernetes is an isolated and self-contained execution environment that runs one or more containers. It allows containers to share the same resources, such as network namespace and storage volumes, while maintaining their own process and file system isolation.
  * How Kubernetes handle container networking and communication
    +  Kubernetes does not have its own networking implementation but relies on the underlying network infrastructure to handle container networking,  uses network overlays and software-defined networking (SDN) to provide connectivity and isolation between containers.
    + Popular SDN solutions used with Kubernetes include Flannel, Calico, Weave, and Cilium.
    + With network overlays, Kubernetes creates an abstraction layer that allows containers to communicate with each other across different nodes as if they were on the same network. This is achieved by encapsulating and tunneling the container traffic over the underlying network infrastructure. Network policies are then used to control and secure the traffic between pods, specifying rules for allowing or blocking communication based on various criteria such as source IP, destination IP, ports, and protocols.
    + By relying on the underlying network infrastructure and SDN, Kubernetes enables seamless networking and communication between containers, regardless of the underlying physical or virtual network setup. It provides flexibility, scalability, and isolation for containerized workloads, allowing them to be easily deployed and managed in a distributed environment.
  *  how manage network rules and direct traffic on each node
    + By configuring the kube-proxy component
    + Kube-proxy operates in iptables mode (for iptables rules on each node to handle packet forwarding and load balancing) or IPVS (IP Virtual Server) mode (for advanced load balancing operations)
    + one can specify rules to route traffic to specific Pods based on selectors or labels, define load-balancing algorithms, and configure service discovery for external access.

  * components of the master node in Kubernetes and their functions
    + `kube-apiserver` is the central control point and the primary management component of the Kubernetes control plane. It exposes the Kubernetes API and handles all API requests, validating them and maintaining the desired state of the cluster.
    + `etcd` is a distributed key-value store used by Kubernetes to store the entire configuration and the state of the cluster. It provides a reliable and consistent data store for the control plane components to read and write cluster information.
    + `kube-controller-manager` consists of multiple controllers that monitor and manage different aspects of the cluster. These controllers include the Node Controller, Replication Controller, and Service Controller, among others. Each controller focuses on specific tasks to maintain the desired state of the cluster, such as managing nodes, ensuring desired pod replicas, and handling service endpoints.
    + `kube-scheduler` is responsible for making intelligent decisions about pod placement within the cluster. It considers factors like resource requirements, node conditions, and affinity/anti-affinity rules to determine the most suitable node for scheduling a pod. The `kube-scheduler` ensures optimal resource utilization and high availability by assigning pods to appropriate nodes.
  * components of the worker node and their functions
    + `kubelet` is an agent that runs on each worker node and is responsible for managing the state of pods. It communicates with the Kubernetes control plane (API server) and ensures that the containers described in the pod specifications are running and healthy.
    + `container runtime` is the software responsible for running containers. Kubernetes supports multiple container runtimes such as Docker, containerd, and CRI-O. The container runtime pulls container images, creates and manages container processes, and provides the necessary isolation and resource management for running containers.
    + `kube-proxy` is a network proxy that runs on each worker node. It maintains network rules and manages network communication to the pods. It enables connectivity to services and load balances traffic between pods within the cluster. It also provides network address translation (NAT) for accessing pods from outside the cluster.
  * Kubectl
    + powerful command-line tool used to interact with Kubernetes clusters. It acts as a client for the Kubernetes API server, allowing users to manage and control Kubernetes resources. Some common operations that can be performed using kubectl include:
      - Creating, updating, and deleting Kubernetes resources such as pods, deployments, services, and namespaces.
      - Inspecting the status and details of running pods, nodes, and other cluster components.
      - Scaling the number of replicas for a deployment or statefulset.
      - Applying or rolling back changes to the configuration of Kubernetes resources.
      - Executing commands inside containers running in pods.
      - Port-forwarding to access services running in pods from the local machine.
      - Viewing logs and events for troubleshooting and monitoring purposes.
    +  a few commonly used commands in kubectl along with a brief explanation of each:
      - `kubectl get`: This command is used to retrieve information about Kubernetes resources. For example, kubectl get pods retrieves a list of running pods in the cluster, kubectl get deployments retrieves a list of deployments, and kubectl get nodes retrieves a list of cluster nodes.
      - `kubectl describe`: This command provides detailed information about a specific resource
      - `kubectl create`: This command is used to create new Kubernetes resources.
      - `kubectl apply`: This command applies configuration changes to existing resources or creates new resources based on a configuration
      - `kubectl delete`: This command is used to delete resources
      - `kubectl exec`: This command allows you to execute commands inside a running container in a pod
      - `kubectl logs`: This command retrieves the logs of a specific pod
      - `kubectl port-forward`: This command enables port forwarding from a local machine to a specific pod or service
      - `kubectl scale`: This command is used to scale the number of replicas in a deployment or statefulset

#### DevOps Cloud - AWS
  * key benefits of cloud computing
    + allows to reduce downtime and achieve high availability by leveraging redundant infrastructure and automatic failover mechanisms.
  *  instance types are available in EC2 ?
    + T2 instances: These instances provide burstable performance and are suitable for workloads with variable CPU usage.
    + M5 instances: These instances offer a balance of compute, memory, and networking resources, making them suitable for general-purpose workloads.
    + C5 instances: These instances are optimized for compute-intensive workloads, providing high-performance computing capabilities.
    + R5 instances: These instances are designed for memory-intensive applications, offering high memory capacity and bandwidth.
    + I3 instances: These instances are optimized for storage-intensive workloads, providing high-performance, low-latency storage.
    + G3 instances: These instances are GPU instances designed for graphics-intensive applications and workloads that require powerful GPU capabilities.
    + P3 instances: These instances are optimized for machine learning, deep learning, and other GPU-accelerated tasks, offering high-performance GPUs.
  * pricing models are there for the EC2
    + On-Demand Instances: These instances have no upfront costs or long-term commitments. You pay for compute capacity by the hour or by the second, depending on the instance type.
    + Reserved Instances: Reserved Instances provide a significant discount compared to On-Demand Instances. They require a one-time upfront payment and a commitment for a specified term, such as one or three years.
    + Spot Instances: Spot Instances offer the ability to bid on unused EC2 capacity, allowing you to obtain instances at significantly lower prices. The pricing fluctuates based on supply and demand, and instances can be interrupted with a two-minute notice.
    + Savings Plans: Savings Plans provide flexibility and cost savings for predictable usage. They offer discounts on compute usage in exchange for a commitment to a specific dollar amount per hour for a one- or three-year term.
    + Dedicated Hosts: Dedicated Hosts provide physical servers dedicated to your use. They allow you to bring your existing server-bound software licenses and meet compliance requirements that require dedicated hardware.
  * managed services in AWS
    +  AWS Elastic Beanstalk: This service provides a platform-as-a-service (PaaS) offering that simplifies the deployment and management of applications. It automatically handles infrastructure provisioning, load balancing, scaling, and health monitoring.
    +  AWS OpsWorks: OpsWorks is a configuration management service that automates the deployment and management of applications. It allows you to define the application's architecture, stack configuration, and deployment workflows using predefined templates.
    +  AWS CloudFormation: CloudFormation is an infrastructure-as-code service that allows you to define and provision AWS resources using declarative templates. It automates the process of creating and managing stacks of resources, making it easier to manage complex deployments.
  * Autoscaling
    + Autoscaling is the process of automatically adjusting the capacity of resources in response to changing demand.
      - Vertical scaling involves adding or removing resources to or from an individual instance or resource. It typically involves upgrading or downgrading the capacity of a resource.
      - Horizontal scaling involves increasing or decreasing the number of instances or resources to handle the workload.
  * replicate an EC2 instance
    + Create AMI
  * Storage available in AWS
    + Amazon Elastic Block Store (EBS)
    +  Amazon Simple Storage Service (S3)
    +  Amazon Elastic File System (EFS)
    +  Amazon Glacie
  * secure data at rest in AWS?
    +  Access control policies with IAM
    + Encryption with AWS KMS
    + Server-side encryption with S3:
    + Encryption in transit with CloudFront
  * achieve high availability and fault tolerance for our app deployed on Ec2 instance ?
    + Utilize AWS Elastic Beanstalk to deploy and manage your application. Elastic Beanstalk automatically handles deployment, capacity provisioning, and load balancing across multiple instances and availability zones.
    + Use Amazon RDS Multi-AZ deployment for database high availability and automatic failover.
    + Implement Amazon CloudWatch alarms to monitor the health of EC2 instances and trigger Auto Scaling actions if instances become unhealthy.
    + Utilize Amazon Route 53 DNS failover to redirect traffic to healthy instances in case of a failure.
  * monitor different aws resources using cloud watch
    + use AWS CloudWatch Metrics to monitor and collect data on various AWS resources such as EC2 instances, RDS databases, and S3 buckets.
    + Set up CloudWatch Alarms to send notifications or take automated actions based on defined thresholds for specific metrics.
    + Utilize CloudWatch Logs to collect and monitor log data from applications, services, and operating systems.
    + Configure CloudWatch Events to respond to changes in AWS resources by triggering actions or sending notifications.
  *  also:
    + AWS CloudTrail is to monitor and track user activity and API usage across your AWS infrastructure.
    + CloudWatch Logs Insights is to analyze and query log data from CloudWatch Logs for troubleshooting and monitoring purposes.
    + CloudWatch Container Insights is to monitor and troubleshoot containerized applications running on AWS services like Amazon ECS and EKS.
    + AWS X-Ray is to trace and monitor requests as they flow through your application, identifying bottlenecks and performance issues.
    + CloudWatch Synthetic Monitoring is to simulate user interactions with your application and monitor its availability and responsiveness.
    + CloudWatch Application Insights to gain visibility into the performance and health of your applications running on EC2 instances or containers.
    + CloudWatch Anomaly Detection to automatically detect and alert on anomalies in your metric data, helping you identify unusual patterns or behavior.
    + CloudWatch ServiceLens to monitor and troubleshoot distributed applications running on AWS services like Lambda, API Gateway, and AppSync
    + AWS Config to monitor and assess resource configurations, compliance, and changes across your AWS account.
    + CloudWatch Contributor Insights to identify top contributors to high-dimensional log data and investigate issues faster
Set up CloudWatch Dashboards to create customized visualizations of the metrics and logs that are most important to your application.
  * prevent or handle DDoS and DoS attacks in AWS?
    + Utilize AWS Shield for DDoS protection
    + Configure Amazon CloudFront with AWS WAF to filter out malicious traffic.
    + Enable AWS CloudTrail to monitor and log potential attacks.
  * Highly available web application is running in two different AWS regions: Region A and Region B. configure DNS failover using Amazon Route 53 to direct traffic to Region B in case Region A becomes unavailable
    + Create a single Route 53 record set with a primary record in Region A and a secondary record in Region B.
    + Configure health checks for both regions to monitor the availability of the applications.
    + Use a failover routing policy to direct traffic to the secondary record in Region B if the health check for the primary record in Region A fails.
    + Adjust the failover settings to ensure fast and accurate failover based on health check results
  * setting up auto scaling for a web application that experiences high traffic during peak hours and low traffic during off-peak hours
    + Configure an auto scaling group with dynamic scaling policies based on CPU utilization.
    + Set up a target tracking scaling policy to increase or decrease the number of instances based on CPU utilization thresholds.
    + Adjust the thresholds to scale up during peak hours and scale down during off-peak hours
  * EBS volumes use cases
    + EBS volumes are Elastic Block Store volumes in AWS that provide persistent block-level storage for EC2 instances.
      - store operating systems, databases, and applications that require consistent and low-latency storage.
    + EBS volumes can be attached and detached from EC2 instances as needed.
    + EBS volumes can be encrypted for enhanced data security

 ### AWS Developer
  * https://www.udemy.com/course/working-with-sqs-and-sns-aws-with-python-and-boto3-series
  * https://www.udemy.com/course/hands-on-aws-sqs/?couponCode=LETSLEARNNOWPP
  * https://medium.com/aws-serverless-microservices-with-patterns-best/udemy-course-published-aws-serverless-microservices-with-patterns-best-practices-639a5cd13482
  * https://www.udemy.com/course/aws-sqs-aws-sqs-tutorial
  * https://www.udemy.com/course/aws-sqs-aws-sqs-tutorial/?couponCode=LETSLEARNNOWPP

###
  * [Building Your Leadership Skills](https://www.coursera.org/learn/leadership-skills)
  * [Principles of Management](https://www.coursera.org/learn/principles-of-management)
     - planning
     - leading
     - organizing
     - controlling
  * https://www.coursera.org/learn/leadershipskills
  * https://www.coursera.org/specializations/principles-of-leadership-leading-technical-teams
  * [Leading People and Teams Specialization](https://www.coursera.org/specializations/leading-teams)
  * [Strategic Leadership and Management Specialization](https://www.coursera.org/specializations/strategic-leadership)

### Jenkins Pipelines
  * https://praveendandu24.medium.com/understanding-the-differences-between-jenkins-scripted-and-declarative-pipeline-a-comprehensive-960826e26c2
  * https://www.geeksforgeeks.org/how-to-make-a-ci-cd-pipeline-in-jenkins/
  * https://www.jenkins.io/doc/book/pipeline/
    + https://www.jenkins.io/doc/pipeline/tour/hello-world
    + https://www.jenkins.io/doc/pipeline/tour/running-multiple-steps/
    + https://www.jenkins.io/doc/pipeline/tour/post/
    + https://www.jenkins.io/doc/pipeline/steps/
    + https://www.jenkins.io/doc/book/pipeline/syntax/
    + https://www.jenkins.io/doc/book/pipeline/jenkinsfile/
    + https://www.jenkins.io/doc/book/pipeline/shared-libraries/
    + https://www.jenkins.io/doc/pipeline/steps/workflow-basic-steps/
  * https://www.blazemeter.com/blog/jenkins-scripted-pipeline
  * https://www.jenkins.io/doc/pipeline/examples/
  * https://www.geeksforgeeks.org/jenkins-interview-questions/?ysclid=lxm40gsznb837390846	
  * https://www.jenkins.io/blog/2019/12/02/matrix-building-with-scripted-pipeline/
### GitLab

  * https://radixweb.com/blog/github-vs-gitlab
    + https://radixweb.com/services/devops/automation
  * https://docs.github.com/en/actions/migrating-to-github-actions/manually-migrating-to-github-actions/migrating-from-gitlab-cicd-to-github-actions
  * https://thexz3dev.medium.com/feature-parity-comparison-gitlab-ci-cd-vs-github-actions-37401d3e3b1c

### Ansible

 * [developing modules](https://docs.ansible.com/ansible/latest/dev_guide/developing_modules_general.html)
 * roles
   +  `tasks`, `handlers`, `modules`, `defaults`, `variables`, `files`, `templates`, `meta`
 * https://spacelift.io/blog/ansible-modules
 * https://blog.devops.dev/ansible-inventory-dynamic-vs-static-7f40e4994b4d
 * https://www.atlassian.com/devops/what-is-devops/devops-engineer
  + Communication and collaboration
  + System administration
  + Experience with DevOps tools
  + Configuration management tools
  + Containers and container orchestration
  + Continuous integration and continuous deployment
  + System architecture and provisioning
  + Familiarity with coding and scripting
  + Collaborative management skills

### Generative AI
  * Generative AI Fundamentals specialization
    + Introduction and Applications
    + 3 NN
       - [Convolutional](https://en.wikipedia.org/wiki/Convolutional_neural_network)
       - [Recurrent NN](https://en.wikipedia.org/wiki/recurrent_neural_network)
       - [Transformer](https://en.wikipedia.org/wiki/Transformer_(deep_learning_architecture))
    + Core AI Models
       - Variational Auto Encoders
       - Generative Advesarial Networks
       - Transformer
       - Diffusion
    + Foundational Model
      - pretrained on huge unlabelerd data
      - general purpose
      - self-supervised
      - has billions parameters
      - capable to adape to multiple applications
      - accepts prompts in miltiple modalities images text video audio
    + Prompt Engineering Basics
      - instruction
      - context
      - input data
      - output indicator
      - critial analysis
      - creativity
      - acumen
      + steps
      - define the goal
      - draft initial prompt
      - test the ptompt
      - analyze the response
      - refine prompt
      - iterate
    + criteria
       - clarity
       - context
       - precision
       - persona /roleplay
    + prompt common use cases
       - summariation
       - clssificsation
       - generation
       - extraction
       - auto completion
       - question answering
    + techniques for effectiveness
       - exlicit objective
       - contextual guideness
       - domain expertise
       - bias mitigation
       - framing
       - user feedback loop
    + Foundation Models and Platforms
    + Impact, Considerations, and Ethical Issues
    + Business Transformation and Career Growth
  * Generative AI for Software Developers Specialization
 includes the following courses:

    + Introduction and Applications
    + Prompt Engineering Basics
    + Elevate your Software Development Career

  * https://www.coursera.org/learn/introduction-to-data-engineering/home/week/1
  * https://www.coursera.org/learn/architecting-solutions-on-aws/home/welcome
  * https://www.coursera.org/learn/launching-machine-learning/home/welcome
  * https://www.coursera.org/learn/vector-search-and-embeddings/home
  * https://www.coursera.org/learn/microsoft-azure-dp-203-data-engineering/home/welcome
  * https://www.coursera.org/learn/microsoft-azure-sql/home/welcome
  * https://www.coursera.org/professional-certificates/aws-cloud-support-associate
  * https://www.coursera.org/specializations/conflict-management
  * https://www.coursera.org/specializations/leadership-introduction
  * https://www.coursera.org/learn/leadershipskills/home/welcome
  * https://www.coursera.org/learn/agile-leadership-introduction-to-change/home/welcome
  * https://www.coursera.org/learn/leaders/home/welcome
  * https://www.coursera.org/specializations/leadership-introduction
  * https://www.cadtutor.net/forum/topic/75673-penn-foster-course-vs-realities-of-day-to-day-drafting-work/
  * https://www.mycadsite.com
  * https://www.mycadsite.com/download.html
  * https://www.mycadsite.com/tutorials/level_1/introduction-to-AutoCAD-basics-1-1.html
  * https://www.autodesk.com/certification/all-certifications/autocad-design-drafting-professional
  * https://www.pennfoster.edu/programs/trades/drafting-with-autocad-career-diploma
  * https://www.pennfoster.edu/programs/computers/autocad-certificate
    + 900
    + 1-800-827-2614
    
  * https://www.ed2go.com/courses/computer-applications/autodesk/ctp/autocad-certification-training#outline
    + 1 877.221.5151
  * https://www.browardtechnicalcolleges.com/request-information/
  * https://www.browardtechnicalcolleges.com/school-locations/
   + 754-287-1716
   + 754-287-1718 754.321.5100 (Atlantic) https://www.atlantictechnicalcollege.edu
      - https://www.atlantictechnicalcollege.edu/drafting/
      - https://www.atlantictechnicalcollege.edu/wp-content/uploads/2024/07/Drafting_7-2024.pdf
      - https://www.google.com/maps/place/4700+Coconut+Creek+Pkwy,+Coconut+Creek,+FL+33063/@26.2439363,-80.2080101,14z/data=!4m6!3m5!1s0x88d90327bf248ecd:0x9b671cda0c302ad0!8m2!3d26.2439363!4d-80.1874107!16s%2Fg%2F11v3_xw5g1?entry=ttu
      - vdhumphrey@browardschools.com
      - https://lp.constantcontactpages.com/ev/reg/qsdrbtt

   + 754-287-1718x (McFatter) https://www.mcfattertechnicalcollege.edu
       - Florida Resident Total Cost	$4,799
       - NEXT CLASS BEGINS August 12, 2024
       + franzie.williams@browardschools.com or 754.321.5840
    +    https://www.sheridantechnicalcollege.edu

   * https://www.palmbeachstate.edu/CCE/trades.aspx
    + AUTOCAD INTRODUCTION - TIO0325
    + AUTOCAD INTERMEDIATE - TIO0326
    + Length of Free Trial: 30 days from date of download
    + https://www.autodesk.com/products/autocad/trial-intake - 15-day trial

  * https://www.best-trade-schools.net/programs/technology/cad/
  * https://learningdl.net/linkedin-autocad-2024-essential-training/
  * https://cgdownload.ru/catalog/0406-linkedin-autocad-2024-essential-training/?ysclid=lzevfc848a311710104
  * https://sanet.st/blogs/training4all/autocad_complete_course_practical_approach_updated.3752746.html
  * https://www.udemy.com/course/autocad-2018-getting-started-quickly-with-autocad/?ysclid=lzfm4anpb2760542789&couponCode=KEEPLEARNING
  * https://www.autodesk.com/training
  * https://www.udemy.com/topic/autocad/
  * https://www.skillshare.com/browse/autocad
  * https://ce.arizona.edu/classes/autocad-online-training
  * https://www.cadtrainingonline.com/
  * https://www.fau.edu/continuinged/certificates-and-programs  
    + AZ-104: Microsoft Azure Administrator
    + AZ-900 & 104: Microsoft Azure Fundamental & Administrator
    + AZ-900 & 500: Microsoft Azure Fundamental & Security Technologies
    + AZ-900: Microsoft Azure Fundamentals
    + AutoCAD Certification Training
    + AutoCad Basics
    + Certified AWS Cloud Practitioner Developer Associate
    + Certified AWS Cloud Practitioner Solutions Architect
    + Certified AWS Cloud Practitioner SysOps Administrator
    + Certified AWS Solutions Architect
    + Certified AWS Developer
  * https://www.fau.edu/engineering/eecs/graduate/certificates/big-data/admiss/
  * https://www.fau.edu/engineering/eecs/undergraduate/data-science-and-analytics/
  * https://www.fau.edu/continuinged/redirect?url=https%3A%2F%2Fwww.ed2go.com%2Fcourses%2Fcomputer-applications%2Fautodesk%2Fctp%2Fautocad-certification-training&l=59&q=AutoCAD%20Certification%20Training%20(Voucher%20Included)
  * https://www.fau.edu/continuinged/redirect?url=https%3A%2F%2Fcareertraining.ed2go.com%2Ffau%2Ftraining-programs%2Fautocad-basics%2F%3FCategory%3Dcomputer-applications&l=229&q=AutoCad%20Basics  * https://alison.com/course/autocad-beginner-to-professional-training
  * https://www.onlc.com/autocad-training-classes.htm
  * https://www.udemy.com/course/autocad-civil-3d-complete-course-roads-highways-design/?ysclid=lzfmfmmjs9867404413&couponCode=KEEPLEARNING
  * https://bookscouter.com/book/9781305659728-engineering-drawing-and-design
  * https://www.chegg.com/textbooks/engineering-drawing-and-design-6th-edition-9781305659728-1305659724?ysclid=lzhju42847870854053
  * [Engineering Drawing and design](https://www.pdfdrive.com/architectural-drafting-and-design-d162133168.html)
  * [Engineering Drawing and design](https://www.pdfdrive.com/engineering-drawing-and-design-d175346802.html)
  * https://www.amazon.com/Architectural-Drafting-Design-Alan-Jefferis/dp/128516573X?ysclid=lzhk38t1iv734462968
  * https://www.amazon.com/Engineering-Drawing-Design-MindTap-Course-ebook/dp/B01BU3X8YA?ysclid=lzhkabsq83936047150
  * https://www.ed2go.com/courses/computer-applications/autodesk/ctp/autocad-certification-training
  * https://business.fau.edu/masters-phd/it-management/traditional/certificates/big-data-analytics/course-information/index.php
  * https://business.fau.edu/undergraduate/majors/data-science-analytics/
  * https://www.coursera.org/learn/google-ai-essentials
  * https://www.linkedin.com/learning/courses/practice-exam-1-for-aws-certified-solutions-architect-associate-saa-c03/practice-exams/urn:li:la_assessmentV2:54623572?u=67698794