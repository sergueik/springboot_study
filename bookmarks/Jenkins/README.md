###  Pipeline

* https://www.blazemeter.com/blog/jenkins-scripted-pipeline
* https://kimlyvith.medium.com/multibranch-pipeline-jenkins-auto-trigger-changed-and-build-your-code-a84641eca7cc
	



```
pipeline {
    agent any
    stages {
        stage('Welcome Step') {
            steps { 
                echo 'Welcome to LambdaTest'
            }
        }
    }
}
```
#### what are most frequently used jenkins declariative pipeline steps
In Jenkins Declarative Pipelines, certain steps are frequently used for defining and executing common CI/CD tasks. Below is a list of some of the most commonly used steps:

1. `pipeline`
Defines the entire Declarative Pipeline structure.


```
pipeline {
    agent any
    stages {
        ...
    }
}
2. `agent`
Specifies where the pipeline or stage should run.

any: Run on any available agent.
label: Run on agents with a specific label.
docker: Use a Docker container.

```
agent any
agent { label 'linux' }
agent { docker { image 'maven:3.6.3' } }
```

3. stages and stage
Defines a sequence of stages. Each stage represents a major phase of the pipeline.


```
stages {
    stage('Build') {
        steps {
            ...
        }
    }
    stage('Test') {
        steps {
            ...
        }
    }
}
```
4. steps
Contains a sequence of *tasks* within a stage.

steps {
    sh 'mvn clean install'
    echo 'Build completed!'
}

5. `sh`
Runs a shell script (for Linux/macOS agents).

```
sh 'echo "Running tests..."'
```
6. bat
Runs a batch command (for Windows agents).

```

bat 'dir'
```
7. echo
Prints a message to the Jenkins console output.

```
echo 'Hello, Jenkins!'

```
8. checkout
Checks out source code from version control (e.g., Git).

```
checkout scm
checkout([$class: 'GitSCM', branches: [[name: '*/main']], userRemoteConfigs: [[url: 'https://github.com/example/repo.git']]])
```
9. archiveArtifacts
Archives build artifacts for later retrieval.

```

archiveArtifacts artifacts: '**/target/*.jar', allowEmptyArchive: true
```
10. junit
Parses JUnit test result files and displays test results.
```
junit '**/target/surefire-reports/*.xml'

```11. input
Pauses the pipeline and waits for user input.

```
input message: 'Deploy to production?', ok: 'Yes, deploy!'
```
12. timeout
Sets a timeout for a block of steps.

```
timeout(time: 5, unit: 'MINUTES') {
    sh 'long-running-task'
}
```
13. environment
Defines environment variables for the pipeline or stage.

```
environment {
    PATH = "/usr/local/bin:$PATH"
    JAVA_HOME = "/usr/lib/jvm/java-11-openjdk"
}
```
14. when
Applies conditional logic to stages.

```
when {
    branch 'main'
    expression { env.BUILD_NUMBER == '1' }
}
```
15. parallel
Runs multiple branches of steps in parallel.
```
stage('Parallel Tasks') {
    parallel {
        stage('Unit Tests') {
            steps {
                sh 'run-unit-tests.sh'
            }
        }
        stage('Integration Tests') {
            steps {
                sh 'run-integration-tests.sh'
            }
        }
    }
}
```
16. post
Defines actions to take after a stage or pipeline execution.

always: Run regardless of outcome.
success: Run if the pipeline succeeds.
failure: Run if the pipeline fails.

```
post {
    always {
        echo 'Cleaning up...'
    }
    success {
        echo 'Build succeeded!'
    }
    failure {
        echo 'Build failed!'
    }
}
```
17. script
Executes arbitrary Groovy code.

```
script {
    def customMessage = "Build #${env.BUILD_NUMBER}"
    echo customMessage
}
```
18. retry
Retries a block of steps if they fail.

groovy
Copy code
retry(3) {
    sh 'unstable-command'
}
19. withCredentials
Manages sensitive information securely.

```
withCredentials([string(credentialsId: 'my-secret', variable: 'SECRET')]) {
    sh 'echo $SECRET'
}
```
20. withEnv
Adds temporary environment variables for a block of steps.

groovy
Copy code
withEnv(['MY_VAR=value']) {
    sh 'echo $MY_VAR'
}
These steps provide the foundational building blocks for most Jenkins Declarative Pipelines. Let me know if you'd like examples for specific scenarios!





To define a new task or custom logic that can be reused in a Declarative Pipeline, you can use one of the following approaches:

1. Using steps or script Block
You can define custom tasks directly within the pipeline using the steps or script block, where Groovy code can be written.
This is useful for simple tasks that don't require sharing across pipelines.
Example:
groovy
Copy code
pipeline {
    agent any
    stages {
        stage('Custom Task') {
            steps {
                script {
                    def customTask() {
                        echo "Running a custom task"
                        sh 'echo "Task done!"'
                    }
                    customTask()
                }
            }
        }
    }
}
2. Defining a Function in a Shared Library
For tasks that are reused across multiple pipelines, you can define them in a Jenkins Shared Library.
This is the most common and powerful way to define reusable logic in Declarative Pipelines.
Steps:
Create the Shared Library:

In your Jenkins instance, navigate to Manage Jenkins > Configure System > Global Pipeline Libraries and define your shared library.
The shared library should follow a specific directory structure:
```
vars/
    myCustomTask.groovy
```
Define the Custom Task:

In vars/myCustomTask.groovy, define your reusable task:
groovy
Copy code
def call(String message = 'Default message') {
    echo "Custom Task: ${message}"
    sh 'echo "Performing the task"'
}
Use the Shared Library in Declarative Pipeline:

In your pipeline script, import and use the shared library function:

@Library('my-shared-library') _
pipeline {
    agent any
    stages {
        stage('Run Custom Task') {
            steps {
                myCustomTask('Hello, Jenkins!')
            }
        }
    }
}



What is @Library?
@Library is a Groovy annotation used in Jenkins pipeline scripts to load a Shared Library that has been configured in Jenkins.
It indicates that the pipeline depends on code defined in the named Shared Library, which is managed separately from the Jenkinsfile.
2. How does Jenkins know which Library to Load?
The Shared Library name (my-shared-library in this case) corresponds to the configuration you set in Jenkins under Manage Jenkins > Configure System > Global Pipeline Libraries.

Configuration in Jenkins:
Library Name: When you configure a Shared Library in Jenkins, you give it a name, such as my-shared-library.
Source Repository: The Shared Library code is typically stored in a Git repository or a Subversion repository.
Default Version: You can specify a default branch or tag (like main, master, or v1.0) to be used when the library is loaded.
Directory Structure: Jenkins expects a specific structure in the repository:
python
Copy code
(Root of the repository)
├── vars/          # Global variables and functions
│   ├── myTask.groovy
│   ├── anotherTask.groovy
├── src/           # Groovy classes for more advanced logic
│   ├── com/
│       ├── example/
│           ├── utilities.groovy
├── resources/     # Resource files for use by the library
3. What Happens When You Use @Library?
Jenkins loads the Shared Library code into the pipeline at runtime.
For example, if you have a function defined in vars/myTask.groovy within the library, you can call it directly in your pipeline after using @Library.
Example of Shared Library (vars/myTask.groovy):
groovy
Copy code
def call(String message) {
    echo "Message from shared library: ${message}"
}
Example Pipeline Using the Library:
groovy
Copy code
@Library('my-shared-library') _
pipeline {
    agent any
    stages {
        stage('Run Task') {
            steps {
                myTask('Hello from shared library!')
            }
        }
    }
}
4. Does @Library Correspond to a Directory Path?
Not directly. Instead:

The name in @Library corresponds to the library configuration in Jenkins.
Jenkins retrieves the library code from the repository configured under that name.
The actual directory structure in the repository (vars, src, etc.) is used to locate the code within the library.


The Multibranch Pipeline in Jenkins is a powerful feature designed to automate the creation and execution of pipelines for multiple branches in a source code repository. It dynamically discovers, manages, and executes pipelines for each branch based on defined configurations. Here are the key components of a Multibranch Pipeline:

1. Jenkinsfile
A Jenkinsfile is a text file containing the pipeline code, written in either Declarative or Scripted Pipeline syntax.
Each branch should have its own Jenkinsfile (or inherit it from a parent branch), which defines the pipeline's build, test, and deploy stages.
Example of a Jenkinsfile:
groovy
Copy code
pipeline {
    agent any
    stages {
        stage('Build') {
            steps {
                echo 'Building...'
            }
        }
        stage('Test') {
            steps {
                echo 'Testing...'
            }
        }
        stage('Deploy') {
            steps {
                echo 'Deploying...'
            }
        }
    }
}
2. Source Code Repository
The Multibranch Pipeline is linked to a version control system (VCS), such as Git, Bitbucket, GitHub, or GitLab.
Jenkins automatically scans the repository for branches containing a Jenkinsfile.
3. Branch Source Plugin
Plugins like Git, GitHub Branch Source, or Bitbucket Branch Source enable Jenkins to connect to the repository and scan branches or pull requests.
These plugins provide integration points to fetch branches, handle webhooks, and manage SCM-based credentials.
4. Branch Discovery
Jenkins scans the repository and discovers all branches that contain a Jenkinsfile.
The discovered branches are listed as individual jobs under the Multibranch Pipeline project.
5. Build Triggers
Multibranch Pipelines support triggers like:
Webhook Triggers: Automatically build pipelines when changes are pushed to the repository.
Periodic Scans: Configure Jenkins to periodically check the repository for changes and trigger builds.
6. Pull Request (PR) Handling
When integrated with tools like GitHub or Bitbucket, Multibranch Pipelines can detect pull requests.
Each PR can have its own pipeline, allowing Jenkins to run validation builds for incoming changes.
7. SCM Configuration
Jenkins requires configuration to access the repository. Key settings include:
Repository URL: The URL to the repository.
Credentials: Authentication credentials (e.g., SSH key or token).
Behaviors: Options to include/exclude branches or define branch discovery strategies.
8. Pipeline Branches
Once branches are discovered, Jenkins treats each branch as a separate pipeline job.
Branches can have distinct Jenkinsfile configurations or inherit from a parent branch.
9. Folder-like Organization
The Multibranch Pipeline organizes branches into a parent job (the Multibranch Pipeline) with child jobs for each branch or PR.
Example structure:
bash
Copy code
MultibranchPipelineProject/
├── main
├── feature/awesome-feature
├── PR-123
10. Pipeline Status Visualization
Jenkins provides status for each branch pipeline, such as:
Successful or failed builds.
Latest changes or commit IDs.
Build history and logs for each branch.
11. Pipeline Build and Execution
Each branch’s pipeline is independently built based on its Jenkinsfile.
Common steps include:
Fetching the code.
Running build scripts or tests.
Deploying artifacts.
12. Script Approval
If the Jenkinsfile or any libraries require the use of Groovy scripts outside of the sandbox, Jenkins admins must approve the scripts for execution.
13. Multibranch Pipeline Dashboard
The Jenkins UI provides a dashboard where you can:
View all discovered branches.
See the build history of each branch.
Trigger builds manually or configure options.
14. Environment Variables
Multibranch Pipelines expose useful environment variables, such as:
BRANCH_NAME: The name of the current branch.
CHANGE_ID: The pull request ID (if applicable).
CHANGE_AUTHOR: The author of the pull request.
GIT_COMMIT: The current commit hash.
GIT_URL: The repository URL.
15. Webhooks and SCM Polling
Jenkins listens for repository events using webhooks (preferred) or periodically polls for changes.
When a change is detected, the corresponding branch pipeline is triggered.
16. Custom Configuration Options
You can configure behaviors for branch management:
Excluding Branches: Ignore branches that don’t match certain patterns.
Build Strategies: Define whether to build branches on creation, update, or periodically.
Example Multibranch Pipeline Project Configuration:
Set Repository URL:
Example: https://github.com/user/repo.git
Credentials:
Add an SSH key or personal access token for authentication.
Branch Discovery:
Include all branches or apply filters like feature/*.
Benefits of Multibranch Pipeline:
Automates pipeline creation for each branch.
Provides a unified way to handle branches and pull requests.
Eliminates manual configuration of jobs for every new branch or feature.
Integrates well with modern CI/CD workflows.

