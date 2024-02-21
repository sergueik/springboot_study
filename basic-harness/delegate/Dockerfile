ARG tag = latest
FROM harness/delegate:${tag}
USER root
ENV key=value

RUN microdnf update && microdnf install unzip yum-utils
RUN mkdir /opt/harness-delegate/tools

# specific version of terraform
#RUN curl -O -L  https://releases.hashicorp.com/terraform/1.3.1/terraform_1.3.1_linux_amd64.zip
#RUN unzip terraform_1.3.1_linux_amd64.zip
#RUN mv ./terraform /usr/bin/

#specific version of helm
RUN curl -OL https://get.helm.sh/helm-v3.12.0-linux-arm64.tar.gz && tar -zxvf helm-v3.12.0-linux-arm64.tar.gz && mv linux-arm64/helm /opt/harness-delegate/tools/helm

# add terraform
RUN yum-config-manager --add-repo https://rpm.releases.hashicorp.com/RHEL/hashicorp.repo \
  && microdnf install -y terraform

# add kubectl
RUN cd /opt/harness-delegate/tools \
  && curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl" && chmod +x kubectl

# add gh cli
RUN yum-config-manager --add-repo https://cli.github.com/packages/rpm/gh-cli.repo \
  && microdnf install -y gh 

ENV PATH=/opt/harness-delegate/tools/:$PATH
RUN terraform --version
