terraform {
  required_providers {
    docker = {
      // NOTE: HOSTNAME is omitted, defaults to https://registry.terraform.io
      // the hostname of the public Terraform Registry.
      // https://registry.terraform.io/providers/kreuzwerker/docker/latest/docs
      source  = "kreuzwerker/docker"
      version = "~>3.0.1"
    }
  }
}
locals {
  version = "latest"
}
provider "docker" {
  // for ubuntu: 
  // host = "unix:///var/run/docker.sock"
  // Windows 8.x Docker ToolBox running in Virtual Box
  // use docker-toolbox ip 
  host = "tcp://192.168.99.100:2376"
  // Windows 10 Docker
  // see also: (get-childitem \\.\pipe\).FullName
  // host = "pipe:////.//pipe/"
}
// https://registry.terraform.io/providers/kreuzwerker/docker/latest/docs/resources/image
resource "docker_image" "nginx" {
  name         = "nginx:${local.version}"
  keep_locally = true
}
// https://registry.terraform.io/providers/kreuzwerker/docker/latest/docs/resources/container
resource "docker_container" "nginx_test" {
  image = docker_image.nginx.image_id
  name  = "nginx_test"
  ports {
    internal = 80
    external = 8000
  }

}

