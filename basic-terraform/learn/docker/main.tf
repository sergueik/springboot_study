terraform {
  required_providers {
    docker = {
      // https://registry.terraform.io/providers/kreuzwerker/docker/latest/docs
      source  = "kreuzwerker/docker"
      version = "~>3.0.1"
    }
  }
}
locals {
  version = "3.9.5"
}
provider "docker" {
  // for ubuntu: 
  host = "unix:///var/run/docker.sock"
  // for Windows 8.x Docker ToolBox ??
  //   host = "tcp://192.168.99.100:2376"
  // NOTE: does not work:
  // # 2024-06-08T20:31:51.061-0400 [DEBUG] provider.stdio: received EOF, stopping recv loop: err="rpc error: code = Unavailable desc = error reading from server: EOF"
  // Windows 10 Docker
  // see also: (get-childitem \\.\pipe\).FullName
  // host = "npipe:////.//pipe//docker_engine"
}

resource "docker_image" "alpine" {
  name = "alpine:${local.version}"
}

resource "docker_container" "alpine_test" {
  image = docker_image.alpine.image_id
  name  = "alpine_test"
}

