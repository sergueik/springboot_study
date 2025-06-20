// TODO: required_providers to pessimistically pin version
provider "google" {
  project     = "my-project-id"
  region      = "us-central1"

}

resource "google_notebooks_instance" "instance" {
  name = "notebooks-instance"
  location = "us-west1-a"
  machine_type = "e2-medium"
  vm_image {
    project      = "deeplearning-platform-release"
    image_family = "tf-latest-cpu"
  }
}
