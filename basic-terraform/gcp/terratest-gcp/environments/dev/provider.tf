
provider "google" {
  project     = var.project
  region      = var.region
  zone        = var.zone
  credentials = file(var.credentials)

}

provider "google-beta" {
  project     = var.project
  region      = var.region
  zone        = var.zone
  credentials = file(var.credentials)
}


