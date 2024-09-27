// https://stackoverflow.com/questions/49392448/possible-to-host-a-website-with-google-cloud-without-a-domain
output "website_endpoint" {
  description = "Website endpoint URL"
  value       = "https://storage.googleapis.com/${google_storage_bucket.static_website.name}/${google_storage_bucket_object.indexpage.name}"
}

// browsing selg_url will show JSON object
output "object_self_link" {
  description = "Website index page self_link"
  value       = google_storage_bucket_object.indexpage.self_link
}

output "page_hash" {
  description = "Website index page md5"
  value       = filemd5("index.html")
}

output "index_hash" {
  description = "Website index page md5"
  value       = google_storage_bucket_object.indexpage.md5hash
}

