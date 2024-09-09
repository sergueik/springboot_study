# Call the setup module to create a random bucket prefix
run "setup_tests" {
  module {
    source = "./tests/setup"
  }
}

# Apply run block to create the bucket
#  no need to define outputs
run "create_bucket" {
  variables {
    bucket_name = "spheric-alcove-${run.setup_tests.bucket_prefix}"
  }


  # Check that the bucket name is correct
  assert {
    condition     = google_storage_bucket.bucket.name == "spheric-alcove-${run.setup_tests.bucket_prefix}"
    error_message = "Invalid bucket name"
  }
}

run "website_is_running" {
  command = plan

  module {
    source = "./tests/final"
  }

  variables {
    endpoint = run.create_bucket.website_endpoint
  }

  assert {
    condition     = data.http.index.status_code == 200
    error_message = "Website responded with HTTP status ${data.http.index.status_code}"
  }

  assert {
    condition     = can(regex("[a-z]+", data.http.index.response_body ))
    error_message = "Website response body ${data.http.index.response_body} should contain alphanumeric characters"
  }
}
/*

Error: Error in function call

  on tests\website.tftest.hcl line 49, in run "website_is_running":
  49:     condition     = regex("[a-z]+", data.http.index.response_body ) != ""
    ├────────────────
    │ while calling regex(pattern, string)
    │ data.http.index.response_body is "12345\r\n"

Call to function "regex" failed: pattern did not match any part of the given
string.

*/

run "webpage_is_deployed" {
  command = plan

  module {
    source = "./tests/final"
  }

  variables {
    endpoint = run.create_bucket.website_endpoint
  }

  # Check index.html hash matches
  // md5hash - (Computed) Base 64 MD5 hash of the uploaded data.
  // does not wotk
  assert {
    condition     = google_storage_bucket_object.indexpage.md5hash == filemd5("./index.html")
    error_message = "Invalid hash for index.html"
  }

}