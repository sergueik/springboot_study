

output "target-email" {
  value = data.google_client_openid_userinfo.me.email
}
