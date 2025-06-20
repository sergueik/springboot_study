output "user_data" {
  value = length(data.template_file.data1[*]) > 0 ? data.template_file.data1[0].rendered : data.template_file.data2[0].rendered

}

output "length" {
  value = length(data.template_file.data1[*])
}