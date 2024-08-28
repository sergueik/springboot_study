mock "tfplan/v2" {
  module {
    source = "../../mock-tfplan-pass-v2.sentinel"
  }
}

test {
    rules = {
        main = true
    }
}

